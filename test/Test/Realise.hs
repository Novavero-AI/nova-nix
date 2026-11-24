-- | Tests of realisation and of the dependency graph that orders its
-- builds.
module Test.Realise
  ( testRealise,
    testDepGraph,
  )
where

import Control.Exception (evaluate)
import Control.Monad.State.Strict (State, get, gets, modify', runState)
import qualified Data.ByteString as BS
import Data.Either (isLeft, isRight)
import Data.Functor.Identity (Identity (..))
import Data.List (find)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Nix.DependencyGraph as DepGraph
import Nix.Derivation (Derivation (..), DerivationOutput (..), Platform (..))
import qualified Nix.Realise as Realise
import Nix.Store.Path (StoreDir (..), StorePath, defaultStoreDir, storePathToFilePath)
import Nix.Store.Path.Internal (StorePath (..))
import System.Timeout (timeout)
import Test.Harness (TestResult (..), andThen, assertEqual, genJson, pureDepGraph, runTest, runTestM, walkWatchdogMicros)
import qualified Test.QuickCheck as QC

-- ---------------------------------------------------------------------------
-- Tests: Realise
-- ---------------------------------------------------------------------------

-- | What the fake store and caches record, in order.
data RealiseEvent
  = EvQueried StorePath
  | EvQueriedNext StorePath
  | EvFetched [StorePath]
  | EvProbed [StorePath]
  | EvBuilt Text
  | EvChecked Text
  | EvReported Realise.Realised Text
  deriving (Eq, Show)

-- | A fake store and two caches, asked in order.  A cache entry maps a path
-- to its references, a fetch refuses a path whose references are not valid,
-- as the database's unregistered-referent guard does, and a build makes its
-- outputs valid.  The first cache fails to deliver the broken paths it
-- offers, no longer holds the gone ones, and cannot be asked about the
-- unreachable ones at all; the next cache is asked only about a path the
-- first did not deliver, and delivers what it offers.  A fetch that
-- registers nothing records what arrived as probed.  Building a derivation
-- again gives its unstable outputs other contents, and changes no path.
-- Each build is recorded with the outputs the request that led to it
-- asked.
data FakeWorld = FakeWorld
  { fwValid :: !(Set.Set StorePath),
    fwCache :: !(Map.Map StorePath [StorePath]),
    fwNextCache :: !(Map.Map StorePath [StorePath]),
    fwBroken :: !(Set.Set StorePath),
    fwGone :: !(Set.Set StorePath),
    fwUnreachable :: !(Set.Set StorePath),
    fwUnstable :: !(Set.Set StorePath),
    fwEvents :: ![RealiseEvent],
    -- | Each build's event name and the outputs its request asked,
    -- newest first.
    fwBuiltFor :: ![(Text, Set.Set Text)]
  }

-- | Which of the fake's caches offered a path.
data FakeCache = FirstCache | NextCache
  deriving (Eq, Show)

-- | A world with the given entries in the first cache and valid paths, and
-- no faults.
fakeWorld :: [(Text, [Text])] -> [Text] -> FakeWorld
fakeWorld cache valid =
  FakeWorld
    { fwValid = Set.fromList (map fakePath valid),
      fwCache = fakeCache cache,
      fwNextCache = Map.empty,
      fwBroken = Set.empty,
      fwGone = Set.empty,
      fwUnreachable = Set.empty,
      fwUnstable = Set.empty,
      fwEvents = [],
      fwBuiltFor = []
    }

-- | Cache entries by name.
fakeCache :: [(Text, [Text])] -> Map.Map StorePath [StorePath]
fakeCache cache = Map.fromList [(fakePath p, map fakePath refs) | (p, refs) <- cache]

fakeEvent :: RealiseEvent -> State FakeWorld ()
fakeEvent event = modify' (\world -> world {fwEvents = event : fwEvents world})

-- | What the fake cache says when it cannot be asked about a path.
fakeQueryFailure :: StorePath -> Text
fakeQueryFailure sp = "cannot ask about " <> spName sp

fakeSubst :: Realise.SubstEffects FakeCache (State FakeWorld)
fakeSubst =
  Realise.SubstEffects
    { Realise.seValid = \sp -> gets (Set.member sp . fwValid),
      Realise.seReferences = \sp -> do
        fakeEvent (EvQueried sp)
        unreachable <- gets (Set.member sp . fwUnreachable)
        offer <- gets (maybe Realise.NotOffered (Realise.Offered FirstCache) . Map.lookup sp . fwCache)
        pure (if unreachable then Realise.QueryFailed (fakeQueryFailure sp) else offer),
      Realise.seNextOffer = \sp offered -> case offered of
        FirstCache -> do
          fakeEvent (EvQueriedNext sp)
          gets (maybe Realise.NotOffered (Realise.Offered NextCache) . Map.lookup sp . fwNextCache)
        NextCache -> pure Realise.NotOffered,
      Realise.seFetch = \registration batch -> do
        world <- get
        let paths = map fst (NE.toList batch)
            incoming = Set.fromList paths
            landed sp = Set.member sp (fwValid world) || Set.member sp incoming
            declared (sp, FirstCache) = Map.findWithDefault [] sp (fwCache world)
            declared (sp, NextCache) = Map.findWithDefault [] sp (fwNextCache world)
            undelivered (sp, FirstCache)
              | Set.member sp (fwBroken world) = Just Realise.DeliveryFailed
              | Set.member sp (fwGone world) = Just Realise.Gone
            undelivered _ = Nothing
            short = [(sp, why) | each@(sp, _) <- NE.toList batch, Just why <- [undelivered each]]
            refused = registration == Realise.RegisterAll && not (all (all landed . declared) batch)
        case (NE.nonEmpty short, registration) of
          (Just undeliveredPaths, _) -> pure (Realise.NotFetched undeliveredPaths)
          (Nothing, _) | refused -> pure (Realise.NotFetched (fmap (\(sp, _) -> (sp, Realise.DeliveryFailed)) batch))
          (Nothing, Realise.RegisterNone) -> Realise.Fetched <$ fakeEvent (EvProbed paths)
          (Nothing, Realise.RegisterAll) -> do
            fakeEvent (EvFetched paths)
            modify' (\updated -> updated {fwValid = fwValid updated <> incoming})
            pure Realise.Fetched
    }

fakeRealiseEffects :: Set.Set Text -> Realise.RealiseEffects FakeCache (State FakeWorld)
fakeRealiseEffects failing =
  Realise.RealiseEffects
    { Realise.reSubst = fakeSubst,
      Realise.reBuild = \drv _ wanted -> do
        let name = fakeDrvName drv
        if Set.member name failing
          then pure (Left ("building " <> name <> " failed"))
          else do
            fakeEvent (EvBuilt name)
            modify' (\world -> world {fwValid = fwValid world <> Set.fromList (map doPath (drvOutputs drv)), fwBuiltFor = (name, wanted) : fwBuiltFor world})
            pure (Right ()),
      Realise.reCheck = \drv _ -> do
        let name = fakeDrvName drv
        fakeEvent (EvChecked name)
        unstable <- gets fwUnstable
        pure $
          if Set.member name failing
            then Left ("building " <> name <> " failed")
            else Right (maybe Realise.OutputsMatch Realise.OutputDiffers (find (`Set.member` unstable) (map doPath (drvOutputs drv)))),
      Realise.reReport = \status drv -> fakeEvent (EvReported status (fakeDrvName drv))
    }

fakeDrvName :: Derivation -> Text
fakeDrvName drv = case drvOutputs drv of
  (out : _) -> spName (doPath out)
  [] -> ""

-- | A store path with a name-derived hash, so a failure reads as names.
fakePath :: Text -> StorePath
fakePath name = StorePath (T.take 32 (T.replicate 32 (T.take 1 name))) name

-- | A derivation with one output per name, depending on the named nodes
-- for the outputs named with each.
fakeNode :: Text -> [Text] -> [(Text, [Text])] -> (StorePath, DepGraph.DepNode)
fakeNode = fakeNodeWith Map.empty

-- | 'fakeNode' with an environment.
fakeNodeWith :: Map.Map Text BS.ByteString -> Text -> [Text] -> [(Text, [Text])] -> (StorePath, DepGraph.DepNode)
fakeNodeWith env name outputs deps =
  ( drvPath,
    DepGraph.DepNode
      { DepGraph.dnDrvPath = drvPath,
        DepGraph.dnDerivation =
          Derivation
            { drvOutputs = [DerivationOutput out (fakePath out) "" "" | out <- outputs],
              drvInputDrvs = Map.empty,
              drvInputSrcs = [],
              drvPlatform = X86_64_Linux,
              drvBuilder = "/bin/sh",
              drvArgs = [],
              drvEnv = env
            },
        DepGraph.dnDeps = Map.fromList [(fakePath (dep <> ".drv"), Set.fromList wanted) | (dep, wanted) <- deps]
      }
  )
  where
    drvPath = fakePath (name <> ".drv")

-- | Run 'Realise.realise' from a root over a graph and a cache, returning the
-- result, the events in order, and the final valid set.
runRealise ::
  Set.Set Text ->
  [(StorePath, DepGraph.DepNode)] ->
  [(Text, [Text])] ->
  [Text] ->
  Text ->
  (Either Realise.RealiseFailure (), [RealiseEvent], Set.Set StorePath)
runRealise failing nodes cache valid = runRealiseIn Realise.NoFallback (fakeWorld cache valid) failing nodes

-- | 'runRealise' in a given world, under a given fallback.
runRealiseIn ::
  Realise.Fallback ->
  FakeWorld ->
  Set.Set Text ->
  [(StorePath, DepGraph.DepNode)] ->
  Text ->
  (Either Realise.RealiseFailure (), [RealiseEvent], Set.Set StorePath)
runRealiseIn fallback = runRealiseUnder (Realise.RealiseSettings fallback Realise.HeedAllowSubstitutes) Realise.BuildNormal

-- | 'runRealiseIn' checking the root: under 'Realise.BuildCheck'.
runCheckIn ::
  Realise.Fallback ->
  FakeWorld ->
  Set.Set Text ->
  [(StorePath, DepGraph.DepNode)] ->
  Text ->
  (Either Realise.RealiseFailure (), [RealiseEvent], Set.Set StorePath)
runCheckIn fallback = runRealiseUnder (Realise.RealiseSettings fallback Realise.HeedAllowSubstitutes) Realise.BuildCheck

-- | 'runCheckIn' under 'Realise.NoFallback', in a world without faults.
runCheck ::
  Set.Set Text ->
  [(StorePath, DepGraph.DepNode)] ->
  [(Text, [Text])] ->
  [Text] ->
  Text ->
  (Either Realise.RealiseFailure (), [RealiseEvent], Set.Set StorePath)
runCheck failing nodes cache valid = runCheckIn Realise.NoFallback (fakeWorld cache valid) failing nodes

-- | 'runRealise' in a given world, under given settings and build mode.
runRealiseUnder ::
  Realise.RealiseSettings ->
  Realise.BuildMode ->
  FakeWorld ->
  Set.Set Text ->
  [(StorePath, DepGraph.DepNode)] ->
  Text ->
  (Either Realise.RealiseFailure (), [RealiseEvent], Set.Set StorePath)
runRealiseUnder settings mode world failing nodes root =
  let (result, final) = runRealiseWorld settings mode world failing nodes root
   in (result, reverse (fwEvents final), fwValid final)

-- | 'runRealiseUnder', answering the world it ends in.
runRealiseWorld ::
  Realise.RealiseSettings ->
  Realise.BuildMode ->
  FakeWorld ->
  Set.Set Text ->
  [(StorePath, DepGraph.DepNode)] ->
  Text ->
  (Either Realise.RealiseFailure (), FakeWorld)
runRealiseWorld settings mode world failing nodes root =
  runState
    (Realise.realise settings mode (fakeRealiseEffects failing) (DepGraph.DepGraph (Map.fromList nodes)) (fakePath (root <> ".drv")))
    world

-- | Run 'Realise.realiseRequests' under 'Realise.NoFallback' over a graph,
-- each request a node's name and the outputs it asks of it, in a world,
-- returning the result, the events in order, and the world it ends in.
runRequests ::
  FakeWorld ->
  Set.Set Text ->
  [(StorePath, DepGraph.DepNode)] ->
  [(Text, [Text])] ->
  (Either Realise.RequestsFailure (), [RealiseEvent], FakeWorld)
runRequests world failing nodes requests =
  let (result, final) =
        runState
          ( Realise.realiseRequests
              (Realise.RealiseSettings Realise.NoFallback Realise.HeedAllowSubstitutes)
              (fakeRealiseEffects failing)
              (DepGraph.DepGraph (Map.fromList nodes))
              [(fakePath (name <> ".drv"), Set.fromList outputs) | (name, outputs) <- requests]
          )
          world
   in (result, reverse (fwEvents final), final)

-- | Only the fetches, builds and checks of a run, the part that costs
-- anything.
realiseWork :: [RealiseEvent] -> [RealiseEvent]
realiseWork = filter isWork
  where
    isWork (EvFetched _) = True
    isWork (EvBuilt _) = True
    isWork (EvChecked _) = True
    isWork _ = False

-- | A random derivation graph to check.  Each node has one output or two
-- and depends only on lower nodes, for some of each one's outputs, so the
-- graph is acyclic and the highest node is the root.  With it: the nodes
-- that forbid substitutes, the references a cache declares for each
-- output it offers, the outputs that are valid, the ones a cache fails to
-- deliver and the ones it cannot be asked about, the nodes whose builder
-- fails, the outputs of the root that building it again gives other
-- contents, and what a failed substitute leads to.
data CheckCase = CheckCase
  { -- | Each node's dependencies, with the outputs it wants of each.
    ccDeps :: !(Map.Map Int [(Int, [Text])]),
    ccTwoOutputs :: ![Int],
    ccForbidding :: ![Int],
    ccCache :: !(Map.Map Text [Text]),
    ccValid :: ![Text],
    ccBroken :: ![Text],
    ccUnreachable :: ![Text],
    ccFailing :: ![Int],
    ccUnstable :: ![Text],
    ccFallback :: !Realise.Fallback
  }
  deriving (Show)

-- | The names of a node's outputs, given the nodes that have a second.
-- The first names the node in an event, as 'fakeDrvName' reads it.
checkCaseOutputs :: [Int] -> Int -> [Text]
checkCaseOutputs twoOutputs node = (stem <> "-out") : [stem <> "-dev" | node `elem` twoOutputs]
  where
    stem = "n" <> T.pack (show node)

-- | A 'CheckCase'.  A fault is drawn for a third of the cases and absent
-- from the rest, so that the cases reaching a rebuild are not rare; a
-- cache's declared references are an output's siblings and the outputs
-- of lower nodes.
genCheckCase :: QC.Gen CheckCase
genCheckCase = do
  size <- QC.chooseInt (1, 5)
  let nodes = [0 .. size - 1]
      root = size - 1
  twoOutputs <- QC.sublistOf nodes
  let outputsOf = checkCaseOutputs twoOutputs
      allOutputs = concatMap outputsOf nodes
      wanting dep = (,) dep <$> (QC.sublistOf (outputsOf dep) `QC.suchThat` (not . null))
      offered node output =
        QC.frequency
          [ (1, pure Nothing),
            (2, Just . (,) output <$> QC.sublistOf (filter (/= output) (outputsOf node) ++ concatMap outputsOf [0 .. node - 1]))
          ]
      seldom items = QC.frequency [(2, pure []), (1, QC.sublistOf items)]
  deps <- traverse (\node -> (,) node <$> (QC.sublistOf [0 .. node - 1] >>= traverse wanting)) nodes
  cache <- catMaybes <$> sequence [offered node output | node <- nodes, output <- outputsOf node]
  forbidding <- seldom nodes
  valid <- QC.sublistOf allOutputs
  broken <- seldom allOutputs
  unreachable <- seldom allOutputs
  failing <- seldom nodes
  unstable <- QC.sublistOf (outputsOf root)
  fallback <- QC.elements [Realise.NoFallback, Realise.BuildFromSource]
  pure
    CheckCase
      { ccDeps = Map.fromList deps,
        ccTwoOutputs = twoOutputs,
        ccForbidding = forbidding,
        ccCache = Map.fromList cache,
        ccValid = valid,
        ccBroken = broken,
        ccUnreachable = unreachable,
        ccFailing = failing,
        ccUnstable = unstable,
        ccFallback = fallback
      }

-- | What a 'CheckCase' runs in: the world, the names of the outputs whose
-- derivations fail to build, the graph's nodes, and the root's name.
data CheckSetup = CheckSetup
  { csWorld :: !FakeWorld,
    csFailing :: !(Set.Set Text),
    csNodes :: ![(StorePath, DepGraph.DepNode)],
    csRoot :: !Text
  }

-- | The 'CheckSetup' of a 'CheckCase'.  A failing node is named by its
-- first output, as 'fakeDrvName' names it.
checkSetup :: CheckCase -> CheckSetup
checkSetup checkCase =
  CheckSetup
    { csWorld =
        (fakeWorld [] (ccValid checkCase))
          { fwCache = Map.fromList [(fakePath output, map fakePath refs) | (output, refs) <- Map.toList (ccCache checkCase)],
            fwBroken = paths (ccBroken checkCase),
            fwUnreachable = paths (ccUnreachable checkCase),
            fwUnstable = paths (ccUnstable checkCase)
          },
      csFailing = Set.fromList (concatMap (take 1 . outputsOf) (ccFailing checkCase)),
      csNodes = [fakeNodeWith (env node) (name node) (outputsOf node) [(name dep, wanted) | (dep, wanted) <- below] | (node, below) <- Map.toList (ccDeps checkCase)],
      csRoot = name (maximum (0 : Map.keys (ccDeps checkCase)))
    }
  where
    name node = "n" <> T.pack (show node)
    outputsOf = checkCaseOutputs (ccTwoOutputs checkCase)
    paths = Set.fromList . map fakePath
    env node
      | node `elem` ccForbidding checkCase = Map.singleton "allowSubstitutes" ""
      | otherwise = Map.empty

-- | Run a 'CheckCase' from its root under a build mode: the result, the
-- events with the outputs that were valid before each, those valid at the
-- end, and each build's event name with the outputs its request asked, in
-- the order of the builds.
runCheckCase ::
  Realise.BuildMode ->
  CheckCase ->
  (Either Realise.RealiseFailure (), [(Set.Set StorePath, RealiseEvent)], Set.Set StorePath, [(Text, Set.Set Text)])
runCheckCase mode checkCase = (result, steps, fwValid ended, reverse (fwBuiltFor ended))
  where
    setup = checkSetup checkCase
    events = reverse (fwEvents ended)
    steps = zip (scanl landed (fwValid (csWorld setup)) events) events
    nodes = Map.keys (ccDeps checkCase)
    outputsOf = checkCaseOutputs (ccTwoOutputs checkCase)
    paths = Set.fromList . map fakePath
    (result, ended) =
      runRealiseWorld
        (Realise.RealiseSettings (ccFallback checkCase) Realise.HeedAllowSubstitutes)
        mode
        (csWorld setup)
        (csFailing setup)
        (csNodes setup)
        (csRoot setup)
    -- A build makes every output of its derivation valid, and its event
    -- names the first.
    builtOutputs = Map.fromList [(first, paths outputs) | node <- nodes, outputs@(first : _) <- [outputsOf node]]
    landed valid event = case event of
      EvFetched fetched -> valid <> Set.fromList fetched
      EvBuilt built -> valid <> Map.findWithDefault Set.empty built builtOutputs
      _ -> valid

-- | The law of a check, for one 'CheckCase'.  The root is never built in
-- the ordinary way.  It is built again at most once, and only when every
-- output of its own and every output it wants of each input is valid.  The
-- check's result is that rebuild's, and without a rebuild a failure.  A
-- root that cannot be checked builds nothing at all.  No output of the
-- root that was valid is fetched, and a root whose outputs were all valid
-- is not asked about.  A root that forbids substitutes and is short of an
-- output cannot be checked, and nothing at all is done for it.  A build
-- that is no check builds nothing again.
--
-- Each class the generator has to reach for those clauses to say anything
-- is covered: the property fails when one is rarer than its share.  The
-- shares are about a third of what 'genCheckCase' gives, measured over
-- 20000 cases (47%, 6%, 14% and 11% in this order).
checkLaw :: CheckCase -> QC.Property
checkLaw checkCase =
  QC.counterexample (show (result, events))
    . QC.cover 15 checked "the root was built again"
    . QC.cover 2 (checked && partlyValid) "a root with one output of two valid was built again"
    . QC.cover 5 (queryFailed result) "a cache could not be asked"
    . QC.cover 3 mustBuildShort "a root that forbids substitutes was short of an output"
    $ QC.conjoin
      [ QC.counterexample "the root was built" (EvBuilt rootName `notElem` events),
        QC.counterexample "another derivation was checked, or the root twice" ([name | EvChecked name <- events] `elem` [[], [rootName]]),
        QC.counterexample "checked before its outputs and inputs were valid" (all (needed `Set.isSubsetOf`) checks),
        QC.counterexample "the result is not the rebuild's" resultIsRebuilds,
        QC.counterexample "built something for a root it could not check" (not (unbuildable result) || null [() | EvBuilt _ <- events]),
        QC.counterexample "fetched an output of the root that was valid" (Set.null (Set.fromList (concat [fetched | EvFetched fetched <- events]) `Set.intersection` validAtStart)),
        QC.counterexample "asked a cache about a root whose outputs were valid" (not allValidAtStart || null [() | EvQueried asked <- events, asked `Set.member` rootOutputs]),
        QC.counterexample "did something for a root that forbids substitutes and lacks an output" (not mustBuildShort || (result, events) == (Left (Realise.OutputsNotValid rootDrv), [])),
        QC.counterexample "an ordinary build checked" (null [() | (_, EvChecked _) <- ordinary])
      ]
  where
    root = maximum (0 : Map.keys (ccDeps checkCase))
    outputsOf = checkCaseOutputs (ccTwoOutputs checkCase)
    rootDrv = fakePath ("n" <> T.pack (show root) <> ".drv")
    rootName = T.concat (take 1 (outputsOf root))
    rootOutputs = Set.fromList (map fakePath (outputsOf root))
    needed = Set.fromList (map fakePath (outputsOf root ++ concatMap snd (Map.findWithDefault [] root (ccDeps checkCase))))
    validAtStart = rootOutputs `Set.intersection` Set.fromList (map fakePath (ccValid checkCase))
    allValidAtStart = validAtStart == rootOutputs
    partlyValid = not (Set.null validAtStart) && not allValidAtStart
    mustBuildShort = root `elem` ccForbidding checkCase && not allValidAtStart
    (result, steps, _, _) = runCheckCase Realise.BuildCheck checkCase
    events = map snd steps
    checks = [valid | (valid, EvChecked _) <- steps]
    checked = not (null checks)
    rebuilt
      | root `elem` ccFailing checkCase = Left (Realise.BuildFailed ("building " <> rootName <> " failed"))
      | otherwise = maybe (Right ()) (Left . Realise.NotDeterministic rootDrv . fakePath) (find (`elem` ccUnstable checkCase) (outputsOf root))
    -- Without a rebuild the check can only have failed.
    resultIsRebuilds
      | checked = result == rebuilt
      | otherwise = either (const True) (const False) result
    unbuildable (Left (Realise.OutputsNotValid _)) = True
    unbuildable _ = False
    queryFailed (Left (Realise.CacheQueryFailed _)) = True
    queryFailed _ = False
    (_, ordinary, _, _) = runCheckCase Realise.BuildNormal checkCase

-- | The law of a build, for one 'CheckCase'.  A derivation builds only
-- while some output the request that led to the build asks of it is not
-- valid, and only once every output it wants of each of its inputs is
-- valid, so an input asked again for an output it has not realised is
-- realised again for it.  What is fetched is an output some request asks
-- for, or something such an output references, as the cache declares.  A
-- build that succeeds leaves every output of the root valid.
--
-- An input asked for one output of its two is what the first and third
-- clauses need to say anything.  An input that two dependents ask for
-- different outputs is what the second needs, and what the first needs to
-- tell the request that led to a build from the others made of the same
-- derivation.  Each is held to about a third of the share 'genCheckCase'
-- gives it, measured over 20000 cases (about 39% and 15% in this order).
realiseLaw :: CheckCase -> QC.Property
realiseLaw checkCase =
  QC.counterexample (show (result, steps, builtFor))
    . QC.cover 10 partlyWanted "an input was asked for one output of its two"
    . QC.cover 5 askedDifferently "an input was asked for different outputs by two dependents"
    $ QC.conjoin
      [ QC.counterexample "a build was not recorded with its request" (map fst builtFor == map snd buildSteps),
        QC.counterexample "built a derivation whose outputs the request asked were valid" (and [not (Set.map fakePath wanted `Set.isSubsetOf` valid) | ((valid, _), (_, wanted)) <- zip buildSteps builtFor]),
        QC.counterexample "built before the outputs it wants of its inputs were valid" (all inputsValid buildSteps),
        QC.counterexample "fetched what no request reaches" (all (all (`Set.member` reachable)) [fetched | (_, EvFetched fetched) <- steps]),
        QC.counterexample "succeeded short of an output of the root" (either (const True) (const (paths (outputsOf root) `Set.isSubsetOf` final)) result)
      ]
  where
    nodes = Map.keys (ccDeps checkCase)
    root = maximum (0 : nodes)
    outputsOf = checkCaseOutputs (ccTwoOutputs checkCase)
    paths = Set.fromList . map fakePath
    (result, steps, final, builtFor) = runCheckCase Realise.BuildNormal checkCase
    -- Each build, with the outputs valid before it.
    buildSteps = [(valid, built) | (valid, EvBuilt built) <- steps]
    edges = [(dep, wanted) | below <- Map.elems (ccDeps checkCase), (dep, wanted) <- below]
    -- The root is asked for every output, an input for what each of its
    -- dependents names.
    asked node
      | node == root = outputsOf root
      | otherwise = concat [wanted | (dep, wanted) <- edges, dep == node]
    -- A build's event names the node's first output.
    nodeBuilt = Map.fromList [(first, node) | node <- nodes, first : _ <- [outputsOf node]]
    wantedOfInputs node = paths (concatMap snd (Map.findWithDefault [] node (ccDeps checkCase)))
    inputsValid (valid, built) = maybe False ((`Set.isSubsetOf` valid) . wantedOfInputs) (Map.lookup built nodeBuilt)
    reachable = Set.map fakePath (closeUnder Set.empty (concatMap asked nodes))
    closeUnder seen pending = case pending of
      [] -> seen
      output : rest
        | Set.member output seen -> closeUnder seen rest
        | otherwise -> closeUnder (Set.insert output seen) (Map.findWithDefault [] output (ccCache checkCase) ++ rest)
    partlyWanted = or [length wanted < length (outputsOf dep) | (dep, wanted) <- edges]
    askedDifferently = or [Set.size (Set.fromList [Set.fromList wanted | (dep, wanted) <- edges, dep == node]) > 1 | node <- nodes]

-- | The law of a lone request, for one 'CheckCase': a request for every
-- output of the root realises the graph as 'Realise.realise' does under
-- 'Realise.BuildNormal', to the same result, through the same events and
-- builds, and to the same valid paths.  Cases that fail and cases that
-- succeed are each held to a share well under what 'genCheckCase' gives
-- them.
loneRequestLaw :: CheckCase -> QC.Property
loneRequestLaw checkCase =
  QC.cover 10 (isLeft requested) "the request failed"
    . QC.cover 10 (isRight requested) "the request was realised"
    $ alone QC.=== observed (either (Left . Realise.FailedWith) Right <$> Realise.realise settings Realise.BuildNormal effects graph rootDrv)
  where
    setup = checkSetup checkCase
    settings = Realise.RealiseSettings (ccFallback checkCase) Realise.HeedAllowSubstitutes
    effects = fakeRealiseEffects (csFailing setup)
    graph = DepGraph.DepGraph (Map.fromList (csNodes setup))
    rootDrv = fakePath (csRoot setup <> ".drv")
    everyOutput = Set.fromList (checkCaseOutputs (ccTwoOutputs checkCase) (maximum (0 : Map.keys (ccDeps checkCase))))
    alone@(requested, _, _, _) = observed (Realise.realiseRequests settings effects graph [(rootDrv, everyOutput)])
    observed action =
      let (result, final) = runState action (csWorld setup)
       in (result, reverse (fwEvents final), fwValid final, fwBuiltFor final)

-- | A random reference graph: each node references only lower nodes, so the
-- graph is acyclic, and some nodes are absent from the cache.
genClosureCase :: QC.Gen (Map.Map Int [Int], [Int])
genClosureCase = do
  size <- QC.chooseInt (1, 12)
  references <- traverse (\node -> (,) node <$> QC.sublistOf [0 .. node - 1]) [0 .. size - 1]
  absent <- QC.sublistOf [0 .. size - 1]
  pure (Map.fromList references, absent)

testRealise :: IO [Bool]
testRealise = do
  putStrLn "realise"
  sequence
    [ -- hello is on the cache, so nothing it was built from is touched, only
      -- what it references at runtime.
      runTest "a root on the cache fetches its runtime closure and nothing else" $
        let (result, events, _) =
              runRealise
                Set.empty
                [fakeNode "hello" ["hello-out"] [("gcc", ["gcc-out"])], fakeNode "gcc" ["gcc-out"] []]
                [("hello-out", ["glibc-out"]), ("glibc-out", []), ("gcc-out", [])]
                []
                "hello"
         in assertEqual
              "realise"
              (Right (), [EvFetched [fakePath "glibc-out"], EvFetched [fakePath "hello-out"]], False)
              (result, realiseWork events, EvQueried (fakePath "gcc-out") `elem` events),
      runTest "a root the cache lacks realises its inputs, then builds" $
        let (result, events, _) =
              runRealise
                Set.empty
                [fakeNode "hello" ["hello-out"] [("gcc", ["gcc-out"])], fakeNode "gcc" ["gcc-out"] []]
                [("gcc-out", [])]
                []
                "hello"
         in assertEqual "realise" (Right (), [EvFetched [fakePath "gcc-out"], EvBuilt "hello-out"]) (result, realiseWork events),
      runTest "a reference the cache lacks fails the substitution and builds" $
        let (result, events, valid) =
              runRealise
                Set.empty
                [fakeNode "hello" ["hello-out"] []]
                [("hello-out", ["gone-out"])]
                []
                "hello"
         in assertEqual
              "realise"
              (Right (), [EvBuilt "hello-out"], False)
              (result, realiseWork events, Set.member (fakePath "gone-out") valid),
      -- Upstream's retrySubstitution: the closure's hole is an input built
      -- here, and then the substitute is good, so hello is not built.
      runTest "an input that fills the closure's hole lets the substitute through" $
        let (result, events, _) =
              runRealise
                Set.empty
                [fakeNode "hello" ["hello-out"] [("lib", ["lib-out"])], fakeNode "lib" ["lib-out"] []]
                [("hello-out", ["lib-out"])]
                []
                "hello"
         in assertEqual "realise" (Right (), [EvBuilt "lib-out", EvFetched [fakePath "hello-out"]]) (result, realiseWork events),
      runTest "a root with a missing output is not retried when no reference was at fault" $
        let (_, events, _) =
              runRealise Set.empty [fakeNode "hello" ["hello-out"] []] [] [] "hello"
         in assertEqual "narinfo queries" (1 :: Int) (length [() | EvQueried _ <- events]),
      runTest "outputs arrive together, and one referencing another is no hole" $
        let (result, events, _) =
              runRealise
                Set.empty
                [fakeNode "pkg" ["pkg-bin", "pkg-lib"] [("gcc", ["gcc-out"])], fakeNode "gcc" ["gcc-out"] []]
                [("pkg-bin", ["pkg-lib"]), ("pkg-lib", [])]
                []
                "pkg"
         in assertEqual "realise" (Right (), [EvFetched [fakePath "pkg-bin", fakePath "pkg-lib"]]) (result, realiseWork events),
      runTest "one missing output of two builds rather than half substituting" $
        let (result, events, valid) =
              runRealise
                Set.empty
                [fakeNode "pkg" ["pkg-bin", "pkg-lib"] []]
                [("pkg-bin", [])]
                []
                "pkg"
         in assertEqual
              "realise"
              (Right (), [EvBuilt "pkg-bin"], True)
              (result, realiseWork events, Set.member (fakePath "pkg-lib") valid),
      runTest "a self-reference is no hole" $
        let (result, events, _) =
              runRealise Set.empty [fakeNode "hello" ["hello-out"] []] [("hello-out", ["hello-out"])] [] "hello"
         in assertEqual "realise" (Right (), [EvFetched [fakePath "hello-out"]]) (result, realiseWork events),
      -- The cycle ends at the visited set, and the registration it reaches
      -- refuses the unregistered referent: a failed substitute.
      runTest "a reference cycle a cache declares ends, and fails the substitution" $
        let (result, events, _) =
              runRealise
                Set.empty
                [fakeNode "hello" ["hello-out"] []]
                [("hello-out", ["a-out"]), ("a-out", ["b-out"]), ("b-out", ["a-out"])]
                []
                "hello"
         in assertEqual "realise" (Left (Realise.SubstitutesFailed (fakePath "hello.drv")), []) (result, realiseWork events),
      runTest "under fallback a reference cycle a cache declares builds" $
        let (result, events, _) =
              runRealiseIn
                Realise.BuildFromSource
                (fakeWorld [("hello-out", ["a-out"]), ("a-out", ["b-out"]), ("b-out", ["a-out"])] [])
                Set.empty
                [fakeNode "hello" ["hello-out"] []]
                "hello"
         in assertEqual "realise" (Right (), [EvBuilt "hello-out"]) (result, realiseWork events),
      -- Upstream's DerivationGoal::outputsSubstitutionTried: a substitute
      -- that fails is not a substitute the cache lacks.
      runTest "a substitute that fails stops the build, naming the derivation" $
        let (result, events, valid) =
              runRealiseIn
                Realise.NoFallback
                (fakeWorld [("hello-out", [])] []) {fwBroken = Set.singleton (fakePath "hello-out")}
                Set.empty
                [fakeNode "hello" ["hello-out"] [("gcc", ["gcc-out"])], fakeNode "gcc" ["gcc-out"] []]
                "hello"
         in assertEqual
              "realise"
              (Left (Realise.SubstitutesFailed (fakePath "hello.drv")), [], False, False)
              (result, realiseWork events, EvQueried (fakePath "gcc-out") `elem` events, Set.member (fakePath "hello-out") valid),
      runTest "under fallback a substitute that fails builds instead, and is not retried" $
        let (result, events, _) =
              runRealiseIn
                Realise.BuildFromSource
                (fakeWorld [("hello-out", []), ("gcc-out", [])] []) {fwBroken = Set.singleton (fakePath "hello-out")}
                Set.empty
                [fakeNode "hello" ["hello-out"] [("gcc", ["gcc-out"])], fakeNode "gcc" ["gcc-out"] []]
                "hello"
         in assertEqual
              "realise"
              (Right (), [EvFetched [fakePath "gcc-out"], EvBuilt "hello-out"], 1 :: Int)
              (result, realiseWork events, length [() | EvQueried sp <- events, sp == fakePath "hello-out"]),
      runTest "a reference whose substitute fails stops the build of what names it" $
        let (result, events, _) =
              runRealiseIn
                Realise.NoFallback
                (fakeWorld [("hello-out", ["lib-out"]), ("lib-out", [])] []) {fwBroken = Set.singleton (fakePath "lib-out")}
                Set.empty
                [fakeNode "hello" ["hello-out"] []]
                "hello"
         in assertEqual "realise" (Left (Realise.SubstitutesFailed (fakePath "hello.drv")), []) (result, realiseWork events),
      -- Upstream's goal moves on to its next substituter when a copy fails,
      -- and realises what that substituter's path info references first.
      runTest "a path the first cache fails to deliver comes from the next, after what that cache says it references" $
        let (result, events, _) =
              runRealiseIn
                Realise.NoFallback
                (fakeWorld [("hello-out", []), ("lib-out", [])] [])
                  { fwBroken = Set.singleton (fakePath "hello-out"),
                    fwNextCache = fakeCache [("hello-out", ["lib-out"])]
                  }
                Set.empty
                [fakeNode "hello" ["hello-out"] []]
                "hello"
         in assertEqual
              "realise"
              ( Right (),
                [ EvQueried (fakePath "hello-out"),
                  EvQueriedNext (fakePath "hello-out"),
                  EvQueried (fakePath "lib-out"),
                  EvFetched [fakePath "lib-out"],
                  EvFetched [fakePath "hello-out"],
                  EvReported Realise.Substituted "hello-out"
                ]
              )
              (result, events),
      runTest "only the output a cache failed to deliver is asked of the next cache" $
        let (result, events, _) =
              runRealiseIn
                Realise.NoFallback
                (fakeWorld [("pkg-bin", []), ("pkg-lib", [])] [])
                  { fwBroken = Set.singleton (fakePath "pkg-bin"),
                    fwNextCache = fakeCache [("pkg-bin", []), ("pkg-lib", [])]
                  }
                Set.empty
                [fakeNode "pkg" ["pkg-bin", "pkg-lib"] []]
                "pkg"
         in assertEqual
              "realise"
              (Right (), [fakePath "pkg-bin"], [EvFetched [fakePath "pkg-bin", fakePath "pkg-lib"]])
              (result, [sp | EvQueriedNext sp <- events], realiseWork events),
      -- Upstream's goal for each output runs its own course: dev's ends for
      -- want of a substitute and out's arrives from the next cache, so no
      -- goal failed and the derivation builds (outputsSubstitutionTried in
      -- derivation-goal.cc at 2.24.9, haveDerivation at 2.36.0).
      runTest "an output whose cache failed and that the next cache delivers fails nothing, though its sibling has no cache left" $
        let (result, events, _) =
              runRealiseIn
                Realise.NoFallback
                (fakeWorld [("pkg-out", []), ("pkg-dev", [])] [])
                  { fwBroken = Set.singleton (fakePath "pkg-out"),
                    fwGone = Set.singleton (fakePath "pkg-dev"),
                    fwNextCache = fakeCache [("pkg-out", [])]
                  }
                Set.empty
                [fakeNode "pkg" ["pkg-out", "pkg-dev"] []]
                "pkg"
         in assertEqual
              "realise"
              (Right (), [EvBuilt "pkg-out"], [[fakePath "pkg-out"]])
              (result, realiseWork events, [probed | EvProbed probed <- events]),
      runTest "an output whose every cache failed to deliver it stops the build, though its sibling is gone" $
        let (result, events, _) =
              runRealiseIn
                Realise.NoFallback
                (fakeWorld [("pkg-out", []), ("pkg-dev", [])] [])
                  { fwBroken = Set.singleton (fakePath "pkg-out"),
                    fwGone = Set.singleton (fakePath "pkg-dev")
                  }
                Set.empty
                [fakeNode "pkg" ["pkg-out", "pkg-dev"] []]
                "pkg"
         in assertEqual "realise" (Left (Realise.SubstitutesFailed (fakePath "pkg.drv")), []) (result, realiseWork events),
      -- Upstream rethrows a failed narinfo query, which ends the build
      -- before any other goal is asked anything.
      runTest "a cache that cannot be asked stops the build, and nothing after it is asked" $
        let (result, events, _) =
              runRealiseIn
                Realise.NoFallback
                (fakeWorld [("pkg-lib", [])] []) {fwUnreachable = Set.singleton (fakePath "pkg-bin")}
                Set.empty
                [fakeNode "pkg" ["pkg-bin", "pkg-lib"] [("gcc", ["gcc-out"])], fakeNode "gcc" ["gcc-out"] []]
                "pkg"
         in assertEqual
              "realise"
              (Left (Realise.CacheQueryFailed (fakeQueryFailure (fakePath "pkg-bin"))), [EvQueried (fakePath "pkg-bin")])
              (result, events),
      runTest "a substitution is retried once after the inputs, and not again" $
        let (_, events, _) =
              runRealise Set.empty [fakeNode "hello" ["hello-out"] []] [("hello-out", ["gone-out"])] [] "hello"
         in assertEqual "narinfo queries" (2 :: Int) (length [() | EvQueried sp <- events, sp == fakePath "hello-out"]),
      runTest "a reference that landed before another failed stays valid" $
        let (result, events, valid) =
              runRealise
                Set.empty
                [fakeNode "hello" ["hello-out"] []]
                [("hello-out", ["a-out", "z-out"]), ("a-out", [])]
                []
                "hello"
         in assertEqual
              "realise"
              (Right (), [EvFetched [fakePath "a-out"], EvBuilt "hello-out"], True)
              (result, realiseWork events, Set.member (fakePath "a-out") valid),
      runTestM "a closure lands exactly when all of it is cached, and stays closed" $ do
        result <-
          QC.quickCheckWithResult QC.stdArgs {QC.chatty = False} $
            QC.forAll genClosureCase $ \(references, absent) ->
              let paths = Map.keys references
                  root = maximum (0 : paths)
                  closure seen node
                    | Set.member node seen = seen
                    | otherwise = foldl closure (Set.insert node seen) (Map.findWithDefault [] node references)
                  path = fakePath . ("p" <>) . T.pack . show
                  cache = Map.fromList [(path node, map path refs) | (node, refs) <- Map.toList references, node `notElem` absent]
                  (outcome, final) =
                    runState (Realise.substituteClosure fakeSubst [path root]) (fakeWorld [] []) {fwCache = cache}
                  valid = fwValid final
                  closed = all (\sp -> all (`Set.member` valid) (Map.findWithDefault [] sp cache)) valid
                  wholeOnCache = all (`notElem` absent) (closure Set.empty root)
               in (outcome == Realise.ClosureComplete, closed) QC.=== (wholeOnCache, True)
        pure (if QC.isSuccess result then Pass else Fail (T.pack (QC.output result))),
      runTest "valid outputs need no cache at all" $
        let (result, events, _) =
              runRealise Set.empty [fakeNode "hello" ["hello-out"] [("gcc", ["gcc-out"])], fakeNode "gcc" ["gcc-out"] []] [] ["hello-out"] "hello"
         in assertEqual "realise" (Right (), [EvReported Realise.AlreadyValid "hello-out"]) (result, events),
      runTest "a dependency two others share is realised once" $
        let (result, events, _) =
              runRealise
                Set.empty
                [ fakeNode "top" ["top-out"] [("left", ["left-out"]), ("right", ["right-out"])],
                  fakeNode "left" ["left-out"] [("base", ["base-out"])],
                  fakeNode "right" ["right-out"] [("base", ["base-out"])],
                  fakeNode "base" ["base-out"] []
                ]
                []
                []
                "top"
         in assertEqual
              "realise"
              (Right (), [EvBuilt "base-out", EvBuilt "left-out", EvBuilt "right-out", EvBuilt "top-out"])
              (result, realiseWork events),
      -- An input is realised for the outputs its dependent names, which
      -- alone are checked and substituted (checkPathValidity and
      -- haveDerivation, derivation-goal.cc at 2.24.9).
      runTest "a dependency on one output fetches that output and its closure only" $
        let (result, events, valid) =
              runRealise
                Set.empty
                [fakeNode "app" ["app-out"] [("lib", ["lib-dev"])], threeOutputLib]
                [("lib-dev", ["lib-out"]), ("lib-out", []), ("lib-doc", [])]
                []
                "app"
         in assertEqual
              "realise"
              (Right (), [EvFetched [fakePath "lib-out"], EvFetched [fakePath "lib-dev"], EvBuilt "app-out"], False)
              (result, realiseWork events, Set.member (fakePath "lib-doc") valid),
      runTest "an output nothing wants and no cache holds builds nothing" $
        let (result, events, _) =
              runRealise
                Set.empty
                [fakeNode "app" ["app-out"] [("lib", ["lib-dev"])], threeOutputLib]
                [("lib-dev", ["lib-out"]), ("lib-out", [])]
                []
                "app"
         in assertEqual
              "realise"
              (Right (), [EvFetched [fakePath "lib-out"], EvFetched [fakePath "lib-dev"], EvBuilt "app-out"])
              (result, realiseWork events),
      -- lib.drv sorts before mid.drv, so app asks lib for lib-dev, and mid
      -- asks it for lib-out after that request is done.
      runTest "a later request for another output substitutes it" $
        let (result, events, _) = runRealise Set.empty laterRequest [("lib-dev", []), ("lib-out", [])] [] "app"
         in assertEqual
              "realise"
              (Right (), [EvFetched [fakePath "lib-dev"], EvFetched [fakePath "lib-out"], EvBuilt "mid-out", EvBuilt "app-out"])
              (result, realiseWork events),
      runTest "a later request for another output no cache holds builds the derivation" $
        let (result, events, _) = runRealise Set.empty laterRequest [("lib-dev", [])] [] "app"
         in assertEqual
              "realise"
              (Right (), [EvFetched [fakePath "lib-dev"], EvBuilt "lib-out", EvBuilt "mid-out", EvBuilt "app-out"])
              (result, realiseWork events),
      -- checkPathValidity refuses an output name the derivation does not
      -- have (derivation-goal.cc at 2.24.9), before anything is
      -- substituted for it.
      runTest "an output a derivation does not have fails the request, naming each in order" $
        let (result, events, _) =
              runRealise
                Set.empty
                [fakeNode "app" ["app-out"] [("lib", ["lib-out", "lib-man", "lib-bin"])], fakeNode "lib" ["lib-out"] []]
                [("lib-out", [])]
                []
                "app"
         in assertEqual
              "realise"
              (Left ("derivation '/nix/store/" <> T.replicate 32 "l" <> "-lib.drv' does not have wanted outputs 'lib-bin', 'lib-man'"), [])
              (messageOf result, realiseWork events),
      -- Upstream starts no goal for an input that names no output
      -- (DerivationGoal::gaveUpOnSubstitution in derivation-goal.cc at
      -- 2.24.9, DerivationResolutionGoal::realiseInputs in
      -- derivation-resolution-goal.cc at 2.36.0).
      runTest "an input asked for no output is not realised" $
        let (result, events, _) =
              runRealise Set.empty [fakeNode "app" ["app-out"] [("lib", [])], fakeNode "lib" ["lib-out"] []] [] [] "app"
         in assertEqual
              "realise"
              (Right (), [EvReported Realise.Built "app-out"])
              (result, [event | event@(EvReported _ _) <- events]),
      runTest "a later request for outputs already realised realises nothing again" $
        let (result, events, _) =
              runRealise
                Set.empty
                [ fakeNode "app" ["app-out"] [("lib", ["lib-dev"]), ("mid", ["mid-out"])],
                  fakeNode "mid" ["mid-out"] [("lib", ["lib-dev"])],
                  fakeNode "lib" ["lib-out", "lib-dev"] []
                ]
                [("lib-dev", [])]
                []
                "app"
         in assertEqual
              "realise"
              (Right (), [EvReported Realise.Substituted "lib-out", EvReported Realise.Built "mid-out", EvReported Realise.Built "app-out"])
              (result, [event | event@(EvReported _ _) <- events]),
      runTest "a failed input stops the build of what depends on it" $
        let (result, events, _) =
              runRealise
                (Set.singleton "gcc-out")
                [fakeNode "hello" ["hello-out"] [("gcc", ["gcc-out"])], fakeNode "gcc" ["gcc-out"] []]
                []
                []
                "hello"
         in assertEqual "realise" (Left (Realise.BuildFailed "building gcc-out failed"), []) (result, realiseWork events),
      -- Upstream's ParsedDerivation::substitutesAllowed, read where
      -- DerivationGoal::haveDerivation reads it: before any cache is asked
      -- about the derivation's own outputs.
      runTest "a derivation that forbids substitutes builds though a cache holds its output, and no cache is asked" $
        let (result, events, _) =
              runRealise Set.empty [fakeNodeWith forbidding "hello" ["hello-out"] []] [("hello-out", [])] [] "hello"
         in assertEqual
              "realise"
              (Right (), [EvBuilt "hello-out"], [])
              (result, realiseWork events, [path | EvQueried path <- events]),
      runTest "its inputs are substituted as any other derivation's are" $
        let (result, events, _) =
              runRealise
                Set.empty
                [fakeNodeWith forbidding "hello" ["hello-out"] [("gcc", ["gcc-out"])], fakeNode "gcc" ["gcc-out"] []]
                [("hello-out", []), ("gcc-out", [])]
                []
                "hello"
         in assertEqual "realise" (Right (), [EvFetched [fakePath "gcc-out"], EvBuilt "hello-out"]) (result, realiseWork events),
      -- Upstream's PathSubstitutionGoal knows paths, not the derivations
      -- that built them.
      runTest "its output is substituted as a path another substituted output references" $
        let (result, events, _) =
              runRealise
                Set.empty
                [fakeNode "app" ["app-out"] [("lib", ["lib-out"])], fakeNodeWith forbidding "lib" ["lib-out"] []]
                [("app-out", ["lib-out"]), ("lib-out", [])]
                []
                "app"
         in assertEqual "realise" (Right (), [EvFetched [fakePath "lib-out"], EvFetched [fakePath "app-out"]]) (result, realiseWork events),
      runTest "under always-allow-substitutes it is substituted" $
        let (result, events, _) =
              runRealiseUnder alwaysAllowing Realise.BuildNormal (fakeWorld [("hello-out", [])] []) Set.empty [fakeNodeWith forbidding "hello" ["hello-out"] []] "hello"
         in assertEqual "realise" (Right (), [EvFetched [fakePath "hello-out"]]) (result, realiseWork events),
      runTest "a structured allowSubstitutes that is no Boolean stops the build, naming the derivation as upstream does" $
        let (result, events, _) =
              runRealise Set.empty [fakeNodeWith mistyped "hello" ["hello-out"] []] [("hello-out", [])] [] "hello"
         in assertEqual
              "realise"
              (Left ("attribute 'allowSubstitutes' of derivation '" <> helloDrvName <> "' must be a Boolean"), [])
              (either (Left . Realise.realiseFailureMessage defaultStoreDir) Right result, realiseWork events),
      runTest "with several outputs it builds them all and asks no cache about any" $
        let (result, events, valid) =
              runRealise
                Set.empty
                [fakeNodeWith forbidding "pkg" ["pkg-bin", "pkg-lib"] []]
                [("pkg-bin", ["pkg-lib"]), ("pkg-lib", [])]
                []
                "pkg"
         in assertEqual
              "realise"
              (Right (), [EvBuilt "pkg-bin"], [], True)
              (result, realiseWork events, [path | EvQueried path <- events], Set.member (fakePath "pkg-lib") valid),
      runTest "it is not read for a derivation whose outputs are valid, nor under always-allow-substitutes" $
        let nodes = [fakeNodeWith mistyped "hello" ["hello-out"] []]
            (valid, _, _) = runRealise Set.empty nodes [] ["hello-out"] "hello"
            (always, _, _) = runRealiseUnder alwaysAllowing Realise.BuildNormal (fakeWorld [("hello-out", [])] []) Set.empty nodes "hello"
         in assertEqual "valid" (Right ()) valid `andThen` assertEqual "always" (Right ()) always,
      runTest "allowSubstitutes is read as upstream's getBoolAttr reads it" $
        let cases =
              [ ("absent", Map.empty, Realise.MaySubstitute),
                ("true as evaluation writes it", Map.singleton "allowSubstitutes" "1", Realise.MaySubstitute),
                ("false as evaluation writes it", forbidding, Realise.MustBuild),
                ("another string", Map.singleton "allowSubstitutes" "true", Realise.MustBuild),
                ("structured, absent", structured "{}", Realise.MaySubstitute),
                ("structured true", structured "{\"allowSubstitutes\":true}", Realise.MaySubstitute),
                ("structured false", structured "{\"allowSubstitutes\":false}", Realise.MustBuild),
                ("structured null", structured "null", Realise.MaySubstitute),
                ("structured, no object", structured "[false]", Realise.MaySubstitute),
                -- Structured attributes are read from the JSON alone.
                ("structured beside an environment entry", Map.union forbidding (structured "{}"), Realise.MaySubstitute)
              ] ::
                [(Text, Map.Map Text BS.ByteString, Realise.Substitutes)]
         in assertEqual
              "substitutes"
              [(label, Right allowed) | (label, _, allowed) <- cases]
              [(label, heeding env) | (label, env, _) <- cases],
      runTest "structured attributes that are not JSON are an error naming the derivation" $
        assertEqual "not-json" (Left True) (either (Left . T.isPrefixOf unreadable) Right (heeding (structured "{")))
          `andThen` assertEqual "not-utf8" (Left (unreadable <> "invalid UTF-8")) (heeding (structured "\xff")),
      -- Upstream's ParsedDerivation constructor runs for every derivation
      -- it has a goal for, before the outputs are looked at and whatever
      -- always-allow-substitutes says.
      runTest "they stop the build though the outputs are valid, and under always-allow-substitutes" $
        let nodes = [fakeNodeWith (structured "\xff") "hello" ["hello-out"] []]
            stopped (result, events, _) = (either (Left . Realise.realiseFailureMessage defaultStoreDir) Right result, events)
            refused = (Left (unreadable <> "invalid UTF-8"), [])
         in assertEqual "valid" refused (stopped (runRealise Set.empty nodes [] ["hello-out"] "hello"))
              `andThen` assertEqual
                "always"
                refused
                (stopped (runRealiseUnder alwaysAllowing Realise.BuildNormal (fakeWorld [("hello-out", [])] []) Set.empty nodes "hello")),
      runTestM "in the environment only 1 allows substitutes" $
        property $
          QC.forAll (QC.oneof [pure "1", BS.pack <$> QC.listOf QC.arbitrary]) $ \value ->
            heeding (Map.singleton "allowSubstitutes" value)
              QC.=== Right (if value == "1" then Realise.MaySubstitute else Realise.MustBuild),
      runTestM "always-allow-substitutes allows a derivation whatever its attributes hold" $
        property $
          QC.forAll genAttributes $ \attrs ->
            Realise.substitutesAllowed Realise.AlwaysAllowSubstitutes helloDrv attrs QC.=== Right Realise.MaySubstitute,
      -- Upstream's bmCheck.  A valid output's substitution goal ends at
      -- once (PathSubstitutionGoal::init), so the cache that holds hello
      -- is never asked about it, and gaveUpOnSubstitution gives the input
      -- an ordinary goal.
      runTest "a check realises the inputs of a valid root, builds the root again and asks no cache about it" $
        let (result, events, _) = runCheck Set.empty helloAndGcc [("hello-out", []), ("gcc-out", [])] ["hello-out"] "hello"
         in assertEqual
              "check"
              (Right (), [EvFetched [fakePath "gcc-out"], EvChecked "hello-out"], [EvReported Realise.Substituted "gcc-out", EvReported Realise.Checked "hello-out"], False)
              (result, realiseWork events, [event | event@(EvReported _ _) <- events], EvQueried helloOut `elem` events),
      runTest "a check builds an input no cache holds, and never the root" $
        let (result, events, _) = runCheck Set.empty helloAndGcc [] ["hello-out"] "hello"
         in assertEqual "check" (Right (), [EvBuilt "gcc-out", EvChecked "hello-out"]) (result, realiseWork events),
      runTest "an input that is valid is not built again" $
        let (result, events, _) = runCheck Set.empty helloAndGcc [] ["hello-out", "gcc-out"] "hello"
         in assertEqual "check" (Right (), [EvChecked "hello-out"]) (result, realiseWork events),
      -- registerOutputs raises NotDeterministic, and failingExitStatus
      -- gives the build that ends in it its own status.
      runTest "a rebuilt output that differs stops the check with upstream's message and status" $
        let (result, events, _) =
              runCheckIn Realise.NoFallback (fakeWorld [] ["hello-out"]) {fwUnstable = Set.singleton helloOut} Set.empty [fakeNode "hello" ["hello-out"] []] "hello"
            differs = Realise.NotDeterministic helloDrv helloOut
         in assertEqual "check" (Left differs, [EvChecked "hello-out"], []) (result, realiseWork events, [status | EvReported status _ <- events])
              `andThen` assertEqual
                "message"
                ("derivation '" <> helloDrvText <> "' may not be deterministic: output '" <> T.pack (storePathToFilePath defaultStoreDir helloOut) <> "' differs")
                (Realise.realiseFailureMessage defaultStoreDir differs)
              `andThen` assertEqual "status" 104 (Realise.realiseFailureStatus differs),
      -- printStorePath for the derivation, toRealPath for the output.
      runTest "the output that differs is named where the store's files are, and the derivation under the store's own directory" $
        assertEqual
          "message"
          ("derivation '" <> helloDrvText <> "' may not be deterministic: output '" <> T.pack (storePathToFilePath elsewhere helloOut) <> "' differs")
          (Realise.realiseFailureMessage elsewhere (Realise.NotDeterministic helloDrv helloOut)),
      -- Which output a rebuild finds different, and in what order it
      -- compares them, is the check effect's to say ("builder/check" pins
      -- the real one); the failure only passes its answer on.
      runTest "the output the check effect finds different is the one the failure names" $
        let (result, _, _) =
              runCheckIn
                Realise.NoFallback
                (fakeWorld [] ["pkg-bin", "pkg-lib"]) {fwUnstable = Set.singleton (fakePath "pkg-lib")}
                Set.empty
                [fakeNode "pkg" ["pkg-bin", "pkg-lib"] []]
                "pkg"
         in assertEqual "check" (Left (Realise.NotDeterministic (fakePath "pkg.drv") (fakePath "pkg-lib"))) result,
      -- Upstream's goal for a derivation it cannot load fails; a check
      -- that built nothing again and compared nothing has not passed.
      runTest "a check of a root the graph does not hold fails, where a build of one realises nothing and succeeds" $
        let (checked, events, _) = runCheck Set.empty [] [] [] "hello"
            (built, _, _) = runRealise Set.empty [] [] [] "hello"
         in assertEqual "check" (Left ("cannot build missing derivation '" <> helloDrvText <> "'"), []) (messageOf checked, events)
              `andThen` assertEqual "failure" (Left (Realise.MissingDerivation helloDrv)) checked
              `andThen` assertEqual "build" (Right ()) built,
      -- haveDerivation starts the substitution goals whatever the mode,
      -- and the check goes on once they have made the outputs valid.
      runTest "a check substitutes a root that is not valid, realises its inputs, then builds it again" $
        let (result, events, _) = runCheck Set.empty helloAndGcc [("hello-out", []), ("gcc-out", [])] [] "hello"
         in assertEqual
              "check"
              (Right (), [EvFetched [helloOut], EvFetched [fakePath "gcc-out"], EvChecked "hello-out"], [Realise.Substituted, Realise.Substituted, Realise.Checked])
              (result, realiseWork events, [status | EvReported status _ <- events]),
      runTest "of two outputs only the one that is not valid is fetched before the check" $
        let (result, events, _) =
              runCheck Set.empty [fakeNode "pkg" ["pkg-bin", "pkg-lib"] []] [("pkg-bin", []), ("pkg-lib", [])] ["pkg-bin"] "pkg"
         in assertEqual "check" (Right (), [EvFetched [fakePath "pkg-lib"], EvChecked "pkg-bin"]) (result, realiseWork events),
      -- outputsSubstitutionTried throws before gaveUpOnSubstitution, so no
      -- input is looked at.
      runTest "a root no cache holds cannot be checked, and nothing is asked about its inputs or built" $
        let (result, events, _) = runCheck Set.empty helloAndGcc [("gcc-out", [])] [] "hello"
         in assertEqual
              "check"
              (Left notValid, [], False)
              (messageOf result, realiseWork events, EvQueried (fakePath "gcc-out") `elem` events)
              `andThen` assertEqual "status" (Left 1) (either (Left . Realise.realiseFailureStatus) Right result),
      runTest "one output of two that no cache holds leaves nothing to check" $
        let (result, events, _) = runCheck Set.empty [fakeNode "pkg" ["pkg-bin", "pkg-lib"] []] [] ["pkg-bin"] "pkg"
         in assertEqual "check" (Left (Realise.OutputsNotValid (fakePath "pkg.drv")), []) (result, realiseWork events),
      runTest "a root that forbids substitutes is not fetched to be checked" $
        let (result, events, _) =
              runCheck Set.empty [fakeNodeWith forbidding "hello" ["hello-out"] []] [("hello-out", [])] [] "hello"
         in assertEqual "check" (Left notValid, []) (messageOf result, events),
      runTest "a substitute that fails stops a check as it stops a build, and under fallback leaves nothing to check" $
        let broken = (fakeWorld [("hello-out", [])] []) {fwBroken = Set.singleton helloOut}
            stopped fallback =
              let (result, events, _) = runCheckIn fallback broken Set.empty helloAndGcc "hello"
               in (result, realiseWork events)
         in assertEqual "no fallback" (Left (Realise.SubstitutesFailed helloDrv), []) (stopped Realise.NoFallback)
              `andThen` assertEqual "fallback" (Left (Realise.OutputsNotValid helloDrv), []) (stopped Realise.BuildFromSource),
      runTest "a cache that cannot be asked stops a check" $
        let unreachable = (fakeWorld [] []) {fwUnreachable = Set.singleton helloOut}
            (result, events, _) = runCheckIn Realise.NoFallback unreachable Set.empty helloAndGcc "hello"
         in assertEqual "check" (Left (Realise.CacheQueryFailed (fakeQueryFailure helloOut)), []) (result, realiseWork events),
      -- The error for outputs that are not valid comes before the inputs,
      -- so the retry 'realise' makes after them never happens: lib is
      -- not built to fill the hole, and hello is asked about once.
      runTest "a substitute with a hole in its closure is not retried after the inputs" $
        let (result, events, _) =
              runCheck Set.empty [fakeNode "hello" ["hello-out"] [("lib", ["lib-out"])], fakeNode "lib" ["lib-out"] []] [("hello-out", ["lib-out"])] [] "hello"
         in assertEqual
              "check"
              (Left (Realise.OutputsNotValid helloDrv), [], 1 :: Int)
              (result, realiseWork events, length [() | EvQueried path <- events, path == helloOut]),
      runTest "a failed input stops the check before the root is built again" $
        let (result, events, _) = runCheck (Set.singleton "gcc-out") helloAndGcc [] ["hello-out"] "hello"
         in assertEqual "check" (Left (Realise.BuildFailed "building gcc-out failed"), []) (result, realiseWork events),
      runTest "a rebuild that fails stops the check with the builder's report, and the status of a failed build" $
        let (result, events, _) = runCheck (Set.singleton "hello-out") helloAndGcc [] ["hello-out", "gcc-out"] "hello"
            failed = Realise.BuildFailed "building hello-out failed"
         in assertEqual "check" (Left failed, [EvChecked "hello-out"]) (result, realiseWork events)
              `andThen` assertEqual "status" 1 (Realise.realiseFailureStatus failed),
      -- haveDerivation reads substitutesAllowed for a valid derivation
      -- too once the mode keeps it from returning early.
      runTest "a check reads allowSubstitutes though the outputs are valid, where a build leaves it unread" $
        let nodes = [fakeNodeWith mistyped "hello" ["hello-out"] []]
            (checked, events, _) = runCheck Set.empty nodes [] ["hello-out"] "hello"
            (built, _, _) = runRealise Set.empty nodes [] ["hello-out"] "hello"
         in assertEqual
              "check"
              (Left ("attribute 'allowSubstitutes' of derivation '" <> helloDrvName <> "' must be a Boolean"), [])
              (messageOf checked, events)
              `andThen` assertEqual "build" (Right ()) built,
      runTest "only a rebuilt output that differs has a status of its own" $
        assertEqual
          "statuses"
          [104, 1, 1, 1, 1, 1, 1, 1]
          ( map
              Realise.realiseFailureStatus
              [ Realise.NotDeterministic helloDrv helloOut,
                Realise.BuildFailed "",
                Realise.CacheQueryFailed "",
                Realise.SubstitutesFailed helloDrv,
                Realise.InvalidDerivation "",
                Realise.OutputsNotValid helloDrv,
                Realise.MissingDerivation helloDrv,
                Realise.MissingOutputs helloDrv ("dev" :| [])
              ]
          ),
      -- realiseContext asks for one output of a derivation (primops.cc),
      -- and the goal it gets substitutes the outputs it wants alone
      -- (DerivationGoal::haveDerivation).
      runTest "a request for one output of three substitutes it alone, asks about no other, and builds nothing" $
        let (result, events, final) =
              runRequests (fakeWorld [("lib-out", []), ("lib-dev", []), ("lib-doc", [])] []) Set.empty [threeOutputLib] [("lib", ["lib-dev"])]
         in assertEqual
              "requests"
              (Right (), [EvFetched [fakePath "lib-dev"]], [fakePath "lib-dev"], Set.singleton (fakePath "lib-dev"))
              (result, realiseWork events, [path | EvQueried path <- events], fwValid final),
      -- One worker keeps one goal for a derivation, whichever request
      -- reaches it (Worker::makeDerivationGoalCommon in worker.cc at
      -- 2.24.9, Worker::initGoalIfNeeded at 2.36.0).
      runTest "an input two requests share is realised once" $
        let (result, events, _) =
              runRequests
                (fakeWorld [] [])
                Set.empty
                [ fakeNode "left" ["left-out"] [("base", ["base-out"])],
                  fakeNode "right" ["right-out"] [("base", ["base-out"])],
                  fakeNode "base" ["base-out"] []
                ]
                [("left", ["left-out"]), ("right", ["right-out"])]
         in assertEqual
              "requests"
              (Right (), [EvReported Realise.Built "base-out", EvReported Realise.Built "left-out", EvReported Realise.Built "right-out"])
              (result, [event | event@(EvReported _ _) <- events]),
      runTest "two requests for outputs of one derivation are realised as one request for both" $
        let (result, events, _) =
              runRequests
                (fakeWorld [("pkg-bin", []), ("pkg-lib", [])] [])
                Set.empty
                [fakeNode "pkg" ["pkg-bin", "pkg-lib"] []]
                [("pkg", ["pkg-lib"]), ("pkg", ["pkg-bin"])]
         in assertEqual
              "requests"
              (Right (), [EvFetched [fakePath "pkg-bin", fakePath "pkg-lib"]], [Realise.Substituted])
              (result, realiseWork events, [status | EvReported status _ <- events]),
      -- A failed goal ends every other unless keep-going is set
      -- (Worker::removeGoal in worker.cc), and buildPaths then names every
      -- request its worker did not finish (entry-points.cc).
      runTest "a request that fails leaves the ones after it unrealised, and the call names each" $
        let (result, events, _) =
              runRequests
                (fakeWorld [] [])
                (Set.singleton "a-out")
                [fakeNode "a" ["a-out"] [], fakeNode "b" ["b-out"] [], fakeNode "c" ["c-out"] []]
                [("c", ["c-out"]), ("a", ["a-out"]), ("b", ["b-out"])]
            leaving = Realise.FailedLeaving (Realise.BuildFailed "building a-out failed") (fakePath "a.drv") (fakePath "b.drv" :| [fakePath "c.drv"])
         in assertEqual "requests" (Left leaving, []) (result, realiseWork events)
              `andThen` assertEqual
                "message"
                ("build of " <> T.intercalate ", " ["'/nix/store/" <> T.replicate 32 name <> "-" <> name <> ".drv'" | name <- ["a", "b", "c"]] <> " failed")
                (Realise.requestsFailureMessage defaultStoreDir leaving)
              `andThen` assertEqual "status" 1 (Realise.requestsFailureStatus leaving),
      runTest "a request that fails once every other is realised is reported as its own failure" $
        let (result, events, _) =
              runRequests (fakeWorld [] []) (Set.singleton "b-out") [fakeNode "a" ["a-out"] [], fakeNode "b" ["b-out"] []] [("a", ["a-out"]), ("b", ["b-out"])]
         in assertEqual "requests" (Left (Realise.FailedWith (Realise.BuildFailed "building b-out failed")), [EvBuilt "a-out"]) (result, realiseWork events),
      -- The worker's first pass finishes a goal whose outputs are valid
      -- (Worker::run in worker.cc), before any build can fail.
      runTest "a request whose outputs are valid is realised before the others, and a failure does not name it" $
        let (result, events, _) =
              runRequests (fakeWorld [] ["z-out"]) (Set.singleton "a-out") [fakeNode "a" ["a-out"] [], fakeNode "z" ["z-out"] []] [("a", ["a-out"]), ("z", ["z-out"])]
         in assertEqual
              "requests"
              (Left (Realise.FailedWith (Realise.BuildFailed "building a-out failed")), [EvReported Realise.AlreadyValid "z-out"])
              (result, [event | event@(EvReported _ _) <- events]),
      -- c's goal is b's input goal, so it is done when d's build fails.
      runTest "a request realised as an input of one that fails is not named among those it leaves" $
        let (result, events, _) =
              runRequests
                (fakeWorld [] [])
                (Set.singleton "d-out")
                [fakeNode "b" ["b-out"] [("c", ["c-out"]), ("d", ["d-out"])], fakeNode "c" ["c-out"] [], fakeNode "d" ["d-out"] []]
                [("b", ["b-out"]), ("c", ["c-out"])]
         in assertEqual "requests" (Left (Realise.FailedWith (Realise.BuildFailed "building d-out failed")), [EvBuilt "c-out"]) (result, realiseWork events),
      -- A failed query is rethrown out of the worker
      -- (PathSubstitutionGoal::init in substitution-goal.cc at 2.24.9,
      -- PathSubstitutionGoal::substitute at 2.36.0), so buildPaths does
      -- not catch it.
      runTest "a cache that cannot be asked stops the call and is reported as it is, though requests are left" $
        let (result, events, _) =
              runRequests
                (fakeWorld [] []) {fwUnreachable = Set.singleton (fakePath "a-out")}
                Set.empty
                [fakeNode "a" ["a-out"] [], fakeNode "b" ["b-out"] []]
                [("a", ["a-out"]), ("b", ["b-out"])]
         in assertEqual
              "requests"
              (Left (Realise.FailedWith (Realise.CacheQueryFailed (fakeQueryFailure (fakePath "a-out")))), [])
              (result, realiseWork events),
      -- checkPathValidity throws in the worker's first pass
      -- (derivation-goal.cc), before any goal has built.
      runTest "a request for an output its derivation lacks stops the call before any request is realised" $
        let (result, events, _) =
              runRequests (fakeWorld [] []) Set.empty [fakeNode "a" ["a-out"] [], fakeNode "b" ["b-out"] []] [("a", ["a-out"]), ("b", ["b-man"])]
         in assertEqual "requests" (Left (Realise.FailedWith (Realise.MissingOutputs (fakePath "b.drv") ("b-man" :| []))), []) (result, realiseWork events)
              `andThen` assertEqual
                "message"
                (Left ("derivation '/nix/store/" <> T.replicate 32 "b" <> "-b.drv' does not have wanted outputs 'b-man'"))
                (either (Left . Realise.requestsFailureMessage defaultStoreDir) Right result),
      -- Upstream's worker takes its goals by key, the derivation's name
      -- and then its path (DerivationGoal::key in derivation-goal.cc):
      -- here the name and the path disagree.
      runTest "of two requests for outputs their derivations lack, the call reports the one named first, not the one whose path sorts first" $
        let first = StorePath (T.replicate 32 "z") "a.drv"
            second = StorePath (T.replicate 32 "b") "b.drv"
            at path (_, node) = (path, node)
            graph = DepGraph.DepGraph (Map.fromList [at first (fakeNode "a" ["a-out"] []), at second (fakeNode "b" ["b-out"] [])])
            settings = Realise.RealiseSettings Realise.NoFallback Realise.HeedAllowSubstitutes
            (result, _) = runState (Realise.realiseRequests settings (fakeRealiseEffects Set.empty) graph [(second, Set.singleton "b-man"), (first, Set.fromList ["a-man", "a-zed"])]) (fakeWorld [] [])
         in assertEqual "requests" (Left (Realise.FailedWith (Realise.MissingOutputs first ("a-man" :| ["a-zed"])))) result,
      -- A derivation's outputs are kept by output name, dev before out,
      -- where upstream's worker runs their substitution goals by key, the
      -- path's name and then the path (PathSubstitutionGoal::key in
      -- substitution-goal.hh).  An out path's name is the derivation's own,
      -- so it comes first, though its path sorts after dev's here.
      runTest "a derivation's missing outputs are asked of the caches in the order upstream's worker asks, by path name and then path" $
        let out = StorePath (T.replicate 32 "z") "two"
            dev = StorePath (T.replicate 32 "a") "two-dev"
            (drvPath, node) = fakeNode "two" [] []
            twoOutputs = node {DepGraph.dnDerivation = (DepGraph.dnDerivation node) {drvOutputs = [DerivationOutput "dev" dev "" "", DerivationOutput "out" out "" ""]}}
            settings = Realise.RealiseSettings Realise.NoFallback Realise.HeedAllowSubstitutes
            world = (fakeWorld [] []) {fwUnreachable = Set.fromList [out, dev]}
            (result, _) = runState (Realise.realiseRequests settings (fakeRealiseEffects Set.empty) (DepGraph.DepGraph (Map.singleton drvPath twoOutputs)) [(drvPath, Set.fromList ["dev", "out"])]) world
         in assertEqual "requests" (Left (Realise.FailedWith (Realise.CacheQueryFailed (fakeQueryFailure out)))) result,
      runTest "a call exits with its one failure's status, however it is reported" $
        let differs = Realise.NotDeterministic helloDrv helloOut
         in assertEqual
              "statuses"
              [104, 104]
              (map Realise.requestsFailureStatus [Realise.FailedWith differs, Realise.FailedLeaving differs helloDrv (fakePath "other.drv" :| [])]),
      -- 'checkLaw', over random graphs of one- and two-output derivations,
      -- caches, valid sets and faults, with each class of case it speaks
      -- about held to a share of the cases generated.
      runTestM "a check builds the root again only once it and its inputs are valid, and never otherwise" $
        property (QC.checkCoverage (QC.forAll genCheckCase checkLaw)),
      -- 'realiseLaw', over the same graphs, where an input is asked for
      -- some of its outputs.
      runTestM "a build waits for the outputs it wants of its inputs and leaves the root's outputs valid" $
        property (QC.checkCoverage (QC.forAll genCheckCase realiseLaw)),
      -- 'loneRequestLaw', over the same graphs.
      runTestM "a request for every output of the root realises it as a build of the root does" $
        property (QC.checkCoverage (QC.forAll genCheckCase loneRequestLaw))
    ]
  where
    forbidding = Map.singleton "allowSubstitutes" ""
    structured = Map.singleton "__json"
    mistyped = structured "{\"allowSubstitutes\":\"no\"}"
    alwaysAllowing = Realise.RealiseSettings Realise.NoFallback Realise.AlwaysAllowSubstitutes
    helloDrv = fakePath "hello.drv"
    helloOut = fakePath "hello-out"
    helloAndGcc = [fakeNode "hello" ["hello-out"] [("gcc", ["gcc-out"])], fakeNode "gcc" ["gcc-out"] []]
    threeOutputLib = fakeNode "lib" ["lib-out", "lib-dev", "lib-doc"] []
    -- app asks lib for lib-dev, and mid, which app also asks for, asks it
    -- for lib-out.
    laterRequest =
      [ fakeNode "app" ["app-out"] [("lib", ["lib-dev"]), ("mid", ["mid-out"])],
        fakeNode "mid" ["mid-out"] [("lib", ["lib-out"])],
        fakeNode "lib" ["lib-out", "lib-dev"] []
      ]
    helloDrvText = "/nix/store/" <> helloDrvName
    notValid = "some outputs of '" <> helloDrvText <> "' are not valid, so checking is not possible"
    messageOf = either (Left . Realise.realiseFailureMessage defaultStoreDir) Right
    -- Where a store's files are when they are not under its logical
    -- directory.
    elsewhere = StoreDir "elsewhere"
    helloDrvName = T.replicate 32 "h" <> "-hello.drv"
    unreadable = "cannot process __json attribute of '" <> helloDrvName <> "': "
    drvWith env = DepGraph.dnDerivation (snd (fakeNodeWith env "hello" ["hello-out"] []))
    heeding env =
      Realise.derivationAttrs helloDrv (drvWith env) >>= Realise.substitutesAllowed Realise.HeedAllowSubstitutes helloDrv
    -- Attributes as either kind of derivation holds them: any JSON value,
    -- or an environment whose allowSubstitutes, when it has one, is any
    -- bytes.
    genAttributes =
      QC.oneof
        [ Realise.AttrsInJson <$> genJson 3,
          Realise.AttrsInEnvironment . maybe Map.empty (Map.singleton "allowSubstitutes" . BS.pack) <$> QC.arbitrary
        ]
    property :: (QC.Testable prop) => prop -> IO TestResult
    property prop = do
      result <- QC.quickCheckWithResult QC.stdArgs {QC.chatty = False} prop
      pure (if QC.isSuccess result then Pass else Fail (T.pack (QC.output result)))

-- ---------------------------------------------------------------------------
-- Tests: DependencyGraph (Phase 3, Batch 5)
-- ---------------------------------------------------------------------------

testDepGraph :: IO [Bool]
testDepGraph = do
  putStrLn "dep-graph"
  let mkSP = StorePath
      spA = mkSP "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" "a.drv"
      spB = mkSP "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb" "b.drv"
      spC = mkSP "cccccccccccccccccccccccccccccccccc" "c.drv"
      spD = mkSP "dddddddddddddddddddddddddddddddd" "d.drv"
      baseDrv =
        Derivation
          { drvOutputs = [],
            drvInputDrvs = Map.empty,
            drvInputSrcs = [],
            drvPlatform = X86_64_Linux,
            drvBuilder = "/bin/sh",
            drvArgs = [],
            drvEnv = Map.empty
          }
      -- Single node: A has no deps
      drvA = baseDrv
      -- Linear chain: B depends on C
      drvB = baseDrv {drvInputDrvs = Map.singleton spC ["out"]}
      -- C has no deps
      drvC = baseDrv
      -- Diamond: D depends on B and C, B depends on C
      drvD = baseDrv {drvInputDrvs = Map.fromList [(spB, ["out"]), (spC, ["out"])]}
      -- Cycle: A depends on B, B depends on A
      drvACycle = baseDrv {drvInputDrvs = Map.singleton spB ["out"]}
      drvBCycle = baseDrv {drvInputDrvs = Map.singleton spA ["out"]}
      readSingle _ = Left "not found"
      readChain sp
        | sp == spC = Right drvC
        | otherwise = Left ("unknown drv: " <> spName sp)
      readDiamond sp
        | sp == spB = Right drvB
        | sp == spC = Right drvC
        | otherwise = Left ("unknown drv: " <> spName sp)
      readCycle sp
        | sp == spB = Right drvBCycle
        | sp == spA = Right drvACycle
        | otherwise = Left ("unknown drv: " <> spName sp)
      -- Self-loop: A depends on itself
      drvSelf = baseDrv {drvInputDrvs = Map.singleton spA ["out"]}
      readSelf sp
        | sp == spA = Right drvSelf
        | otherwise = Left ("unknown drv: " <> spName sp)
      -- D and B read from one lookup, as a store holds every .drv.
      readStored sp
        | sp == spD = Right drvD
        | otherwise = readDiamond sp
      rootsGraph roots = runIdentity (DepGraph.buildDepGraphFrom (Identity . readStored) roots)
  sequence
    [ runTest "a graph from several roots reads each root and holds every derivation one of them reaches" $
        assertEqual
          "graph"
          (Right (Map.fromList [(spB, drvB), (spC, drvC), (spD, drvD)]))
          (Map.map DepGraph.dnDerivation . DepGraph.unDepGraph <$> rootsGraph [spD, spB, spD]),
      runTest "a graph from several roots fails on a root that cannot be read" $
        assertEqual "graph" (Left "unknown drv: a.drv") (DepGraph.unDepGraph <$> rootsGraph [spB, spA]),
      runTest "a graph from one root is the graph from that root read" $
        assertEqual "graph" (pureDepGraph readStored drvD spD) (rootsGraph [spD]),
      -- Single node
      runTest "single node graph" $ case pureDepGraph readSingle drvA spA of
        Right (DepGraph.DepGraph g) -> assertEqual "single-size" 1 (Map.size g)
        Left err -> Fail ("unexpected error: " <> err),
      -- Linear chain A to C: topoSort should give [C, A]
      runTest "linear chain topo" $ case pureDepGraph readChain drvB spB of
        Right graph -> case DepGraph.topoSort graph of
          DepGraph.TopoSorted order ->
            case order of
              [first, _] | first == spC -> Pass
              _ -> Fail ("bad order: " <> T.pack (show order))
          DepGraph.TopoCycle cyc -> Fail ("unexpected cycle: " <> T.pack (show cyc))
        Left err -> Fail ("graph build failed: " <> err),
      -- Diamond D to B,C; B to C: topoSort should have C first, D last
      runTest "diamond topo" $ case pureDepGraph readDiamond drvD spD of
        Right graph -> case DepGraph.topoSort graph of
          DepGraph.TopoSorted order ->
            case order of
              [first, _, lastElem] | first == spC, lastElem == spD -> Pass
              _ -> Fail ("bad diamond order: " <> T.pack (show order))
          DepGraph.TopoCycle cyc -> Fail ("unexpected cycle: " <> T.pack (show cyc))
        Left err -> Fail ("graph build failed: " <> err),
      runTest "each edge carries the outputs the dependent's .drv names" $
        let drvWanting = baseDrv {drvInputDrvs = Map.fromList [(spB, ["out", "dev", "out"]), (spC, ["lib"])]}
         in case pureDepGraph readDiamond drvWanting spD of
              Right (DepGraph.DepGraph g) ->
                assertEqual
                  "edges"
                  (Just (Map.fromList [(spB, Set.fromList ["dev", "out"]), (spC, Set.singleton "lib")]), Just (Map.singleton spC (Set.singleton "out")))
                  (DepGraph.dnDeps <$> Map.lookup spD g, DepGraph.dnDeps <$> Map.lookup spB g)
              Left err -> Fail ("graph build failed: " <> err),
      -- transitiveDeps
      runTest "transitiveDeps diamond" $ case pureDepGraph readDiamond drvD spD of
        Right graph ->
          let deps = DepGraph.transitiveDeps graph spD
           in if Set.size deps == 2 && Set.member spB deps && Set.member spC deps
                then Pass
                else Fail ("bad transitive deps: " <> T.pack (show deps))
        Left err -> Fail ("graph build failed: " <> err),
      -- directDeps
      runTest "directDeps diamond" $ case pureDepGraph readDiamond drvD spD of
        Right graph ->
          let deps = DepGraph.directDeps graph spD
           in assertEqual "direct-count" 2 (length deps)
        Left err -> Fail ("graph build failed: " <> err),
      -- Missing .drv causes failure
      runTest "missing drv fails" $ case pureDepGraph readSingle drvB spB of
        Left _ -> Pass
        Right _ -> Fail "expected failure for missing drv",
      -- Single node topoSort
      runTest "single node topoSort" $ case pureDepGraph readSingle drvA spA of
        Right graph -> case DepGraph.topoSort graph of
          DepGraph.TopoSorted [x] -> assertEqual "single-topo" spA x
          other -> Fail ("unexpected topo result: " <> T.pack (show other))
        Left err -> Fail ("graph build failed: " <> err),
      -- buildDepGraph with mock for cycle detection
      -- (Cycle detection happens at topoSort level, not buildDepGraph)
      runTest "cycle detection" $ case pureDepGraph readCycle drvACycle spA of
        Right graph -> case DepGraph.topoSort graph of
          DepGraph.TopoCycle _ -> Pass
          -- A "sorted" cyclic graph means Kahn's silently dropped the
          -- cycle: buildWithDeps would then loop or build with unbuilt
          -- inputs.
          DepGraph.TopoSorted order ->
            Fail ("cyclic graph topo-sorted as " <> T.pack (show (length order)) <> " nodes")
        -- A loud rejection at graph-build time also counts as detection.
        Left _ -> Pass,
      -- A root that depends on itself: the walk marks the root visited
      -- on enqueue, so it terminates - and per contract the result
      -- excludes the root, leaving nothing.
      runTestM "transitiveDeps terminates on a self-loop" $ do
        outcome <-
          timeout walkWatchdogMicros $
            evaluate $ case pureDepGraph readSelf drvSelf spA of
              Left _ -> Nothing
              Right graph -> Just $! DepGraph.transitiveDeps graph spA
        pure $ case outcome of
          Nothing -> Fail "did not terminate on a self-loop"
          Just Nothing -> Fail "graph build failed"
          Just (Just deps) -> assertEqual "self-loop deps" Set.empty deps,
      -- A two-node cycle already terminated; pin that the root stays
      -- excluded from its own transitive closure.
      runTestM "transitiveDeps two-node cycle excludes the root" $ do
        outcome <-
          timeout walkWatchdogMicros $
            evaluate $ case pureDepGraph readCycle drvACycle spA of
              Left _ -> Nothing
              Right graph -> Just $! DepGraph.transitiveDeps graph spA
        pure $ case outcome of
          Nothing -> Fail "did not terminate on a cycle"
          Just Nothing -> Fail "graph build failed"
          Just (Just deps) -> assertEqual "cycle deps" (Set.singleton spB) deps
    ]
