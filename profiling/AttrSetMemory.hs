{-# LANGUAGE OverloadedStrings #-}

-- | The attrset-memory benchmark: what a population of attribute sets
-- costs held in C, as nova-nix holds them (@cbits\/nn_attrset.c@), and
-- held in GHC's heap as flat arrays, in bytes per entry and in the time a
-- major collection takes while the population is live.
--
-- @attrset-memory c|flat SIZES@ builds one set for each set the file
-- @SIZES@ lists, a line of an entry count and how many sets have it (the
-- census's reached sets, which bench.sh's memory suite passes it), and
-- prints its figures as lines of a name and a value.  The collector is the
-- RTS's: the default copying one, or the non-moving one under
-- @+RTS -xn@.  Each timed collection is a full major one: under the
-- non-moving collector it marks inside the pause, where a running program
-- marks beside the mutator and pauses only to synchronise, so its pause
-- here is the collector's whole work.  A set's bytes in GHC's heap are
-- the copying collector's live bytes: the non-moving collector reports
-- for its heap the estimate its previous mark left (genLiveUncopiedWords,
-- rts/sm/Storage.c at GHC 9.14.1), so under it they are not measured.
--
-- A C set is built through "Nix.Eval.CAttrSet" as the evaluator builds a
-- set literal's, with room for its entries and no more.  The evaluator's
-- sets from @removeAttrs@, and from @\/\/@ when it merges, have room for
-- every entry of their inputs, so a C set of the same size there costs at
-- least what it does here, while one @\/\/@ layers holds the right
-- operand's entries alone; bench.sh's memory report gives the census's
-- own figure beside.
-- A flat set holds the same keys and positions in one 'PrimArray', the
-- keys first as the C set's shared block holds them, and its values in a
-- 'SmallArray'.  Every value is one shared closure, so that the figures
-- are the sets' own and not their values': a value slot costs a pointer
-- in both, as it does in the evaluator.
module Main (main) where

import Control.Exception (evaluate)
import Control.Monad (forM_, replicateM_)
import qualified Data.ByteString.Char8 as BS8
import Data.Primitive.Array (Array, newArray, sizeofArray, unsafeFreezeArray, writeArray)
import Data.Primitive.PrimArray (PrimArray, newPrimArray, primArrayFromListN, sizeofPrimArray, unsafeFreezePrimArray, writePrimArray)
import Data.Primitive.SmallArray (SmallArray, newSmallArray, runSmallArray)
import Data.Text (Text)
import Data.Word (Word32, Word64)
import GHC.Stats (GCDetails (..), RTSStats (..), getRTSStats)
import Nix.Builder (BuildStop (..))
import Nix.Eval.AllocSite (SetSite (SetLiteral))
import Nix.Eval.CAttrSet (CAttrSet, cattrsetFreeze, cattrsetInsert, cattrsetNew)
import Nix.Eval.CRegion (Regions (RegionsOff))
import Nix.Eval.CThunk (cthunkNull)
import Nix.Eval.IO (SessionSettings (..), SessionStore (ReadOnlySession), withEvalSession)
import Nix.Eval.Policy (unrestrictedPolicy)
import Nix.Eval.SourcePos (PosIdx (..))
import Nix.Eval.Symbol (Symbol (..))
import Nix.Store.Ops (StoreOps (..), StoreQueries (..))
import Nix.Store.Path (defaultStoreDir)
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitWith)
import System.IO (hPutStrLn, stderr)
import System.Mem (performBlockingMajorGC)

foreign import ccall unsafe "heap_in_use"
  c_heap_in_use :: IO Word64

-- | @HEAP_IN_USE_UNKNOWN@ in heap-in-use.c.
heapInUseUnknown :: Word64
heapInUseUnknown = maxBound

-- | Major collections timed with the population live, after the one that
-- settles it.
timedCollections :: Int
timedCollections = 5

-- | Apart from each other, so that a set's keys look like interned
-- symbols rather than a run of consecutive numbers.
keyStride :: Word32
keyStride = 7

nanosPerSecond :: Double
nanosPerSecond = 1e9

-- | Where the population lives.
data Representation = InC | InGhcHeap

-- | Which collector the RTS runs, as its statistics show it: only the
-- non-moving one spends time marking its own heap.
data Collector = Copying | NonMoving

-- | A set held in GHC's heap: its keys, then its positions, and its
-- values.
data FlatAttrs = FlatAttrs !(PrimArray Word32) !(SmallArray ())

main :: IO ()
main = do
  args <- getArgs
  case args of
    [representation, sizesPath] -> do
      sizes <- parseSizes <$> BS8.readFile sizesPath
      case (representation, sizes) of
        (_, Left problem) -> usage problem
        ("c", Right counted) -> inSession (measure InC counted (buildC counted) sizeofPrimArray)
        ("flat", Right counted) -> measure InGhcHeap counted (buildFlat counted) sizeofArray
        _ -> usage ("no representation " <> representation)
    _ -> usage "expected a representation and a sizes file"

-- | Run the C population's measure in an evaluation session, the only
-- place the C data layer makes a set.  The session runs no regions, so
-- the C allocator's growth is the sets' alone and none of the
-- collector's bookkeeping, and it opens its arena before the measure
-- takes its first figure.  Nothing is evaluated, so its store holds
-- nothing and refuses every operation.
inSession :: IO a -> IO a
inSession action = withEvalSession settings (const action)
  where
    settings = SessionSettings unrestrictedPolicy (pure []) defaultStoreDir "/" RegionsOff (ReadOnlySession queries ops)
    queries = StoreQueries {sqValidPaths = const (pure mempty), sqReferences = const (pure (Right mempty))}
    ops =
      StoreOps
        { soQueries = queries,
          soAddEvalObjects = const (pure (Left refusal)),
          soAddSourceTree = \_ _ _ -> pure (Left refusal),
          soBuild = const (pure (Left (BuildStop refusal 1))),
          soCheck = const (pure (Left (BuildStop refusal 1))),
          soAddOutLinkRoot = \_ _ -> pure (Left refusal)
        }
    refusal :: Text
    refusal = "the attrset-memory benchmark evaluates nothing"

usage :: String -> IO a
usage problem = do
  hPutStrLn stderr ("attrset-memory: " <> problem)
  hPutStrLn stderr "usage: attrset-memory c|flat SIZES [+RTS -xn]"
  exitWith (ExitFailure 2)

-- | Each line's entry count and how many sets have it.
parseSizes :: BS8.ByteString -> Either String [(Int, Int)]
parseSizes = traverse line . filter (not . BS8.null) . BS8.lines
  where
    line text = case map BS8.readInt (BS8.words text) of
      [Just (size, rest), Just (count, more)]
        | BS8.null rest && BS8.null more && size >= 0 && count >= 0 -> Right (size, count)
      _ -> Left ("not an entry count and a number of sets: " <> BS8.unpack text)

-- | Build the population, settle it with a major collection and time
-- more with it live, then print the figures.  The population is used
-- after the timed collections, which keeps it live through them.  Its
-- bytes are GHC's live bytes for sets in GHC's heap, and the C
-- allocator's growth across the build for C sets, which a C library
-- heap-in-use.c has no figure for leaves unknown.
measure :: Representation -> [(Int, Int)] -> IO population -> (population -> Int) -> IO ()
measure representation sizes construct count = do
  before <- settle
  cBefore <- c_heap_in_use
  population <- construct
  built <- settle
  cBuilt <- c_heap_in_use
  replicateM_ timedCollections performBlockingMajorGC
  timed <- getRTSStats
  held <- evaluate (count population)
  let entries = sum [size * sets | (size, sets) <- sizes]
      collector = if nonmoving_gc_elapsed_ns timed > 0 then NonMoving else Copying
      ghcBytes = gcdetails_live_bytes (gc built) - gcdetails_live_bytes (gc before)
      bytes = case (representation, collector) of
        (InGhcHeap, Copying) -> Just ghcBytes
        (InGhcHeap, NonMoving) -> Nothing
        (InC, _)
          | cBefore == heapInUseUnknown -> Nothing
          | otherwise -> Just (cBuilt - cBefore)
      collections = fromIntegral (major_gcs timed - major_gcs built) :: Double
      perCollection field = fromIntegral (field timed - field built) / collections / nanosPerSecond
  mapM_
    (\(name, value) -> putStrLn (name <> " " <> value))
    [ ("collector", case collector of Copying -> "copying"; NonMoving -> "non-moving"),
      ("sets", show held),
      ("entries", show entries),
      ("bytes", maybe "unknown" show bytes),
      ("bytes-per-entry", maybe "unknown" (\b -> show (fromIntegral b / fromIntegral (max 1 entries) :: Double)) bytes),
      ("major-gcs", show (major_gcs timed - major_gcs built)),
      ("gc-pause-seconds", show (perCollection gc_elapsed_ns)),
      ("gc-cpu-seconds", show (perCollection gc_cpu_ns)),
      ("mark-seconds", show (perCollection nonmoving_gc_elapsed_ns))
    ]

-- | A major collection, and the statistics after it.  While the
-- non-moving collector's concurrent mark runs, GHC downgrades a forced
-- major collection to a minor one (GarbageCollect, rts/sm/GC.c at
-- 9.14.1), and nothing in base waits for that mark, so the collection is
-- repeated until one is major.  It is a non-concurrent one, as every
-- 'performBlockingMajorGC' is, so no mark runs once it returns and every
-- later one is major too.
settle :: IO RTSStats
settle = do
  start <- getRTSStats
  performBlockingMajorGC
  after <- getRTSStats
  if major_gcs after > major_gcs start then pure after else settle

-- | One size per set, in the file's order.
perSet :: [(Int, Int)] -> [Int]
perSet sizes = concat [replicate count size | (size, count) <- sizes]

-- | A set's keys: distinct and ascending, as a frozen C set's are.
keysOf :: Int -> [Word32]
keysOf size = [fromIntegral i * keyStride + 1 | i <- [0 .. size - 1]]

buildC :: [(Int, Int)] -> IO (PrimArray CAttrSet)
buildC sizes = do
  let each = perSet sizes
  slots <- newPrimArray (length each)
  -- The benchmark never counts by site, so the site is a placeholder.
  forM_ (zip [0 ..] each) $ \(i, size) -> do
    set <- cattrsetNew SetLiteral size
    forM_ (zip (keysOf size) [0 ..]) $ \(key, pos) ->
      cattrsetInsert set (Symbol key) (PosIdx pos) cthunkNull
    cattrsetFreeze set
    writePrimArray slots i set
  unsafeFreezePrimArray slots

buildFlat :: [(Int, Int)] -> IO (Array FlatAttrs)
buildFlat sizes = do
  let each = perSet sizes
  slots <- newArray (length each) (FlatAttrs mempty (flatValues 0))
  forM_ (zip [0 ..] each) $ \(i, size) ->
    writeArray slots i =<< evaluate (flatAttrs size)
  unsafeFreezeArray slots

flatAttrs :: Int -> FlatAttrs
flatAttrs size =
  FlatAttrs
    (primArrayFromListN (2 * size) (keysOf size ++ map fromIntegral [0 .. size - 1]))
    (flatValues size)

flatValues :: Int -> SmallArray ()
flatValues size = runSmallArray (newSmallArray size ())
