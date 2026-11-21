-- | Regions (@cbits\/nn_region.h@): the C data layer's collector, which
-- frees at the end of each force of a thunk what the force allocated
-- that nothing reaches, and a census of what collectors would free there,
-- which frees nothing.
--
-- A session runs its regions for its arena's whole life ('withRegions'),
-- and each force in it opens a region as it starts and closes it once its
-- value is stored or it throws ('inRegion').  Under 'RegionsCollect' each
-- close the collector marks at frees the attribute sets, list headers,
-- lambdas, builtins' partial applications and strings with context that
-- the force allocated and nothing reaches, and the lines of thunk blocks,
-- env pages and string blocks that hold nothing it reached, which the
-- allocators then bump from again.  Under 'RegionsCensus' each
-- close marks, among what the force allocated, what its writes into older
-- cells and with-scopes and the roots it registered still reach, and
-- frees nothing.  'regionFigures' reads the census's counts
-- by size of region, and a simulation of collectors that mark only at
-- the closes of regions past a threshold of bytes, freeing single objects,
-- whole blocks of each of several sizes, or whole lines of a block of each
-- of two lengths.  It also counts what a close reached after an earlier
-- close found it unreached, which a collector would have freed under a
-- live pointer.  And it simulates, each as a t'Tenure', collectors that
-- tenure what their closes marked and mark it again only where a close
-- marks its region whole.
--
-- Under 'RegionsOff' a force pays for regions one match on its session's
-- setting, and the allocators one flag test per block, tracked object
-- and stored object.
module Nix.Eval.CRegion
  ( -- * Running regions
    Regions (..),
    withRegions,
    regionsRunning,
    inRegion,

    -- * Figures
    SizeClass (..),
    Threshold (..),
    Tenure (..),
    RegionFigures (..),
    regionFigures,
    renderRegionFigures,
  )
where

import Control.Exception (bracket_, throwIO)
import Control.Monad (unless)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import Data.Word (Word32, Word64)
import Foreign.C.Types (CInt (..))
import Foreign.Marshal.Array (allocaArray)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peek, peekElemOff)
import Nix.Eval.CStatus (CStatusError (..))

-- ---------------------------------------------------------------------------
-- FFI imports
-- ---------------------------------------------------------------------------

foreign import ccall unsafe "nn_region_begin"
  c_nn_region_begin :: CInt -> IO CInt

foreign import ccall unsafe "nn_region_end"
  c_nn_region_end :: IO ()

foreign import ccall unsafe "&nn_region_mode"
  c_nn_region_mode :: Ptr CInt

-- A close marks and retires one region, work bounded by what the force
-- allocated, between two steps of the single-threaded evaluator: an
-- unsafe call, as every allocation is.

foreign import ccall unsafe "nn_region_open"
  c_nn_region_open :: IO ()

foreign import ccall unsafe "nn_region_close"
  c_nn_region_close :: IO ()

foreign import ccall unsafe "nn_region_totals"
  c_nn_region_totals :: Ptr Word64 -> IO ()

foreign import ccall unsafe "nn_region_classes"
  c_nn_region_classes :: Ptr Word64 -> IO ()

foreign import ccall unsafe "nn_region_thresholds"
  c_nn_region_thresholds :: Ptr Word64 -> IO ()

foreign import ccall unsafe "nn_region_tenure"
  c_nn_region_tenure :: Word32 -> Ptr Word64 -> IO ()

foreign import ccall unsafe "nn_region_tenure_budget"
  c_nn_region_tenure_budget :: Word32 -> Word64

foreign import ccall unsafe "nn_region_threshold_bytes"
  c_nn_region_threshold_bytes :: Word32 -> Word64

foreign import ccall unsafe "nn_region_grain_bytes"
  c_nn_region_grain_bytes :: CInt -> Word32 -> Word64

-- ---------------------------------------------------------------------------
-- The layout of the figures, in lockstep with cbits/nn_region.h
-- ---------------------------------------------------------------------------

-- | @NN_REGION_TOTALS@ and the totals' places.
totalCount, totalCloses, totalAllocated, totalOutside, totalDepth, totalRecords, totalUnplaced, totalResurrected, totalResurrectedBytes, totalFailed :: Int
totalCount = 9
totalCloses = 0
totalAllocated = 1
totalOutside = 2
totalDepth = 3
totalRecords = 4
totalUnplaced = 5
totalResurrected = 6
totalResurrectedBytes = 7
totalFailed = 8

-- | @NN_REGION_SIZE_CLASSES@ and @NN_REGION_CLASS_FIGURES@.
classCount, classFigures :: Int
classCount = 48
classFigures = 6

-- | @NN_REGION_THRESHOLDS@, @NN_REGION_GRAINS@, @NN_REGION_BLOCK_GRAINS@
-- and @NN_REGION_THRESHOLD_FIGURES@: three counts, then a peak per grain.
thresholdCount, grainCount, blockGrainCount, thresholdFigures :: Int
thresholdCount = 9
grainCount = 8
blockGrainCount = 5
thresholdFigures = 3 + grainCount

-- | @NN_REGION_TENURE_VARIANTS@, @NN_REGION_TENURE_THRESHOLD@, and
-- @NN_REGION_TENURE_FIGURES@: eleven counts, then a peak per grain.
tenureVariants :: Word32
tenureVariants = 4

tenureThreshold :: Word32
tenureThreshold = 2

tenureFigures :: Int
tenureFigures = 11 + grainCount

-- | The bump kinds in order: @NN_REGION_THUNK_BLOCK@,
-- @NN_REGION_ENV_PAGE@ and @NN_REGION_STR_BLOCK@.
bumpKinds :: [CInt]
bumpKinds = [0, 1, 2]

-- | The bump kind a line grain's bytes are asked for, @NN_REGION_THUNK_BLOCK@:
-- a line is one size in every kind.
lineKind :: CInt
lineKind = 0

-- ---------------------------------------------------------------------------
-- Running regions
-- ---------------------------------------------------------------------------

-- | What a session's regions run for.
data Regions
  = -- | Nothing: no force opens a region, and the arena frees everything
    -- at its teardown.
    RegionsOff
  | -- | A census, which measures what collectors would free and frees
    -- nothing.
    RegionsCensus
  | -- | The collector.
    RegionsCollect
  deriving (Eq, Show)

-- | @NN_REGION_CENSUS@ and @NN_REGION_COLLECT@.
regionCensusMode, regionCollectMode :: CInt
regionCensusMode = 1
regionCollectMode = 2

-- | Run an action that sets up and tears down an arena with its regions
-- running, from before the arena's first block to after its teardown.
-- Raises 'CAllocationFailed' when the regions' tables cannot be
-- allocated.
withRegions :: Regions -> IO a -> IO a
withRegions regions action = case regions of
  RegionsOff -> action
  RegionsCensus -> running regionCensusMode
  RegionsCollect -> running regionCollectMode
  where
    running mode = bracket_ (begin mode) c_nn_region_end action
    begin mode = do
      status <- c_nn_region_begin mode
      unless (status == 0) (throwIO (CAllocationFailed "nn_region_begin" "the regions' tables could not be allocated"))

-- | What the regions running now run for, which a session sharing a
-- running arena takes on.
regionsRunning :: IO Regions
regionsRunning = running <$> peek c_nn_region_mode
  where
    running mode
      | mode == regionCensusMode = RegionsCensus
      | mode == regionCollectMode = RegionsCollect
      | otherwise = RegionsOff

-- | Run a force's work in a region of its own, while regions run.
inRegion :: Regions -> IO a -> IO a
inRegion RegionsOff action = action
inRegion _ action = bracket_ c_nn_region_open c_nn_region_close action

-- ---------------------------------------------------------------------------
-- Figures
-- ---------------------------------------------------------------------------

-- | The closes of regions that allocated from 'classFloor' bytes to
-- under twice that, or nothing for a floor of 0.
data SizeClass = SizeClass
  { classFloor :: !Word64,
    classCloses :: !Word64,
    -- | Bytes the regions allocated.
    classAllocated :: !Word64,
    -- | Bytes of theirs the closes reached.
    classSurviving :: !Word64,
    -- | Objects the marks entered.
    classEntered :: !Word64,
    -- | Pointers the marks examined.
    classExamined :: !Word64,
    -- | Records the closes read.
    classRecords :: !Word64
  }
  deriving (Eq, Show)

-- | A simulated collector that marks at a close only when its region
-- allocated at least 'thresholdBytes'.
data Threshold = Threshold
  { thresholdBytes :: !Word64,
    -- | The closes that mark, and their marks' objects and pointers.
    thresholdCloses :: !Word64,
    thresholdEntered :: !Word64,
    thresholdExamined :: !Word64,
    -- | The most bytes held at once, freeing single objects first, then
    -- freeing whole blocks of each block grain's size, largest first,
    -- then whole lines of each line grain's size, longest first.
    thresholdPeaks :: ![Word64]
  }
  deriving (Eq, Show)

-- | A simulated collector that marks at the closes of regions that
-- allocated at least 'tenureBytes' and then tenures the whole region:
-- each such close marks only what no such close of its inside tenured,
-- but one that marks its region whole, an outermost one or one whose
-- region holds at least 'tenureBudget' tenured bytes no mark of the
-- whole has reached since.
data Tenure = Tenure
  { -- | 0 for no budget, so that only outermost closes mark the whole.
    tenureBudget :: !Word64,
    tenureBytes :: !Word64,
    -- | The closes that mark, and of them those that mark the whole.
    tenureCloses :: !Word64,
    tenureFull :: !Word64,
    -- | Objects the marks entered, and pointers they examined.
    tenureEntered :: !Word64,
    tenureExamined :: !Word64,
    -- | The most records held at once.
    tenureRecords :: !Word64,
    -- | Pointers closes remembered for regions around them, and the most
    -- held at once.
    tenureRemembered :: !Word64,
    tenureHeld :: !Word64,
    -- | Units and places it freed that the census's own mark at the same
    -- close reached.
    tenureReached :: !Word64,
    -- | Objects and array entries a later close reached once it freed
    -- them, and their bytes.
    tenureResurrected :: !Word64,
    tenureResurrectedBytes :: !Word64,
    -- | Bytes of single objects the marks of the whole freed.
    tenureFullFreed :: !Word64,
    -- | The most bytes held at once, at each grain as 'thresholdPeaks'.
    tenurePeaks :: ![Word64]
  }
  deriving (Eq, Show)

-- | What a census of regions found.
data RegionFigures = RegionFigures
  { regionCloses :: !Word64,
    -- | Bytes the data layer allocated while the census ran.
    regionAllocated :: !Word64,
    -- | Of those, bytes allocated outside every region: between forces,
    -- which no close can free.
    regionOutside :: !Word64,
    -- | The most regions open at once.
    regionDepth :: !Word64,
    -- | The most records held at once.
    regionRecords :: !Word64,
    -- | Pointers the census could not place, which a graph it models in
    -- full never holds.
    regionUnplaced :: !Word64,
    -- | Objects and array entries a close reached after an earlier one
    -- found them unreached, and their bytes.
    regionResurrected :: !Word64,
    regionResurrectedBytes :: !Word64,
    -- | Every size class with a close in it, smallest first.
    regionClasses :: ![SizeClass],
    -- | Each threshold, smallest first.
    regionThresholds :: ![Threshold],
    -- | Each tenure collector, by its budget, smallest first and no
    -- budget first of all.
    regionTenures :: ![Tenure]
  }
  deriving (Eq, Show)

-- | The census's figures, read in the session it runs in.  'Left' when
-- one of its tables could not grow and it stopped counting.
regionFigures :: IO (Either CStatusError RegionFigures)
regionFigures =
  allocaArray totalCount $ \totals ->
    allocaArray (classCount * classFigures) $ \classes ->
      allocaArray (thresholdCount * thresholdFigures) $ \thresholds -> do
        c_nn_region_totals totals
        c_nn_region_classes classes
        c_nn_region_thresholds thresholds
        let total = peekElemOff totals
        failed <- total totalFailed
        if failed /= 0
          then pure (Left (CAllocationFailed "nn_region_close" "the census's tables could not grow"))
          else do
            sizeClasses <- traverse (sizeClass classes) [0 .. classCount - 1]
            simulated <- traverse (threshold thresholds) [0 .. thresholdCount - 1]
            tenured <- traverse tenure [0 .. tenureVariants - 1]
            figures <-
              RegionFigures
                <$> total totalCloses
                <*> total totalAllocated
                <*> total totalOutside
                <*> total totalDepth
                <*> total totalRecords
                <*> total totalUnplaced
                <*> total totalResurrected
                <*> total totalResurrectedBytes
                <*> pure (filter ((> 0) . classCloses) sizeClasses)
                <*> pure simulated
                <*> pure tenured
            pure (Right figures)
  where
    sizeClass classes n = do
      let figure i = peekElemOff classes (n * classFigures + i)
      SizeClass (classFloorOf n)
        <$> figure 0
        <*> figure 1
        <*> figure 2
        <*> figure 3
        <*> figure 4
        <*> figure 5
    classFloorOf n
      | n == 0 = 0
      | otherwise = 2 ^ (n - 1)
    threshold thresholds k = do
      let figure i = peekElemOff thresholds (k * thresholdFigures + i)
      Threshold (c_nn_region_threshold_bytes (fromIntegral k))
        <$> figure 0
        <*> figure 1
        <*> figure 2
        <*> traverse (figure . (3 +)) [0 .. grainCount - 1]
    tenure variant = allocaArray tenureFigures $ \tenures -> do
      c_nn_region_tenure variant tenures
      let figure = peekElemOff tenures
      Tenure (c_nn_region_tenure_budget variant) (c_nn_region_threshold_bytes tenureThreshold)
        <$> figure 0
        <*> figure 1
        <*> figure 2
        <*> figure 3
        <*> figure 4
        <*> figure 5
        <*> figure 6
        <*> figure 7
        <*> figure 8
        <*> figure 9
        <*> figure 10
        <*> traverse (figure . (11 +)) [0 .. grainCount - 1]

-- | The figures as lines of a name and its values: totals as
-- @regions.NAME VALUE@, a size class as @regions.class FLOOR CLOSES
-- ALLOCATED SURVIVING ENTERED EXAMINED RECORDS@, a threshold as
-- @regions.threshold BYTES CLOSES ENTERED EXAMINED@ and its peaks by
-- grain, each tenure collector as @regions.tenure BUDGET BYTES CLOSES FULL
-- ENTERED EXAMINED FULLFREED@ and its peaks, with its records, what it
-- remembered and held, what it freed that was reached and what was
-- reached once freed on lines of their own after its budget, each block
-- grain as @regions.grain GRAIN@ and its block bytes for thunk cells, env
-- pages and strings, and each line grain as @regions.line GRAIN BYTES@.
renderRegionFigures :: RegionFigures -> BS.ByteString
renderRegionFigures figures =
  BL.toStrict . BB.toLazyByteString $
    field "regions.closes" [regionCloses figures]
      <> field "regions.allocated.bytes" [regionAllocated figures]
      <> field "regions.outside.bytes" [regionOutside figures]
      <> field "regions.depth" [regionDepth figures]
      <> field "regions.records" [regionRecords figures]
      <> field "regions.unplaced" [regionUnplaced figures]
      <> field "regions.resurrected.count" [regionResurrected figures]
      <> field "regions.resurrected.bytes" [regionResurrectedBytes figures]
      <> foldMap sizeClass (regionClasses figures)
      <> foldMap threshold (regionThresholds figures)
      <> foldMap tenure (regionTenures figures)
      <> foldMap grain [1 .. fromIntegral blockGrainCount]
      <> foldMap line [fromIntegral blockGrainCount + 1 .. fromIntegral grainCount - 1]
  where
    field name values =
      BB.byteString name <> foldMap ((BB.char7 ' ' <>) . BB.word64Dec) values <> BB.char7 '\n'
    sizeClass (SizeClass lowest closes allocated surviving entered examined records) =
      field "regions.class" [lowest, closes, allocated, surviving, entered, examined, records]
    threshold (Threshold bytes closes entered examined peaks) =
      field "regions.threshold" ([bytes, closes, entered, examined] <> peaks)
    tenure t =
      field "regions.tenure" ([tenureBudget t, tenureBytes t, tenureCloses t, tenureFull t, tenureEntered t, tenureExamined t, tenureFullFreed t] <> tenurePeaks t)
        <> field "regions.tenure.records" [tenureBudget t, tenureRecords t]
        <> field "regions.tenure.remembered" [tenureBudget t, tenureRemembered t, tenureHeld t]
        <> field "regions.tenure.reached" [tenureBudget t, tenureReached t]
        <> field "regions.tenure.resurrected" [tenureBudget t, tenureResurrected t, tenureResurrectedBytes t]
    grain g = field "regions.grain" (fromIntegral g : [c_nn_region_grain_bytes kind g | kind <- bumpKinds])
    line g = field "regions.line" [fromIntegral g, c_nn_region_grain_bytes lineKind g]
