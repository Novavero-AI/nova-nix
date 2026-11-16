{-# LANGUAGE DeriveTraversable #-}

-- | A census of the C data layer (@cbits\/nn_stats.h@): what it holds by
-- kind of object, and what a set of roots reaches.
--
-- 'takeCensus' counts every object the allocators hold and then marks
-- from the roots it is given, following every C pointer and, through
-- 'valueRoots', every C pointer a StablePtr-held value holds.  It frees
-- and writes nothing in the data layer, and it adds no work to any
-- allocation, so a process that never takes a census pays nothing for it.
--
-- Once 'countSites' turns counting on, the allocators also count each
-- environment and attribute set under its site ("Nix.Eval.AllocSite"),
-- which 'siteCounts' and every census after it read.  With counting off,
-- an allocation tests one C flag and counts nothing.
--
-- What it reaches is exact for the roots it is given, and how close that
-- is to what the process still uses depends on the roots.  Haskell holds
-- C pointers in values the C side cannot enumerate, so an object only
-- such a value holds goes unmarked unless a root reaches it: the reached
-- figures are a lower bound on what is live, and the rest an upper bound
-- on what no root reaches.  Two parts of the layer are not marked at all.
-- Symbols are reached through the values that hold them, not through the
-- compiled code that names them, so the symbols reached are a lower bound
-- on the symbols in use.  The bytecode store is counted and never marked,
-- since nothing frees code, and a value pointing at one of the code's
-- strings (a literal's bytes) reaches nothing the census counts.
module Nix.Eval.CStats
  ( -- * Kinds of object
    PerKind (..),

    -- * Figures
    Figures (..),
    Census (..),

    -- * Counting by site
    EnvSize (..),
    EnvSiteFigures (..),
    SetSiteFigures (..),
    SiteCounts (..),
    countSites,
    siteCounts,
    envSitesCounted,
    setSitesCounted,

    -- * Roots
    CRoot (..),
    valueRoots,

    -- * Taking a census
    takeCensus,

    -- * Report
    renderCensus,
  )
where

import Control.Exception (finally)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (toList)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Word (Word32, Word64)
import Foreign.C.Types (CInt (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (allocaArray, peekArray)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.StablePtr (castPtrToStablePtr, deRefStablePtr)
import Foreign.Storable (peek, peekElemOff)
import GHC.Clock (getMonotonicTime)
import Nix.Eval.AllocSite (EnvSite, SetSite, envSiteIndex, envSiteName, setSiteIndex, setSiteName)
import Nix.Eval.CCtxStr (CCtxStrPtr)
import Nix.Eval.CList (CList (..))
import Nix.Eval.CStatus (CStatusError (..))
import Nix.Eval.CStr (CStrPtr)
import Nix.Eval.Types (AttrSet (..), BuiltinArg (..), Env (..), HeldBytes (..), NixValue (..), Thunk (..))
import Numeric (showFFloat)

-- ---------------------------------------------------------------------------
-- FFI imports
-- ---------------------------------------------------------------------------

-- The index build and the mark run for as long as the data layer is big,
-- so they are safe calls, which let the RTS go on without them.

foreign import ccall safe "nn_stats_begin"
  c_nn_stats_begin :: IO CInt

foreign import ccall unsafe "nn_stats_end"
  c_nn_stats_end :: IO ()

foreign import ccall unsafe "nn_stats_allocated"
  c_nn_stats_allocated :: Ptr Word64 -> Ptr Word64 -> IO ()

foreign import ccall unsafe "nn_stats_overhead_bytes"
  c_nn_stats_overhead_bytes :: IO Word64

foreign import ccall unsafe "nn_stats_code_bytes"
  c_nn_stats_code_bytes :: IO Word64

foreign import ccall unsafe "nn_stats_stableptrs"
  c_nn_stats_stableptrs :: IO Word64

foreign import ccall unsafe "nn_stats_root_thunk"
  c_nn_stats_root_thunk :: Ptr () -> IO ()

foreign import ccall unsafe "nn_stats_root_env"
  c_nn_stats_root_env :: Ptr () -> IO ()

foreign import ccall unsafe "nn_stats_root_attrset"
  c_nn_stats_root_attrset :: Ptr () -> IO ()

foreign import ccall unsafe "nn_stats_root_list"
  c_nn_stats_root_list :: Ptr () -> IO ()

foreign import ccall unsafe "nn_stats_root_string"
  c_nn_stats_root_string :: Ptr () -> IO ()

foreign import ccall unsafe "nn_stats_root_ctxstr"
  c_nn_stats_root_ctxstr :: Ptr () -> IO ()

foreign import ccall safe "nn_stats_mark"
  c_nn_stats_mark :: IO CInt

foreign import ccall unsafe "nn_stats_pending_stableptrs"
  c_nn_stats_pending_stableptrs :: IO Word32

foreign import ccall unsafe "nn_stats_take_stableptrs"
  c_nn_stats_take_stableptrs :: Ptr (Ptr ()) -> Word32 -> IO Word32

foreign import ccall unsafe "nn_stats_reached"
  c_nn_stats_reached :: Ptr Word64 -> Ptr Word64 -> IO ()

foreign import ccall unsafe "nn_stats_visited"
  c_nn_stats_visited :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

foreign import ccall safe "nn_stats_reached_set_sizes"
  c_nn_stats_reached_set_sizes :: Ptr Word32 -> Ptr Word64 -> Word32 -> Ptr Word32 -> IO CInt

foreign import ccall unsafe "nn_stats_count_sites"
  c_nn_stats_count_sites :: IO ()

foreign import ccall unsafe "&nn_stats_counting"
  c_nn_stats_counting :: Ptr CInt

foreign import ccall unsafe "nn_stats_env_sites"
  c_nn_stats_env_sites :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> Word32 -> IO Word32

foreign import ccall unsafe "nn_stats_env_sizes"
  c_nn_stats_env_sizes :: Ptr Word64 -> Word32 -> IO Word32

foreign import ccall unsafe "nn_stats_set_sites"
  c_nn_stats_set_sites :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> Word32 -> IO Word32

-- ---------------------------------------------------------------------------
-- Kinds of object
-- ---------------------------------------------------------------------------

-- | One of something for each kind of object the data layer holds.
data PerKind a = PerKind
  { -- | Thunk cells in the arena's blocks.
    kindThunks :: !a,
    -- | Envs, with their slots, and their with-scope arrays.
    kindEnvs :: !a,
    -- | Attribute sets: each header with its keys, positions and values.
    kindAttrSets :: !a,
    -- | List headers, with the items arrays they span.
    kindLists :: !a,
    -- | Lambda closures and their formal entries.
    kindLambdas :: !a,
    -- | Strings with context.
    kindCtxStrs :: !a,
    -- | The bytes of strings and paths, a string with context's text and
    -- its context's store paths and output names among them.
    kindStrings :: !a,
    -- | Interned names: attribute names, formals and variables.
    kindSymbols :: !a
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)

-- | Kind by kind: 'pure' is the same value for every kind, and '<*>'
-- applies each kind's function to that kind's value.
instance Applicative PerKind where
  pure a = PerKind a a a a a a a a
  PerKind f1 f2 f3 f4 f5 f6 f7 f8 <*> PerKind a1 a2 a3 a4 a5 a6 a7 a8 =
    PerKind (f1 a1) (f2 a2) (f3 a3) (f4 a4) (f5 a5) (f6 a6) (f7 a7) (f8 a8)

-- | Each kind's index in the arrays @nn_stats_allocated@ and
-- @nn_stats_reached@ fill: the @NN_STATS_*@ constants of
-- @cbits\/nn_stats.h@, which this must match.
kindIndices :: PerKind Int
kindIndices = PerKind 0 1 2 3 4 5 6 7

-- | @NN_STATS_KIND_COUNT@: the length of those arrays.
kindCount :: Int
kindCount = 8

-- | @NN_STATS_UNCOUNTED@: a count no allocator keeps.
uncounted :: Word64
uncounted = maxBound

-- | Each kind's name in 'renderCensus'.
kindNames :: PerKind BS.ByteString
kindNames = PerKind "thunks" "envs" "attrsets" "lists" "lambdas" "ctxstrs" "strings" "symbols"

-- ---------------------------------------------------------------------------
-- Figures
-- ---------------------------------------------------------------------------

-- | Objects of one kind and the bytes they hold from the allocator.
data Figures = Figures
  { -- | How many, or 'Nothing' where no allocator keeps a count: the env
    -- pages hold envs and with-scope arrays side by side, so unless
    -- counting by site ('countSites') has covered every env, the envs
    -- allocated are known in bytes and not in number.
    figuresCount :: !(Maybe Word64),
    figuresBytes :: !Word64
  }
  deriving (Eq, Show)

-- | A census: what the data layer holds, what the roots reach, and what
-- reaching it cost.
data Census = Census
  { -- | Everything allocated, by kind.
    censusAllocated :: !(PerKind Figures),
    -- | What the roots reach, by kind.  An env's with-scope array counts
    -- in the bytes of the first env over it reached and not in its count;
    -- a list's items array counts once whichever header over it is
    -- reached.
    censusReached :: !(PerKind Figures),
    -- | Bytes the allocators hold that no object does: block and page
    -- headers and unused tails, the tracking arrays, and the symbol
    -- table beyond its text.
    censusOverheadBytes :: !Word64,
    -- | Bytes the bytecode store holds.
    censusCodeBytes :: !Word64,
    -- | The StablePtrs the thunk arena holds, one per thunk whose value
    -- lives in GHC's heap.
    censusStablePtrs :: !Word64,
    -- | Those of them the roots reach.
    censusStablePtrsReached :: !Word64,
    -- | Objects the mark entered: thunks, envs, with-scope arrays, sets,
    -- list headers, items arrays, lambdas and strings with context.
    -- Strings and symbols hold no pointer, and are marked and not entered.
    censusVisitedObjects :: !Word64,
    -- | The bytes those objects hold.
    censusVisitedBytes :: !Word64,
    -- | Pointers the mark met that it could not place at the start of an
    -- object it holds, and so did not follow: none while the mark models
    -- every pointer the data layer stores.
    censusUnplaced :: !Word64,
    -- | Wall-clock seconds the count and the index took.
    censusIndexSeconds :: !Double,
    -- | Wall-clock seconds the mark took, the rounds through StablePtrs
    -- included.
    censusMarkSeconds :: !Double,
    -- | The reached attribute sets by their number of entries.
    censusSetSizes :: !(Map Word32 Word64),
    -- | What counting by site has found, when it is on.
    censusSites :: !(Maybe SiteCounts)
  }
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Counting by site
-- ---------------------------------------------------------------------------

-- | The size of an environment: the slots it holds.
data EnvSize
  = -- | Exactly this many.
    EnvSlots !Word32
  | -- | This many or more: the sizes from @NN_STATS_ENV_SIZES@ - 1 on
    -- share one count.
    EnvSlotsAtLeast !Word32
  deriving (Eq, Ord, Show)

-- | The environments one site allocated.
data EnvSiteFigures = EnvSiteFigures
  { -- | How many.
    envSiteEnvs :: !Word64,
    -- | The slots they hold.
    envSiteSlots :: !Word64,
    -- | Every byte of the env pages they took: the envs with their slots,
    -- and their with-scope arrays.
    envSiteBytes :: !Word64
  }
  deriving (Eq, Show)

-- | The attribute sets one site allocated.
data SetSiteFigures = SetSiteFigures
  { -- | How many.
    setSiteSets :: !Word64,
    -- | Those equal to a set that already existed: one with no entries,
    -- or a union one of whose operands had none.
    setSiteShareable :: !Word64,
    -- | Their bytes once built: headers, keys, positions and values.
    setSiteBytes :: !Word64
  }
  deriving (Eq, Show)

-- | What counting by site has found since 'countSites'.
data SiteCounts = SiteCounts
  { -- | Every env site, with what it allocated.
    sitesEnvs :: !(Map EnvSite EnvSiteFigures),
    -- | The environments allocated, by size; sizes none had are absent.
    sitesEnvSizes :: !(Map EnvSize Word64),
    -- | Every set site, with what it allocated.
    sitesSets :: !(Map SetSite SetSiteFigures)
  }
  deriving (Eq, Show)

-- | @NN_STATS_ENV_SIZES@: the buckets of environments by size, the last
-- for every size from its index on.
envSizeBuckets :: Word32
envSizeBuckets = 33

-- | The size from which environments share one bucket.
envSizeOpen :: Word32
envSizeOpen = envSizeBuckets - 1

-- | Turn counting by site on, every count from zero.  Turned on before the
-- first environment is allocated, it covers every one, and a census then
-- reports their number.
countSites :: IO ()
countSites = c_nn_stats_count_sites

-- | What counting by site has found, or 'Nothing' while it is off.
siteCounts :: IO (Maybe SiteCounts)
siteCounts = do
  counting <- peek c_nn_stats_counting
  if counting == 0
    then pure Nothing
    else do
      envs <- readSites envSiteCount c_nn_stats_env_sites
      sizes <- allocaArray (fromIntegral envSizeBuckets) $ \buf -> do
        buckets <- c_nn_stats_env_sizes buf envSizeBuckets
        counts <- peekArray (fromIntegral (min buckets envSizeBuckets)) buf
        pure (Map.fromList [(bucket i, n) | (i, n) <- zip [0 ..] counts, n /= 0])
      sets <- readSites setSiteCount c_nn_stats_set_sites
      pure (Just (SiteCounts (siteMap envSiteIndex envFigures envs) sizes (siteMap setSiteIndex setFigures sets)))
  where
    bucket i
      | i < envSizeOpen = EnvSlots i
      | otherwise = EnvSlotsAtLeast envSizeOpen
    envFigures (envs, slots, bytes) = EnvSiteFigures envs slots bytes
    setFigures (sets, shareable, bytes) = SetSiteFigures sets shareable bytes
    siteMap index figures counted =
      Map.fromList [(site, figures (Map.findWithDefault (0, 0, 0) (index site) counted)) | site <- [minBound .. maxBound]]

-- | How many env sites the C side counts, which must be every 'EnvSite'.
envSitesCounted :: IO Word32
envSitesCounted = c_nn_stats_env_sites nullPtr nullPtr nullPtr 0

-- | How many set sites the C side counts, which must be every 'SetSite'.
setSitesCounted :: IO Word32
setSitesCounted = c_nn_stats_set_sites nullPtr nullPtr nullPtr 0

-- | The number of sites the evaluator names.
envSiteCount, setSiteCount :: Word32
envSiteCount = fromIntegral (length [minBound .. maxBound :: EnvSite])
setSiteCount = fromIntegral (length [minBound .. maxBound :: SetSite])

-- | Three figures per site, keyed by site number, from a C function that
-- fills three arrays for the first @count@ sites and answers how many it
-- counts.
readSites ::
  Word32 ->
  (Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> Word32 -> IO Word32) ->
  IO (Map Word32 (Word64, Word64, Word64))
readSites count fill =
  allocaArray n $ \first -> allocaArray n $ \second -> allocaArray n $ \third -> do
    counted <- fill first second third count
    let k = fromIntegral (min counted count)
    rows <- zip3 <$> peekArray k first <*> peekArray k second <*> peekArray k third
    pure (Map.fromList (zip [0 ..] rows))
  where
    n = fromIntegral count

-- ---------------------------------------------------------------------------
-- Roots
-- ---------------------------------------------------------------------------

-- | A C object a census marks from.
data CRoot
  = RootThunk !Thunk
  | RootEnv !Env
  | RootAttrSet !AttrSet
  | RootList !CList
  | -- | A string's or a path's bytes.
    RootString !CStrPtr
  | -- | A string with context.
    RootCtxStr !CCtxStrPtr

-- | The C objects a value holds.  A value read from a cell is a view of
-- the cell's C objects; one held in GHC's heap behind a StablePtr (a
-- builtin and the arguments it has taken) holds them through its
-- arguments.
valueRoots :: NixValue -> [CRoot]
valueRoots value = case value of
  VInt _ -> []
  VFloat _ -> []
  VBool _ -> []
  VNull -> []
  VStrHeld _ _ held -> heldRoots held
  VPathHeld _ held -> heldRoots held
  VList list -> [RootList list]
  VAttrs set -> [RootAttrSet set]
  VLambda env _ _ -> [RootEnv env]
  VBuiltin _ args -> concatMap argumentRoots args
  where
    argumentRoots (ArgForced forced) = valueRoots forced
    argumentRoots (ArgDeferred thunk) = [RootThunk thunk]
    heldRoots NotHeld = []
    heldRoots (HeldStr str) = [RootString str]
    heldRoots (HeldCtxStr csptr) = [RootCtxStr csptr]

-- | Name a root to the open session.
nameRoot :: CRoot -> IO ()
nameRoot root = case root of
  RootThunk (Thunk ptr) -> c_nn_stats_root_thunk (castPtr ptr)
  RootEnv (Env ptr) -> c_nn_stats_root_env (castPtr ptr)
  RootAttrSet (AttrSet ptr) -> c_nn_stats_root_attrset (castPtr ptr)
  RootList (CList ptr) -> c_nn_stats_root_list (castPtr ptr)
  RootString str -> c_nn_stats_root_string (castPtr str)
  RootCtxStr csptr -> c_nn_stats_root_ctxstr (castPtr csptr)

-- ---------------------------------------------------------------------------
-- Taking a census
-- ---------------------------------------------------------------------------

-- | Count what the data layer holds and mark what the roots reach.  Runs
-- inside the arena's window, with nothing else using the data layer.
-- Every root's object exists when the count runs: the C objects a caller
-- can name are made by actions ("Nix.Eval.CData"), so forcing a root makes
-- none.
-- 'Left' when the C side cannot allocate the index or the mark's work
-- stack.
takeCensus :: [CRoot] -> IO (Either CStatusError Census)
takeCensus roots = do
  started <- getMonotonicTime
  begun <- c_nn_stats_begin
  if begun /= 0
    then pure (Left (CAllocationFailed "nn_stats_begin" "the census index could not be allocated"))
    else measure started `finally` c_nn_stats_end
  where
    measure started = do
      allocated <- readFigures c_nn_stats_allocated
      overhead <- c_nn_stats_overhead_bytes
      code <- c_nn_stats_code_bytes
      held <- c_nn_stats_stableptrs
      indexed <- getMonotonicTime
      marked <- markFrom roots
      finished <- getMonotonicTime
      reached <- readFigures c_nn_stats_reached
      (objects, bytes, unplaced, stablePtrs) <- readVisited
      sizes <- readSetSizes
      sites <- siteCounts
      pure $ case (marked, sizes) of
        (False, _) -> Left (CAllocationFailed "nn_stats_mark" "the mark's work stack could not grow")
        (_, Nothing) -> Left (CAllocationFailed "nn_stats_reached_set_sizes" "the set sizes could not be sorted")
        (True, Just setSizes) ->
          Right
            Census
              { censusAllocated = allocated,
                censusReached = reached,
                censusOverheadBytes = overhead,
                censusCodeBytes = code,
                censusStablePtrs = held,
                censusStablePtrsReached = stablePtrs,
                censusVisitedObjects = objects,
                censusVisitedBytes = bytes,
                censusUnplaced = unplaced,
                censusIndexSeconds = indexed - started,
                censusMarkSeconds = finished - indexed,
                censusSetSizes = setSizes,
                censusSites = sites
              }

-- | Mark from the roots, then from what each StablePtr-held value the
-- mark reaches holds, until a round reaches no new one.  'False' when the
-- C side ran out of memory.
markFrom :: [CRoot] -> IO Bool
markFrom roots = mapM_ nameRoot roots >> rounds
  where
    rounds = do
      status <- c_nn_stats_mark
      pending <- c_nn_stats_pending_stableptrs
      case (status, pending) of
        (0, 0) -> pure True
        (0, _) -> do
          values <- takeStablePtrValues pending
          mapM_ nameRoot (concatMap valueRoots values)
          rounds
        _ -> pure False

-- | The values behind the StablePtrs the mark has reached and not yet
-- handed over.
takeStablePtrValues :: Word32 -> IO [NixValue]
takeStablePtrValues pending = allocaArray (fromIntegral pending) $ \buf -> do
  taken <- c_nn_stats_take_stableptrs buf pending
  ptrs <- peekArray (fromIntegral taken) buf
  mapM (deRefStablePtr . castPtrToStablePtr) ptrs

-- | Figures by kind from a C function that fills a count and a byte
-- array.
readFigures :: (Ptr Word64 -> Ptr Word64 -> IO ()) -> IO (PerKind Figures)
readFigures fill =
  allocaArray kindCount $ \counts -> allocaArray kindCount $ \bytes -> do
    fill counts bytes
    traverse (\i -> Figures . countOf <$> peekElemOff counts i <*> peekElemOff bytes i) kindIndices
  where
    countOf n
      | n == uncounted = Nothing
      | otherwise = Just n

-- | The mark's objects, bytes, unplaced pointers and StablePtrs reached.
readVisited :: IO (Word64, Word64, Word64, Word64)
readVisited =
  alloca $ \objects -> alloca $ \bytes -> alloca $ \unplaced -> alloca $ \stablePtrs -> do
    c_nn_stats_visited objects bytes unplaced stablePtrs
    (,,,) <$> peek objects <*> peek bytes <*> peek unplaced <*> peek stablePtrs

-- | The reached sets by size, or 'Nothing' when C cannot sort them.  The
-- first call asks how many sizes there are, the second fetches them.
readSetSizes :: IO (Maybe (Map Word32 Word64))
readSetSizes = alloca $ \distinctPtr -> do
  first <- c_nn_stats_reached_set_sizes nullPtr nullPtr 0 distinctPtr
  distinct <- peek distinctPtr
  if first /= 0
    then pure Nothing
    else allocaArray (fromIntegral distinct) $ \sizes -> allocaArray (fromIntegral distinct) $ \counts -> do
      status <- c_nn_stats_reached_set_sizes sizes counts distinct distinctPtr
      if status /= 0
        then pure Nothing
        else do
          sizeList <- peekArray (fromIntegral distinct) sizes
          countList <- peekArray (fromIntegral distinct) counts
          pure (Just (Map.fromDistinctAscList (zip sizeList countList)))

-- ---------------------------------------------------------------------------
-- Report
-- ---------------------------------------------------------------------------

-- | A census as lines of a name and a value, each set size a line of its
-- own with the size and the number of reached sets that have it.  A
-- count no allocator keeps has no line.  With counting by site on, the
-- environments' slots and sizes follow, the open size written @N+@, and
-- then each site's figures, environments first.
renderCensus :: Census -> BS.ByteString
renderCensus census =
  BL.toStrict . BB.toLazyByteString $
    mconcat (toList (kindLines <$> kindNames <*> censusAllocated census <*> censusReached census))
      <> field "overhead.bytes" (BB.word64Dec (censusOverheadBytes census))
      <> field "code.bytes" (BB.word64Dec (censusCodeBytes census))
      <> field "stableptrs.held" (BB.word64Dec (censusStablePtrs census))
      <> field "stableptrs.reached" (BB.word64Dec (censusStablePtrsReached census))
      <> field "mark.objects" (BB.word64Dec (censusVisitedObjects census))
      <> field "mark.bytes" (BB.word64Dec (censusVisitedBytes census))
      <> field "mark.unplaced" (BB.word64Dec (censusUnplaced census))
      <> field "mark.index-seconds" (seconds (censusIndexSeconds census))
      <> field "mark.seconds" (seconds (censusMarkSeconds census))
      <> foldMap setSize (Map.toAscList (censusSetSizes census))
      <> foldMap siteLines (censusSites census)
  where
    kindLines name allocated reached =
      figureLines (name <> ".allocated") allocated <> figureLines (name <> ".reached") reached
    figureLines prefix (Figures count bytes) =
      foldMap (field (prefix <> ".count") . BB.word64Dec) count
        <> field (prefix <> ".bytes") (BB.word64Dec bytes)
    field name value = BB.byteString name <> BB.char7 ' ' <> value <> BB.char7 '\n'
    seconds s = BB.string7 (showFFloat (Just secondsDigits) s "")
    setSize (size, count) =
      field "attrsets.reached.size" (BB.word32Dec size <> BB.char7 ' ' <> BB.word64Dec count)
    siteLines (SiteCounts envs sizes sets) =
      field "envs.allocated.slots" (BB.word64Dec (sum (fmap envSiteSlots envs)))
        <> foldMap envSize (Map.toAscList sizes)
        <> foldMap envSite (Map.toAscList envs)
        <> foldMap setSite (Map.toAscList sets)
    envSize (size, count) =
      field "envs.allocated.size" (sizeName size <> BB.char7 ' ' <> BB.word64Dec count)
    sizeName (EnvSlots n) = BB.word32Dec n
    sizeName (EnvSlotsAtLeast n) = BB.word32Dec n <> BB.char7 '+'
    envSite (site, EnvSiteFigures envs slots bytes) =
      let prefix = "envs.site." <> envSiteName site
       in field (prefix <> ".count") (BB.word64Dec envs)
            <> field (prefix <> ".slots") (BB.word64Dec slots)
            <> field (prefix <> ".bytes") (BB.word64Dec bytes)
    setSite (site, SetSiteFigures sets shareable bytes) =
      let prefix = "attrsets.site." <> setSiteName site
       in field (prefix <> ".count") (BB.word64Dec sets)
            <> field (prefix <> ".shareable") (BB.word64Dec shareable)
            <> field (prefix <> ".bytes") (BB.word64Dec bytes)

-- | Decimal places of a time in 'renderCensus': microseconds.
secondsDigits :: Int
secondsDigits = 6
