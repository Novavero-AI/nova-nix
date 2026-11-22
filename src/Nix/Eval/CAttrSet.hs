-- | C-backed sorted attribute set via FFI.
--
-- Wraps @cbits/nn_attrset.c@: parallel arrays of symbols, positions and
-- values, sorted by symbol, at about 16 bytes an entry (a 4-byte symbol,
-- a 4-byte position and an 8-byte pointer) against the 48 of a
-- @Data.Map.Strict@ node.
--
-- Construction is two-phase: insert entries, then freeze (sort + dedup).
-- After freeze, lookup is O(log n) binary search over contiguous memory.
--
-- A set is read by name ('cattrsetLookup', 'cattrsetFind',
-- 'cattrsetMember') or walked in symbol order ('cattrsetWalk').  Only a
-- name table ('cattrsetFromNames'), a t'FlatCAttrSet', is read and
-- written by index into its arrays.
--
-- Values are 'CThunkPtr' cells stored as @void*@ on the C side.  The set
-- never dereferences them, and the census ("Nix.Eval.CStats") follows
-- them.  The thunk arena owns the cells, so freeing a set frees its own
-- arrays alone.
--
-- Every constructor names its 'SetSite', under which the census counts
-- the set.  'cattrsetEmpty' is the one set no constructor makes: the
-- shared set with no entries.
--
-- A set holds at most 'maxBound' :: 'Word32' entries, upstream's own
-- ceiling (its @Bindings@ counts in @uint32_t@).  Every constructor and
-- insert checks its C result, so a set the C side cannot build raises
-- 'Nix.Eval.CStatus.CStatusError' instead of leaving a NULL or a short
-- set behind.
module Nix.Eval.CAttrSet
  ( -- * Opaque handles
    CAttrSet,
    FlatCAttrSet,
    flatCAttrSet,

    -- * Lifecycle
    cattrsetNew,
    cattrsetEmpty,
    cattrsetFromNames,

    -- * Construction
    cattrsetInsert,
    cattrsetFreeze,

    -- * Query
    cattrsetLookup,
    cattrsetFind,
    cattrsetMember,

    -- * Walks
    CAttrSetWalk,
    CAttrSetStep (..),
    cattrsetWalk,
    cattrsetWalkNext,

    -- * Name tables by index
    cattrsetIndex,
    cattrsetSetValue,

    -- * Bulk access
    cattrsetSize,

    -- * Set operations
    cattrsetUnion,
    cattrsetUpdate,
    cattrsetRemoveKeys,
  )
where

import Control.Monad (forM_, unless)
import Data.Word (Word32)
import Foreign.C.Types (CInt (..))
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Marshal.Array (withArray)
import Foreign.Ptr (Ptr, castPtr, nullPtr, plusPtr)
import Foreign.Storable (alignment, peek, sizeOf)
import Nix.Eval.AllocSite (SetSite, setSiteIndex)
import Nix.Eval.CData (MonadCData, cdataIO)
import Nix.Eval.CStatus (cStatusFailure, checkedCPtr)
import Nix.Eval.CThunk (CThunkPtr)
import Nix.Eval.SourcePos (PosIdx (..), noPos)
import Nix.Eval.Symbol (Symbol (..))

-- | Opaque handle to a C-allocated attribute set.
-- The Haskell side holds a raw pointer; the C side owns the memory.
data NnAttrSet

-- | Convenience alias.
type CAttrSet = Ptr NnAttrSet

-- | A frozen set whose own arrays hold every entry it binds, sorted by
-- symbol, so that an index into them names one of its entries: the set
-- 'cattrsetIndex' and 'cattrsetSetValue' take.  The constructor is not
-- exported, so every one is a name table 'cattrsetFromNames' made.
newtype FlatCAttrSet = FlatCAttrSet CAttrSet

-- | The set a t'FlatCAttrSet' is.
flatCAttrSet :: FlatCAttrSet -> CAttrSet
flatCAttrSet (FlatCAttrSet set) = set

-- ---------------------------------------------------------------------------
-- FFI imports (all unsafe - no callbacks, fast data access)
-- ---------------------------------------------------------------------------

foreign import ccall unsafe "nn_attrset_new"
  c_nn_attrset_new :: Word32 -> Word32 -> IO CAttrSet

-- The empty set is a C static, one address for the life of the process,
-- so this import is pure, as the empty env of "Nix.Eval.CEnv" is: no
-- arena window, nor its absence, changes what it returns.

-- | The shared set with no entries (@nn_attrset_empty@): frozen, and
-- refused by every write.
foreign import ccall unsafe "nn_attrset_empty"
  cattrsetEmpty :: CAttrSet

foreign import ccall unsafe "nn_attrset_insert"
  c_nn_attrset_insert :: CAttrSet -> Word32 -> Ptr () -> Word32 -> IO CInt

foreign import ccall unsafe "nn_attrset_freeze"
  c_nn_attrset_freeze :: CAttrSet -> IO ()

foreign import ccall unsafe "nn_attrset_lookup"
  c_nn_attrset_lookup :: CAttrSet -> Word32 -> IO (Ptr ())

foreign import ccall unsafe "nn_attrset_find"
  c_nn_attrset_find :: CAttrSet -> Word32 -> Ptr Word32 -> Ptr (Ptr ()) -> IO CInt

foreign import ccall unsafe "nn_attrset_index"
  c_nn_attrset_index :: CAttrSet -> Word32 -> IO Word32

foreign import ccall unsafe "nn_attrset_set_value"
  c_nn_attrset_set_value :: CAttrSet -> Word32 -> Ptr () -> IO ()

foreign import ccall unsafe "nn_attrset_get_value"
  c_nn_attrset_get_value :: CAttrSet -> Word32 -> IO (Ptr ())

foreign import ccall unsafe "nn_attrset_get_key"
  c_nn_attrset_get_key :: CAttrSet -> Word32 -> IO Word32

foreign import ccall unsafe "nn_attrset_get_pos"
  c_nn_attrset_get_pos :: CAttrSet -> Word32 -> IO Word32

foreign import ccall unsafe "nn_attrset_size"
  c_nn_attrset_size :: CAttrSet -> IO Word32

foreign import ccall unsafe "nn_attrset_layer_count"
  c_nn_attrset_layer_count :: CAttrSet -> IO Word32

foreign import ccall unsafe "nn_attrset_union"
  c_nn_attrset_union :: CAttrSet -> CAttrSet -> Word32 -> IO CAttrSet

foreign import ccall unsafe "nn_attrset_update"
  c_nn_attrset_update :: CAttrSet -> CAttrSet -> Word32 -> IO CAttrSet

foreign import ccall unsafe "nn_attrset_remove_keys"
  c_nn_attrset_remove_keys :: CAttrSet -> Ptr Word32 -> Word32 -> Word32 -> IO CAttrSet

-- | @NN_ATTRSET_NOT_FOUND@, the index answer for an absent key.  Must
-- stay in lockstep with @cbits\/nn_attrset.h@.
indexNotFound :: Word32
indexNotFound = maxBound

-- ---------------------------------------------------------------------------
-- Lifecycle
-- ---------------------------------------------------------------------------

-- | Allocate a new empty attribute set at a site, sized for the expected
-- number of entries.  The count only sizes the first allocation, so one
-- past the ceiling is clamped to it: the inserts are what enforce the
-- ceiling.
cattrsetNew :: (MonadCData m) => SetSite -> Int -> m CAttrSet
cattrsetNew site expected = cdataIO (checkedCPtr "nn_attrset_new" =<< c_nn_attrset_new capacity (setSiteIndex site))
  where
    capacity = fromIntegral (max 0 (min expected entryCeiling))
    entryCeiling = fromIntegral (maxBound :: Word32)

-- | A frozen set at a site binding each name to no value, a null
-- pointer, at no position: a table whose values are written afterwards,
-- by index ('cattrsetSetValue').  No names give the shared empty set,
-- which holds no index to write at.
cattrsetFromNames :: (MonadCData m) => SetSite -> [Symbol] -> m FlatCAttrSet
cattrsetFromNames _ [] = pure (FlatCAttrSet cattrsetEmpty)
cattrsetFromNames site names = do
  set <- cattrsetNew site (length names)
  forM_ names $ \name -> cattrsetInsert set name noPos nullPtr
  cattrsetFreeze set
  pure (FlatCAttrSet set)

-- ---------------------------------------------------------------------------
-- Construction
-- ---------------------------------------------------------------------------

-- | Insert a key-value pair defined at a position, into a set not yet
-- frozen: a frozen one aborts in C.
-- The value is a 'CThunkPtr' cast to @Ptr ()@: the set never dereferences
-- it, and the census follows it.
cattrsetInsert :: (MonadCData m) => CAttrSet -> Symbol -> PosIdx -> CThunkPtr -> m ()
cattrsetInsert set (Symbol sym) (PosIdx pos) ptr = cdataIO $ do
  status <- c_nn_attrset_insert set sym (castPtr ptr) pos
  unless (status == 0) $
    cStatusFailure "nn_attrset_insert" "the attribute set cannot grow (allocation failure or its 4294967295-entry ceiling)"

-- | Sort the entries and keep the last insert of a repeated key, which a
-- lookup needs.  Freezing a frozen set does nothing.
cattrsetFreeze :: (MonadCData m) => CAttrSet -> m ()
cattrsetFreeze = cdataIO . c_nn_attrset_freeze

-- ---------------------------------------------------------------------------
-- Query
-- ---------------------------------------------------------------------------

-- | Look up a key.  Returns 'Nothing' if not found.
-- The returned 'CThunkPtr' is borrowed - the thunk arena owns it.
cattrsetLookup :: (MonadCData m) => CAttrSet -> Symbol -> m (Maybe CThunkPtr)
cattrsetLookup set (Symbol sym) = cdataIO $ do
  ptr <- c_nn_attrset_lookup set sym
  pure (if ptr == nullPtr then Nothing else Just (castPtr ptr))

-- | The position a set binds a name at and the value it binds it to, or
-- 'Nothing' if it binds no such name.  A value not filled yet, the null
-- pointer a name table holds until its frame fills it, is found as such,
-- where 'cattrsetLookup' answers 'Nothing'.
cattrsetFind :: (MonadCData m) => CAttrSet -> Symbol -> m (Maybe (PosIdx, CThunkPtr))
cattrsetFind set (Symbol sym) = cdataIO $
  allocaBytesAligned findOutputBytes findOutputAlignment $ \outputs -> do
    let valuePtr = castPtr outputs
        posPtr = outputs `plusPtr` findPositionOffset
    found <- c_nn_attrset_find set sym posPtr valuePtr
    if found == 0
      then pure Nothing
      else (\pos value -> Just (PosIdx pos, castPtr value)) <$> peek posPtr <*> peek valuePtr

-- | One buffer holds both of @nn_attrset_find@'s outputs, so a find
-- allocates once: the value's pointer at its start, aligned for a
-- pointer, and the position right after it, at an offset of a pointer's
-- size, which its own alignment divides.
findPositionOffset, findOutputBytes, findOutputAlignment :: Int
findPositionOffset = sizeOf (nullPtr :: Ptr ())
findOutputBytes = findPositionOffset + sizeOf (0 :: Word32)
findOutputAlignment = alignment (nullPtr :: Ptr ())

-- | Whether a set binds a name, one whose value is not filled yet among
-- them, as 'cattrsetFind' finds it, with nothing read back.
cattrsetMember :: (MonadCData m) => CAttrSet -> Symbol -> m Bool
cattrsetMember set (Symbol sym) = cdataIO ((/= 0) <$> c_nn_attrset_find set sym nullPtr nullPtr)

-- ---------------------------------------------------------------------------
-- Walks
-- ---------------------------------------------------------------------------

-- | A walk over a set's entries in symbol order, at the entry it reads
-- next: the set, that entry's index into the set's arrays, and how many
-- entries they hold.
data CAttrSetWalk = CAttrSetWalk !CAttrSet !Word32 !Word32

-- | What a walk reads next.
data CAttrSetStep
  = -- | The end of the set.
    CAttrSetDone
  | -- | An entry, its name, the position it was defined at and its value,
    -- with the walk past it.
    CAttrSetEntry !Symbol !PosIdx !CThunkPtr !CAttrSetWalk

-- | A walk from a set's first entry, which reads the set's arrays in
-- place, one entry at a time.  A frozen set's entries never move, so
-- whatever runs between two steps, the walk reads each entry once.
{-# INLINE cattrsetWalk #-}
cattrsetWalk :: (MonadCData m) => CAttrSet -> m CAttrSetWalk
cattrsetWalk set = cdataIO (CAttrSetWalk set 0 <$> c_nn_attrset_layer_count set)

-- | The entry a walk reads next, or 'CAttrSetDone' past the last.
{-# INLINE cattrsetWalkNext #-}
cattrsetWalkNext :: (MonadCData m) => CAttrSetWalk -> m CAttrSetStep
cattrsetWalkNext (CAttrSetWalk set idx count)
  | idx >= count = pure CAttrSetDone
  | otherwise = cdataIO $ do
      name <- c_nn_attrset_get_key set idx
      pos <- c_nn_attrset_get_pos set idx
      value <- c_nn_attrset_get_value set idx
      pure (CAttrSetEntry (Symbol name) (PosIdx pos) (castPtr value) (CAttrSetWalk set (idx + 1) count))

-- ---------------------------------------------------------------------------
-- Name tables by index
-- ---------------------------------------------------------------------------

-- | Find the index of a key, or 'Nothing' if not found.
cattrsetIndex :: (MonadCData m) => FlatCAttrSet -> Symbol -> m (Maybe Word32)
cattrsetIndex (FlatCAttrSet set) (Symbol sym) = cdataIO $ do
  idx <- c_nn_attrset_index set sym
  pure (if idx == indexNotFound then Nothing else Just idx)

-- | Update the value at a known index (for two-phase construction).
cattrsetSetValue :: (MonadCData m) => FlatCAttrSet -> Word32 -> CThunkPtr -> m ()
cattrsetSetValue (FlatCAttrSet set) idx ptr = cdataIO (c_nn_attrset_set_value set idx (castPtr ptr))

-- ---------------------------------------------------------------------------
-- Bulk access
-- ---------------------------------------------------------------------------

-- | Number of entries (unique keys after freeze).
cattrsetSize :: (MonadCData m) => CAttrSet -> m Word32
cattrsetSize = cdataIO . c_nn_attrset_size

-- ---------------------------------------------------------------------------
-- Set operations
-- ---------------------------------------------------------------------------

-- | Right-biased union (// semantics) at a site.  Result is frozen.
cattrsetUnion :: (MonadCData m) => SetSite -> CAttrSet -> CAttrSet -> m CAttrSet
cattrsetUnion site a b = cdataIO (checkedCPtr "nn_attrset_union" =<< c_nn_attrset_union a b (setSiteIndex site))

-- | @a // b@ at a site: the set 'cattrsetUnion' makes, whose entries the
-- census counts as copied while it counts sites ("Nix.Eval.CStats"), as
-- upstream counts them in @nrOpUpdateValuesCopied@.  Result is frozen.
cattrsetUpdate :: (MonadCData m) => SetSite -> CAttrSet -> CAttrSet -> m CAttrSet
cattrsetUpdate site a b = cdataIO (checkedCPtr "nn_attrset_update" =<< c_nn_attrset_update a b (setSiteIndex site))

-- | Remove a list of keys, making the result at a site.  Result is frozen.
cattrsetRemoveKeys :: (MonadCData m) => SetSite -> CAttrSet -> [Symbol] -> m CAttrSet
cattrsetRemoveKeys site set syms = cdataIO $ do
  let raw = map unSymbol syms
      n = fromIntegral (length raw)
  withArray raw $ \ptr ->
    checkedCPtr "nn_attrset_remove_keys" =<< c_nn_attrset_remove_keys set ptr n (setSiteIndex site)
