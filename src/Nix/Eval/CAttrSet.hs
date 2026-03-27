-- | C-backed sorted attribute set via FFI.
--
-- Wraps @cbits/nn_attrset.c@ — a contiguous sorted array of
-- @(nn_symbol_t, void*)@ pairs.  Replaces Haskell's @Data.Map.Strict@
-- for attribute sets, cutting per-entry overhead from ~48 bytes
-- (Map.Bin node) to ~12 bytes (4-byte symbol + 8-byte pointer).
--
-- Construction is two-phase: insert entries, then freeze (sort + dedup).
-- After freeze, lookup is O(log n) binary search over contiguous memory.
--
-- Values are opaque 'StablePtr' handles — the C side never dereferences
-- them.  Haskell is responsible for managing 'StablePtr' lifetimes.
module Nix.Eval.CAttrSet
  ( -- * Opaque handle
    CAttrSet,

    -- * Lifecycle
    cattrsetNew,
    cattrsetFree,
    withCAttrSet,

    -- * Construction
    cattrsetInsert,
    cattrsetFreeze,

    -- * Query
    cattrsetLookup,
    cattrsetIndex,
    cattrsetGetValue,
    cattrsetSetValue,
    cattrsetGetKey,

    -- * Bulk access
    cattrsetSize,
    cattrsetKeys,

    -- * Set operations
    cattrsetIntersect,
    cattrsetUnion,
    cattrsetRemoveKeys,
  )
where

import Data.Word (Word32)
import Foreign.C.Types (CInt (..))
import Foreign.Marshal.Array (peekArray, withArray)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.StablePtr (StablePtr, castPtrToStablePtr, castStablePtrToPtr)
import Nix.Eval.Symbol (Symbol (..))

-- | Opaque handle to a C-allocated attribute set.
-- The Haskell side holds a raw pointer; the C side owns the memory.
data NnAttrSet

-- | Convenience alias.
type CAttrSet = Ptr NnAttrSet

-- ---------------------------------------------------------------------------
-- FFI imports (all unsafe — no callbacks, fast data access)
-- ---------------------------------------------------------------------------

foreign import ccall unsafe "nn_attrset_new"
  c_nn_attrset_new :: Word32 -> IO CAttrSet

foreign import ccall unsafe "nn_attrset_free"
  c_nn_attrset_free :: CAttrSet -> IO ()

foreign import ccall unsafe "nn_attrset_insert"
  c_nn_attrset_insert :: CAttrSet -> Word32 -> Ptr () -> IO ()

foreign import ccall unsafe "nn_attrset_freeze"
  c_nn_attrset_freeze :: CAttrSet -> IO ()

foreign import ccall unsafe "nn_attrset_lookup"
  c_nn_attrset_lookup :: CAttrSet -> Word32 -> IO (Ptr ())

foreign import ccall unsafe "nn_attrset_index"
  c_nn_attrset_index :: CAttrSet -> Word32 -> IO CInt

foreign import ccall unsafe "nn_attrset_set_value"
  c_nn_attrset_set_value :: CAttrSet -> Word32 -> Ptr () -> IO ()

foreign import ccall unsafe "nn_attrset_get_value"
  c_nn_attrset_get_value :: CAttrSet -> Word32 -> IO (Ptr ())

foreign import ccall unsafe "nn_attrset_get_key"
  c_nn_attrset_get_key :: CAttrSet -> Word32 -> IO Word32

foreign import ccall unsafe "nn_attrset_size"
  c_nn_attrset_size :: CAttrSet -> IO Word32

foreign import ccall unsafe "nn_attrset_keys_ptr"
  c_nn_attrset_keys_ptr :: CAttrSet -> IO (Ptr Word32)

foreign import ccall unsafe "nn_attrset_intersect"
  c_nn_attrset_intersect :: CAttrSet -> CAttrSet -> IO CAttrSet

foreign import ccall unsafe "nn_attrset_union"
  c_nn_attrset_union :: CAttrSet -> CAttrSet -> IO CAttrSet

foreign import ccall unsafe "nn_attrset_remove_keys"
  c_nn_attrset_remove_keys :: CAttrSet -> Ptr Word32 -> Word32 -> IO CAttrSet

-- ---------------------------------------------------------------------------
-- Lifecycle
-- ---------------------------------------------------------------------------

-- | Allocate a new empty attribute set with the given capacity hint.
cattrsetNew :: Word32 -> IO CAttrSet
cattrsetNew = c_nn_attrset_new

-- | Free a C-allocated attribute set.  Does NOT free StablePtr values.
cattrsetFree :: CAttrSet -> IO ()
cattrsetFree = c_nn_attrset_free

-- | Bracket pattern: allocate, use, free.
withCAttrSet :: Word32 -> (CAttrSet -> IO a) -> IO a
withCAttrSet cap action = do
  set <- cattrsetNew cap
  result <- action set
  cattrsetFree set
  pure result

-- ---------------------------------------------------------------------------
-- Construction
-- ---------------------------------------------------------------------------

-- | Insert a key-value pair.  Call before freeze.
-- The value is a StablePtr cast to Ptr () — C never dereferences it.
cattrsetInsert :: CAttrSet -> Symbol -> StablePtr a -> IO ()
cattrsetInsert set (Symbol sym) val =
  c_nn_attrset_insert set sym (stablePtrToVoid val)

-- | Sort and deduplicate.  Must be called once before any queries.
cattrsetFreeze :: CAttrSet -> IO ()
cattrsetFreeze = c_nn_attrset_freeze

-- ---------------------------------------------------------------------------
-- Query
-- ---------------------------------------------------------------------------

-- | Look up a key.  Returns 'Nothing' if not found.
-- The returned 'StablePtr' is borrowed — do not free it.
cattrsetLookup :: CAttrSet -> Symbol -> IO (Maybe (StablePtr a))
cattrsetLookup set (Symbol sym) = do
  ptr <- c_nn_attrset_lookup set sym
  pure (if ptr == nullPtr then Nothing else Just (voidToStablePtr ptr))

-- | Find the index of a key, or 'Nothing' if not found.
cattrsetIndex :: CAttrSet -> Symbol -> IO (Maybe Word32)
cattrsetIndex set (Symbol sym) = do
  idx <- c_nn_attrset_index set sym
  pure (if idx < 0 then Nothing else Just (fromIntegral idx))

-- | Get the value at a known index.
cattrsetGetValue :: CAttrSet -> Word32 -> IO (StablePtr a)
cattrsetGetValue set idx = voidToStablePtr <$> c_nn_attrset_get_value set idx

-- | Update the value at a known index (for lazy materialization).
cattrsetSetValue :: CAttrSet -> Word32 -> StablePtr a -> IO ()
cattrsetSetValue set idx val = c_nn_attrset_set_value set idx (stablePtrToVoid val)

-- | Get the key symbol at a known index.
cattrsetGetKey :: CAttrSet -> Word32 -> IO Symbol
cattrsetGetKey set idx = Symbol <$> c_nn_attrset_get_key set idx

-- ---------------------------------------------------------------------------
-- Bulk access
-- ---------------------------------------------------------------------------

-- | Number of entries (unique keys after freeze).
cattrsetSize :: CAttrSet -> IO Word32
cattrsetSize = c_nn_attrset_size

-- | Read all keys as a list of symbols (sorted).
cattrsetKeys :: CAttrSet -> IO [Symbol]
cattrsetKeys set = do
  n <- cattrsetSize set
  ptr <- c_nn_attrset_keys_ptr set
  raw <- peekArray (fromIntegral n) ptr
  pure (map Symbol raw)

-- ---------------------------------------------------------------------------
-- Set operations
-- ---------------------------------------------------------------------------

-- | Intersection: keys in both, values from second.  Result is frozen.
cattrsetIntersect :: CAttrSet -> CAttrSet -> IO CAttrSet
cattrsetIntersect = c_nn_attrset_intersect

-- | Right-biased union (// semantics).  Result is frozen.
cattrsetUnion :: CAttrSet -> CAttrSet -> IO CAttrSet
cattrsetUnion = c_nn_attrset_union

-- | Remove a list of keys.  Result is frozen.
cattrsetRemoveKeys :: CAttrSet -> [Symbol] -> IO CAttrSet
cattrsetRemoveKeys set syms = do
  let raw = map unSymbol syms
      n = fromIntegral (length raw)
  withArray raw $ \ptr ->
    c_nn_attrset_remove_keys set ptr n

-- ---------------------------------------------------------------------------
-- StablePtr <-> Ptr () casting
-- ---------------------------------------------------------------------------

stablePtrToVoid :: StablePtr a -> Ptr ()
stablePtrToVoid = castPtr . castStablePtrToPtr

voidToStablePtr :: Ptr () -> StablePtr a
voidToStablePtr = castPtrToStablePtr . castPtr
