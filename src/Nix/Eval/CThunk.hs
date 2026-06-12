-- | C-backed thunk memoization cells via FFI.
--
-- Wraps @cbits/nn_thunk.c@ - an arena-allocated mutable cell that
-- holds either a pending expression (StablePtr to (Expr, Env)) or a
-- computed value (StablePtr to NixValue).  Replaces Haskell's
-- @IORef ThunkCell@ (~56 bytes on GHC heap, all GC-traced) with a
-- 16-byte C struct in an arena (invisible to GHC GC).
--
-- Includes blackhole detection: a thunk being forced is marked
-- BLACKHOLE; if forced again before completion, infinite recursion
-- is detected (matching C++ Nix behavior).
--
-- @
-- cthunkInit 0
-- ptr <- cthunkNew pendingStablePtr
-- cthunkState ptr  -- 0 (PENDING)
-- cthunkMarkBlackhole ptr  -- 1 (success)
-- cthunkSetComputed ptr valueStablePtr  -- returns old pending ptr
-- cthunkState ptr  -- 1 (COMPUTED)
-- cthunkDestroy
-- @
module Nix.Eval.CThunk
  ( -- * Opaque handle
    NnThunk,
    CThunkPtr,

    -- * Lifecycle
    cthunkInit,
    cthunkDestroy,

    -- * Allocation
    cthunkNew,
    cthunkNewBc,
    cthunkNewComputed,
    cthunkNewComputedInt,
    cthunkNewComputedFloat,
    cthunkNewComputedBool,
    cthunkNewComputedNull,
    cthunkNewComputedStr,
    cthunkNewComputedPath,
    cthunkNewComputedList,
    cthunkNewComputedAttrs,
    cthunkNewComputedCtxStr,
    cthunkNewComputedLambda,

    -- * State queries
    cthunkState,
    cthunkPayload,
    cthunkValueTag,
    cthunkGetBcIdx,
    cthunkSetPayload,
    cthunkGetInt,
    cthunkGetFloat,
    cthunkGetBool,
    cthunkGetStr,
    cthunkGetPath,
    cthunkGetList,
    cthunkGetAttrs,
    cthunkGetCtxStr,
    cthunkGetLambda,

    -- * State transitions
    cthunkMarkBlackhole,
    cthunkSetComputed,
    cthunkSetComputedInt,
    cthunkSetComputedFloat,
    cthunkSetComputedBool,
    cthunkSetComputedNull,
    cthunkSetComputedStr,
    cthunkSetComputedPath,
    cthunkSetComputedList,
    cthunkSetComputedAttrs,
    cthunkSetComputedCtxStr,
    cthunkSetComputedLambda,

    -- * Arena diagnostics / cleanup
    cthunkCount,
    cthunkGet,
  )
where

import Data.Int (Int64)
import Data.Word (Word32, Word8)
import Foreign.C.Types (CDouble (..), CInt (..))
import Foreign.Ptr (Ptr, nullPtr)

-- | Phantom type for C-side @nn_thunk_t@.
data NnThunk

-- | Pointer to a C-allocated thunk (lives in arena).
type CThunkPtr = Ptr NnThunk

-- ---------------------------------------------------------------------------
-- FFI imports (all unsafe - no callbacks, fast data access)
-- ---------------------------------------------------------------------------

foreign import ccall unsafe "nn_thunk_init"
  c_nn_thunk_init :: Word32 -> IO ()

foreign import ccall unsafe "nn_thunk_destroy"
  c_nn_thunk_destroy :: IO ()

foreign import ccall unsafe "nn_thunk_new"
  c_nn_thunk_new :: Ptr () -> IO CThunkPtr

foreign import ccall unsafe "nn_thunk_new_bc"
  c_nn_thunk_new_bc :: Word32 -> Ptr () -> IO CThunkPtr

foreign import ccall unsafe "nn_thunk_get_bc_idx"
  c_nn_thunk_get_bc_idx :: CThunkPtr -> IO Word32

foreign import ccall unsafe "nn_thunk_set_payload"
  c_nn_thunk_set_payload :: CThunkPtr -> Ptr () -> IO ()

foreign import ccall unsafe "nn_thunk_new_computed"
  c_nn_thunk_new_computed :: Ptr () -> IO CThunkPtr

foreign import ccall unsafe "nn_thunk_new_computed_int"
  c_nn_thunk_new_computed_int :: Int64 -> IO CThunkPtr

foreign import ccall unsafe "nn_thunk_new_computed_float"
  c_nn_thunk_new_computed_float :: CDouble -> IO CThunkPtr

foreign import ccall unsafe "nn_thunk_new_computed_bool"
  c_nn_thunk_new_computed_bool :: Word8 -> IO CThunkPtr

foreign import ccall unsafe "nn_thunk_new_computed_null"
  c_nn_thunk_new_computed_null :: IO CThunkPtr

foreign import ccall unsafe "nn_thunk_new_computed_str"
  c_nn_thunk_new_computed_str :: Word32 -> IO CThunkPtr

foreign import ccall unsafe "nn_thunk_new_computed_path"
  c_nn_thunk_new_computed_path :: Word32 -> IO CThunkPtr

foreign import ccall unsafe "nn_thunk_new_computed_list"
  c_nn_thunk_new_computed_list :: Ptr () -> IO CThunkPtr

foreign import ccall unsafe "nn_thunk_new_computed_attrs"
  c_nn_thunk_new_computed_attrs :: Ptr () -> IO CThunkPtr

foreign import ccall unsafe "nn_thunk_new_computed_ctxstr"
  c_nn_thunk_new_computed_ctxstr :: Ptr () -> IO CThunkPtr

foreign import ccall unsafe "nn_thunk_new_computed_lambda"
  c_nn_thunk_new_computed_lambda :: Ptr () -> IO CThunkPtr

foreign import ccall unsafe "nn_thunk_state"
  c_nn_thunk_state :: CThunkPtr -> IO Word8

foreign import ccall unsafe "nn_thunk_payload"
  c_nn_thunk_payload :: CThunkPtr -> IO (Ptr ())

foreign import ccall unsafe "nn_thunk_value_tag"
  c_nn_thunk_value_tag :: CThunkPtr -> IO Word8

foreign import ccall unsafe "nn_thunk_get_int"
  c_nn_thunk_get_int :: CThunkPtr -> IO Int64

foreign import ccall unsafe "nn_thunk_get_float"
  c_nn_thunk_get_float :: CThunkPtr -> IO CDouble

foreign import ccall unsafe "nn_thunk_get_bool"
  c_nn_thunk_get_bool :: CThunkPtr -> IO Word8

foreign import ccall unsafe "nn_thunk_get_str"
  c_nn_thunk_get_str :: CThunkPtr -> IO Word32

foreign import ccall unsafe "nn_thunk_get_path"
  c_nn_thunk_get_path :: CThunkPtr -> IO Word32

foreign import ccall unsafe "nn_thunk_get_list"
  c_nn_thunk_get_list :: CThunkPtr -> IO (Ptr ())

foreign import ccall unsafe "nn_thunk_get_attrs"
  c_nn_thunk_get_attrs :: CThunkPtr -> IO (Ptr ())

foreign import ccall unsafe "nn_thunk_get_ctxstr"
  c_nn_thunk_get_ctxstr :: CThunkPtr -> IO (Ptr ())

foreign import ccall unsafe "nn_thunk_get_lambda"
  c_nn_thunk_get_lambda :: CThunkPtr -> IO (Ptr ())

foreign import ccall unsafe "nn_thunk_mark_blackhole"
  c_nn_thunk_mark_blackhole :: CThunkPtr -> IO CInt

foreign import ccall unsafe "nn_thunk_set_computed"
  c_nn_thunk_set_computed :: CThunkPtr -> Ptr () -> IO (Ptr ())

foreign import ccall unsafe "nn_thunk_set_computed_int"
  c_nn_thunk_set_computed_int :: CThunkPtr -> Int64 -> IO (Ptr ())

foreign import ccall unsafe "nn_thunk_set_computed_float"
  c_nn_thunk_set_computed_float :: CThunkPtr -> CDouble -> IO (Ptr ())

foreign import ccall unsafe "nn_thunk_set_computed_bool"
  c_nn_thunk_set_computed_bool :: CThunkPtr -> Word8 -> IO (Ptr ())

foreign import ccall unsafe "nn_thunk_set_computed_null"
  c_nn_thunk_set_computed_null :: CThunkPtr -> IO (Ptr ())

foreign import ccall unsafe "nn_thunk_set_computed_str"
  c_nn_thunk_set_computed_str :: CThunkPtr -> Word32 -> IO (Ptr ())

foreign import ccall unsafe "nn_thunk_set_computed_path"
  c_nn_thunk_set_computed_path :: CThunkPtr -> Word32 -> IO (Ptr ())

foreign import ccall unsafe "nn_thunk_set_computed_list"
  c_nn_thunk_set_computed_list :: CThunkPtr -> Ptr () -> IO (Ptr ())

foreign import ccall unsafe "nn_thunk_set_computed_attrs"
  c_nn_thunk_set_computed_attrs :: CThunkPtr -> Ptr () -> IO (Ptr ())

foreign import ccall unsafe "nn_thunk_set_computed_ctxstr"
  c_nn_thunk_set_computed_ctxstr :: CThunkPtr -> Ptr () -> IO (Ptr ())

foreign import ccall unsafe "nn_thunk_set_computed_lambda"
  c_nn_thunk_set_computed_lambda :: CThunkPtr -> Ptr () -> IO (Ptr ())

foreign import ccall unsafe "nn_thunk_count"
  c_nn_thunk_count :: IO Word32

foreign import ccall unsafe "nn_thunk_get"
  c_nn_thunk_get :: Word32 -> IO CThunkPtr

-- ---------------------------------------------------------------------------
-- Lifecycle
-- ---------------------------------------------------------------------------

-- | Initialize the global thunk arena.  Call once before evaluation.
-- @capacity@ is a hint for thunks per block (0 uses default: 65536).
cthunkInit :: Word32 -> IO ()
cthunkInit = c_nn_thunk_init

-- | Destroy the global thunk arena, freeing all C-side memory.
-- Caller must free all payload StablePtrs first.
-- All 'CThunkPtr' values become invalid after this call.
cthunkDestroy :: IO ()
cthunkDestroy = c_nn_thunk_destroy

-- ---------------------------------------------------------------------------
-- Allocation
-- ---------------------------------------------------------------------------

-- | Allocate a new PENDING thunk from the arena (legacy StablePtr path).
-- The payload is an opaque pointer (StablePtr cast to Ptr ()).
cthunkNew :: Ptr () -> IO CThunkPtr
cthunkNew = c_nn_thunk_new

-- | Allocate a new PENDING thunk with a bytecode index + C env pointer.
-- No Haskell heap references - zero GC pressure for pending thunks.
cthunkNewBc :: Word32 -> Ptr () -> IO CThunkPtr
cthunkNewBc = c_nn_thunk_new_bc

-- | Read the bytecode index from a PENDING thunk.
cthunkGetBcIdx :: CThunkPtr -> IO Word32
cthunkGetBcIdx = c_nn_thunk_get_bc_idx

-- | Set the payload of a thunk (deferred env fixup for knot-tying).
cthunkSetPayload :: CThunkPtr -> Ptr () -> IO ()
cthunkSetPayload = c_nn_thunk_set_payload

-- | Allocate a new pre-COMPUTED thunk with StablePtr payload (complex types).
cthunkNewComputed :: Ptr () -> IO CThunkPtr
cthunkNewComputed = c_nn_thunk_new_computed

-- | Allocate a pre-COMPUTED thunk with an inline int64 (no StablePtr).
cthunkNewComputedInt :: Int64 -> IO CThunkPtr
cthunkNewComputedInt = c_nn_thunk_new_computed_int

-- | Allocate a pre-COMPUTED thunk with an inline double (no StablePtr).
cthunkNewComputedFloat :: Double -> IO CThunkPtr
cthunkNewComputedFloat d = c_nn_thunk_new_computed_float (CDouble d)

-- | Allocate a pre-COMPUTED thunk with an inline bool (no StablePtr).
cthunkNewComputedBool :: Word8 -> IO CThunkPtr
cthunkNewComputedBool = c_nn_thunk_new_computed_bool

-- | Allocate a pre-COMPUTED thunk with null value (no StablePtr).
cthunkNewComputedNull :: IO CThunkPtr
cthunkNewComputedNull = c_nn_thunk_new_computed_null

-- | Allocate a pre-COMPUTED thunk with an interned string symbol (no StablePtr).
-- For context-free strings only (tag 4).
cthunkNewComputedStr :: Word32 -> IO CThunkPtr
cthunkNewComputedStr = c_nn_thunk_new_computed_str

-- | Allocate a pre-COMPUTED thunk with an interned path symbol (no StablePtr).
cthunkNewComputedPath :: Word32 -> IO CThunkPtr
cthunkNewComputedPath = c_nn_thunk_new_computed_path

-- | Allocate a pre-COMPUTED thunk with a CList pointer (no StablePtr).
cthunkNewComputedList :: Ptr () -> IO CThunkPtr
cthunkNewComputedList = c_nn_thunk_new_computed_list

-- | Allocate a pre-COMPUTED thunk with a CAttrSet pointer (no StablePtr).
cthunkNewComputedAttrs :: Ptr () -> IO CThunkPtr
cthunkNewComputedAttrs = c_nn_thunk_new_computed_attrs

-- | Allocate a pre-COMPUTED thunk with a CCtxStr pointer (no StablePtr).
-- For strings with non-empty context (tag 8).
cthunkNewComputedCtxStr :: Ptr () -> IO CThunkPtr
cthunkNewComputedCtxStr = c_nn_thunk_new_computed_ctxstr

-- | Allocate a pre-COMPUTED thunk with a CLambda pointer (no StablePtr).
-- For lambda closures (tag 9).
cthunkNewComputedLambda :: Ptr () -> IO CThunkPtr
cthunkNewComputedLambda = c_nn_thunk_new_computed_lambda

-- ---------------------------------------------------------------------------
-- State queries
-- ---------------------------------------------------------------------------

-- | Read the current state: 0 = PENDING, 1 = COMPUTED, 2 = BLACKHOLE.
cthunkState :: CThunkPtr -> IO Word8
cthunkState = c_nn_thunk_state

-- | Read the payload pointer.  Returns 'nullPtr' for NULL payloads.
cthunkPayload :: CThunkPtr -> IO (Ptr ())
cthunkPayload ptr = do
  p <- c_nn_thunk_payload ptr
  pure (if p == nullPtr then nullPtr else p)

-- | Read the value tag (valid when state == COMPUTED).
-- 0=INT, 1=FLOAT, 2=BOOL, 3=NULL, 255=PTR (StablePtr).
cthunkValueTag :: CThunkPtr -> IO Word8
cthunkValueTag = c_nn_thunk_value_tag

-- | Read an inline int64 from a COMPUTED thunk (val_tag == 0).
cthunkGetInt :: CThunkPtr -> IO Int64
cthunkGetInt = c_nn_thunk_get_int

-- | Read an inline double from a COMPUTED thunk (val_tag == 1).
cthunkGetFloat :: CThunkPtr -> IO Double
cthunkGetFloat ptr = do
  CDouble d <- c_nn_thunk_get_float ptr
  pure d

-- | Read an inline bool from a COMPUTED thunk (val_tag == 2).
cthunkGetBool :: CThunkPtr -> IO Word8
cthunkGetBool = c_nn_thunk_get_bool

-- | Read an interned string symbol from a COMPUTED thunk (val_tag == 4).
cthunkGetStr :: CThunkPtr -> IO Word32
cthunkGetStr = c_nn_thunk_get_str

-- | Read an interned path symbol from a COMPUTED thunk (val_tag == 5).
cthunkGetPath :: CThunkPtr -> IO Word32
cthunkGetPath = c_nn_thunk_get_path

-- | Read a CList pointer from a COMPUTED thunk (val_tag == 6).
cthunkGetList :: CThunkPtr -> IO (Ptr ())
cthunkGetList = c_nn_thunk_get_list

-- | Read a CAttrSet pointer from a COMPUTED thunk (val_tag == 7).
cthunkGetAttrs :: CThunkPtr -> IO (Ptr ())
cthunkGetAttrs = c_nn_thunk_get_attrs

-- | Read a CCtxStr pointer from a COMPUTED thunk (val_tag == 8).
cthunkGetCtxStr :: CThunkPtr -> IO (Ptr ())
cthunkGetCtxStr = c_nn_thunk_get_ctxstr

-- | Read a CLambda pointer from a COMPUTED thunk (val_tag == 9).
cthunkGetLambda :: CThunkPtr -> IO (Ptr ())
cthunkGetLambda = c_nn_thunk_get_lambda

-- ---------------------------------------------------------------------------
-- State transitions
-- ---------------------------------------------------------------------------

-- | Mark a PENDING thunk as BLACKHOLE (being evaluated).
-- Returns 'True' on success, 'False' if not PENDING.
cthunkMarkBlackhole :: CThunkPtr -> IO Bool
cthunkMarkBlackhole ptr = do
  result <- c_nn_thunk_mark_blackhole ptr
  pure (result /= 0)

-- | Set a BLACKHOLE thunk to COMPUTED with a StablePtr value (complex types).
-- Returns the old payload (pending StablePtr to free), or 'nullPtr'
-- if the thunk was not in BLACKHOLE state.
cthunkSetComputed :: CThunkPtr -> Ptr () -> IO (Ptr ())
cthunkSetComputed = c_nn_thunk_set_computed

-- | Set a BLACKHOLE thunk to COMPUTED with an inline int64 (no StablePtr).
cthunkSetComputedInt :: CThunkPtr -> Int64 -> IO (Ptr ())
cthunkSetComputedInt = c_nn_thunk_set_computed_int

-- | Set a BLACKHOLE thunk to COMPUTED with an inline double (no StablePtr).
cthunkSetComputedFloat :: CThunkPtr -> Double -> IO (Ptr ())
cthunkSetComputedFloat ptr d = c_nn_thunk_set_computed_float ptr (CDouble d)

-- | Set a BLACKHOLE thunk to COMPUTED with an inline bool (no StablePtr).
cthunkSetComputedBool :: CThunkPtr -> Word8 -> IO (Ptr ())
cthunkSetComputedBool = c_nn_thunk_set_computed_bool

-- | Set a BLACKHOLE thunk to COMPUTED with null value (no StablePtr).
cthunkSetComputedNull :: CThunkPtr -> IO (Ptr ())
cthunkSetComputedNull = c_nn_thunk_set_computed_null

-- | Set a non-COMPUTED thunk to COMPUTED with an interned string symbol.
cthunkSetComputedStr :: CThunkPtr -> Word32 -> IO (Ptr ())
cthunkSetComputedStr = c_nn_thunk_set_computed_str

-- | Set a non-COMPUTED thunk to COMPUTED with an interned path symbol.
cthunkSetComputedPath :: CThunkPtr -> Word32 -> IO (Ptr ())
cthunkSetComputedPath = c_nn_thunk_set_computed_path

-- | Set a non-COMPUTED thunk to COMPUTED with a CList pointer.
cthunkSetComputedList :: CThunkPtr -> Ptr () -> IO (Ptr ())
cthunkSetComputedList = c_nn_thunk_set_computed_list

-- | Set a non-COMPUTED thunk to COMPUTED with a CAttrSet pointer.
cthunkSetComputedAttrs :: CThunkPtr -> Ptr () -> IO (Ptr ())
cthunkSetComputedAttrs = c_nn_thunk_set_computed_attrs

-- | Set a non-COMPUTED thunk to COMPUTED with a CCtxStr pointer.
cthunkSetComputedCtxStr :: CThunkPtr -> Ptr () -> IO (Ptr ())
cthunkSetComputedCtxStr = c_nn_thunk_set_computed_ctxstr

-- | Set a non-COMPUTED thunk to COMPUTED with a CLambda pointer.
cthunkSetComputedLambda :: CThunkPtr -> Ptr () -> IO (Ptr ())
cthunkSetComputedLambda = c_nn_thunk_set_computed_lambda

-- ---------------------------------------------------------------------------
-- Arena diagnostics / cleanup
-- ---------------------------------------------------------------------------

-- | Total thunks allocated in the arena.
cthunkCount :: IO Word32
cthunkCount = c_nn_thunk_count

-- | Get a thunk by global index (for cleanup iteration).
-- Returns 'nullPtr' if index is out of range.
cthunkGet :: Word32 -> IO CThunkPtr
cthunkGet = c_nn_thunk_get
