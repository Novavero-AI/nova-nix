-- | C-backed page allocator for environment slot arrays.
--
-- Wraps @cbits/nn_env.c@ — a page-based bump allocator that provides
-- fast O(1) allocation for variable-size void** arrays.  All memory
-- is freed in bulk via 'cenvDestroy' at evaluation end.
--
-- Slot arrays hold @nn_thunk_t*@ pointers (C-side thunk cells).
-- This moves env slot storage off the GHC heap, eliminating the
-- SmallArray# header (16 bytes) and per-slot boxed Thunk wrappers
-- (16 bytes each) from GC pressure.
module Nix.Eval.CEnv
  ( -- * Lifecycle
    cenvInit,
    cenvDestroy,

    -- * Allocation
    cenvAllocSlots,
  )
where

import Data.Word (Word32)
import Foreign.Ptr (Ptr)
import Nix.Eval.CThunk (CThunkPtr)

-- ---------------------------------------------------------------------------
-- FFI imports (all unsafe — no callbacks, fast allocation)
-- ---------------------------------------------------------------------------

foreign import ccall unsafe "nn_env_init"
  c_nn_env_init :: IO ()

foreign import ccall unsafe "nn_env_destroy"
  c_nn_env_destroy :: IO ()

foreign import ccall unsafe "nn_env_alloc_slots"
  c_nn_env_alloc_slots :: Word32 -> IO (Ptr CThunkPtr)

-- ---------------------------------------------------------------------------
-- Lifecycle
-- ---------------------------------------------------------------------------

-- | Initialize the global env slot allocator.  Call once before evaluation.
cenvInit :: IO ()
cenvInit = c_nn_env_init

-- | Destroy the global env slot allocator, freeing all page memory.
-- All slot array pointers become invalid after this call.
cenvDestroy :: IO ()
cenvDestroy = c_nn_env_destroy

-- ---------------------------------------------------------------------------
-- Allocation
-- ---------------------------------------------------------------------------

-- | Allocate a C array of @count@ 'CThunkPtr' slots.  O(1) amortized.
-- Returns 'nullPtr' if count is 0.  All slots are zero-initialized.
cenvAllocSlots :: Word32 -> IO (Ptr CThunkPtr)
cenvAllocSlots = c_nn_env_alloc_slots
