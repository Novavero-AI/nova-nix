-- | Unified arena lifecycle for the C data layer.
--
-- Initializes and destroys all C sub-arenas (symbol table, thunk arena,
-- env slot allocator) with a single pair of calls.  Handles StablePtr
-- cleanup on destruction: iterates all thunks, identifies payloads that
-- are Haskell StablePtrs (PENDING + COMPUTED\/PTR), and frees them
-- before tearing down the C memory.
--
-- @
-- bracket_ arenaInit arenaDestroy $ do
--   ... evaluation ...
-- @
module Nix.Eval.Arena
  ( arenaInit,
    arenaDestroy,
  )
where

import Data.Word (Word32)
import Foreign.Marshal.Array (allocaArray, peekArray)
import Foreign.Ptr (Ptr)
import Foreign.StablePtr (castPtrToStablePtr, freeStablePtr)
import Nix.Eval.CEnv (cenvDestroy, cenvInit)
import Nix.Eval.CThunk (cthunkDestroy, cthunkInit)
import Nix.Eval.Symbol (symbolDestroy, symbolInit)

-- ---------------------------------------------------------------------------
-- FFI imports for batch StablePtr collection
-- ---------------------------------------------------------------------------

foreign import ccall unsafe "nn_arena_stableptr_count"
  c_nn_arena_stableptr_count :: IO Word32

foreign import ccall unsafe "nn_arena_collect_stableptrs"
  c_nn_arena_collect_stableptrs :: Ptr (Ptr ()) -> Word32 -> IO Word32

-- ---------------------------------------------------------------------------
-- FFI import for bulk CAttrSet cleanup
-- ---------------------------------------------------------------------------

foreign import ccall unsafe "nn_attrset_free_all"
  cattrsetFreeAll :: IO ()

-- ---------------------------------------------------------------------------
-- Lifecycle
-- ---------------------------------------------------------------------------

-- | Initialize all C data layer arenas.  Call once before evaluation.
-- Initializes: symbol table, thunk arena, env slot allocator.
arenaInit :: IO ()
arenaInit = do
  symbolInit 0
  cthunkInit 0
  cenvInit

-- | Destroy all C arenas, properly freeing StablePtrs first.
--
-- 1. Collects all StablePtr payloads from thunks (batch C call)
-- 2. Frees each StablePtr from Haskell
-- 3. Frees all tracked CAttrSets (bulk cleanup)
-- 4. Destroys env slot pages
-- 5. Destroys thunk arena blocks
-- 6. Destroys symbol table + string arena
arenaDestroy :: IO ()
arenaDestroy = do
  cleanupStablePtrs
  cattrsetFreeAll
  cenvDestroy
  cthunkDestroy
  symbolDestroy

-- | Free all StablePtrs held by thunk payloads.
-- Uses batch C-side collection to avoid per-thunk FFI crossing.
-- Only frees PENDING payloads (StablePtr to (Expr, Env)) and
-- COMPUTED/NN_VALUE_PTR payloads (StablePtr to NixValue).
-- Inline scalar payloads (INT, FLOAT, BOOL, NULL) are skipped.
cleanupStablePtrs :: IO ()
cleanupStablePtrs = do
  count <- c_nn_arena_stableptr_count
  if count == 0
    then pure ()
    else allocaArray (fromIntegral count) $ \buf -> do
      written <- c_nn_arena_collect_stableptrs buf count
      ptrs <- peekArray (fromIntegral written) buf
      mapM_ (freeStablePtr . castPtrToStablePtr) ptrs
