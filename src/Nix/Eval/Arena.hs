-- | Unified arena lifecycle for the C data layer.
--
-- Initializes and destroys all C sub-arenas (symbol table, thunk arena,
-- env slot allocator, bytecode store) with a single pair of calls.
-- Handles StablePtr cleanup on destruction: iterates all thunks,
-- identifies payloads that are Haskell StablePtrs (PENDING +
-- COMPUTED\/PTR), and frees them before tearing down the C memory.
--
-- Everything that evaluates runs between the two calls:
--
-- @
-- bracket_ arenaInit arenaDestroy $ do
--   ... evaluation ...
-- @
--
-- Outside that window the C layer has no memory to hand out, and every
-- path into it ('Nix.Builtins.builtinEnv', 'Nix.Eval.eval',
-- 'Nix.Eval.force', the value constructors) raises
-- 'ArenaNotInitialized' naming the C entry point that refused, rather
-- than dereferencing state that does not exist.  Parsing ("Nix.Parser")
-- is pure and needs no arena.
--
-- 'ArenaNotInitialized' is a setup diagnosis: add the bracket and start
-- the process again.  It is not an error to catch and retry in the same
-- process, because GHC updates a thunk whose evaluation raised with that
-- exception: a library constant that reached C before the window (the
-- shared null thunk, the @true@ and @false@ slots of
-- 'Nix.Builtins.builtinEnv') re-raises it inside a later, live arena.
-- Removing those process-lifetime constants is #19.
module Nix.Eval.Arena
  ( -- * Lifecycle
    arenaInit,
    arenaDestroy,
    arenaLive,

    -- * Errors
    CStatusError (..),
  )
where

import Data.Word (Word32)
import Foreign.Marshal.Array (allocaArray, peekArray)
import Foreign.Ptr (Ptr)
import Foreign.StablePtr (castPtrToStablePtr, freeStablePtr)
import Nix.Eval.CBytecode (cbcDestroy, cbcInit)
import Nix.Eval.CCtxStr (cctxstrFreeAll)
import Nix.Eval.CEnv (cenvDestroy, cenvInit)
import Nix.Eval.CLambda (clambdaFreeAll)
import Nix.Eval.CList (clistFreeAll)
import Nix.Eval.CStatus (CStatusError (..), arenaLive)
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
-- Initializes: symbol table, thunk arena, env slot allocator, bytecode store.
arenaInit :: IO ()
arenaInit = do
  symbolInit 0
  cthunkInit 0
  cenvInit
  cbcInit 0 0

-- | Destroy all C arenas, properly freeing StablePtrs first.
--
-- This ends the process's use of the evaluator; a second 'arenaInit' does
-- not restore one.  Thunks the library allocates once for the process,
-- explicitly (@derivationWrapper@ in "Nix.Builtins") or as a
-- parameter-free entry of 'Nix.Builtins.builtinEnv' that GHC shares
-- across its calls (@storeDir@, @map@), keep pointing into the memory
-- freed here, and only an entry built per call (@currentTime@) survives
-- a second cycle.
--
-- 1. Collects all StablePtr payloads from thunks (batch C call)
-- 2. Frees each StablePtr from Haskell
-- 3. Frees all tracked CLists (bulk cleanup)
-- 4. Frees all tracked CAttrSets (bulk cleanup)
-- 5. Destroys bytecode store
-- 6. Destroys env slot pages
-- 7. Destroys thunk arena blocks
-- 8. Destroys symbol table + string arena
arenaDestroy :: IO ()
arenaDestroy = do
  cleanupStablePtrs
  cctxstrFreeAll
  clambdaFreeAll
  clistFreeAll
  cattrsetFreeAll
  cbcDestroy
  cenvDestroy
  cthunkDestroy
  symbolDestroy

-- | Free all StablePtrs held by thunk payloads.
-- Uses batch C-side collection to avoid per-thunk FFI crossing.
-- Frees payloads from three thunk states:
--   PENDING:              StablePtr (Expr, Env)
--   BLACKHOLE:            original PENDING StablePtr (eval failed/in-progress)
--   COMPUTED/NN_VALUE_PTR: StablePtr NixValue
-- Inline scalar/C-pointer payloads (INT..CTXSTR) are skipped.
cleanupStablePtrs :: IO ()
cleanupStablePtrs = do
  count <- c_nn_arena_stableptr_count
  if count == 0
    then pure ()
    else allocaArray (fromIntegral count) $ \buf -> do
      written <- c_nn_arena_collect_stableptrs buf count
      ptrs <- peekArray (fromIntegral written) buf
      mapM_ (freeStablePtr . castPtrToStablePtr) ptrs
