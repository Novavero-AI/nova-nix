-- | C-backed context-bearing strings via FFI.
--
-- Wraps @cbits/nn_ctxstr.c@ — a string with its StringContext stored
-- as a contiguous C struct.  Context-free strings continue to use
-- thunk tag 4 (bare nn_symbol_t); this module handles tag 8 for
-- strings with non-empty context.
--
-- This module provides raw FFI bindings only.  The higher-level
-- marshalling between 'StringContext' and C is in 'Nix.Eval.Types'
-- (to avoid circular imports).
module Nix.Eval.CCtxStr
  ( -- * Opaque handle
    NnCtxStr,
    CCtxStrPtr,

    -- * Lifecycle
    cctxstrNew,
    cctxstrFreeAll,

    -- * Element setters
    cctxstrSetPlain,
    cctxstrSetDrvOutput,
    cctxstrSetAllOutputs,

    -- * Accessors
    cctxstrText,
    cctxstrCtxCount,
    cctxstrElemTag,
    cctxstrElemHash,
    cctxstrElemName,
    cctxstrElemOutput,
  )
where

import Data.Word (Word16, Word32, Word8)
import Foreign.Ptr (Ptr)

-- | Phantom type for C-side @nn_ctxstr_t@.
data NnCtxStr

-- | Pointer to a C-allocated context string.
type CCtxStrPtr = Ptr NnCtxStr

-- ---------------------------------------------------------------------------
-- FFI imports (all unsafe — no callbacks, fast data access)
-- ---------------------------------------------------------------------------

foreign import ccall unsafe "nn_ctxstr_new"
  c_nn_ctxstr_new :: Word32 -> Word16 -> IO CCtxStrPtr

foreign import ccall unsafe "nn_ctxstr_free_all"
  c_nn_ctxstr_free_all :: IO ()

foreign import ccall unsafe "nn_ctxstr_set_plain"
  c_nn_ctxstr_set_plain :: CCtxStrPtr -> Word16 -> Word32 -> Word32 -> IO ()

foreign import ccall unsafe "nn_ctxstr_set_drv_output"
  c_nn_ctxstr_set_drv_output :: CCtxStrPtr -> Word16 -> Word32 -> Word32 -> Word32 -> IO ()

foreign import ccall unsafe "nn_ctxstr_set_all_outputs"
  c_nn_ctxstr_set_all_outputs :: CCtxStrPtr -> Word16 -> Word32 -> Word32 -> IO ()

foreign import ccall unsafe "nn_ctxstr_text"
  c_nn_ctxstr_text :: CCtxStrPtr -> IO Word32

foreign import ccall unsafe "nn_ctxstr_ctx_count"
  c_nn_ctxstr_ctx_count :: CCtxStrPtr -> IO Word16

foreign import ccall unsafe "nn_ctxstr_elem_tag"
  c_nn_ctxstr_elem_tag :: CCtxStrPtr -> Word16 -> IO Word8

foreign import ccall unsafe "nn_ctxstr_elem_hash"
  c_nn_ctxstr_elem_hash :: CCtxStrPtr -> Word16 -> IO Word32

foreign import ccall unsafe "nn_ctxstr_elem_name"
  c_nn_ctxstr_elem_name :: CCtxStrPtr -> Word16 -> IO Word32

foreign import ccall unsafe "nn_ctxstr_elem_output"
  c_nn_ctxstr_elem_output :: CCtxStrPtr -> Word16 -> IO Word32

-- ---------------------------------------------------------------------------
-- Lifecycle
-- ---------------------------------------------------------------------------

-- | Allocate a new context string with space for @ctxCount@ elements.
-- Elements are uninitialized — caller must fill via set functions.
cctxstrNew :: Word32 -> Word16 -> IO CCtxStrPtr
cctxstrNew = c_nn_ctxstr_new

-- | Free all tracked nn_ctxstr_t allocations (arena-style cleanup).
cctxstrFreeAll :: IO ()
cctxstrFreeAll = c_nn_ctxstr_free_all

-- ---------------------------------------------------------------------------
-- Element setters
-- ---------------------------------------------------------------------------

cctxstrSetPlain :: CCtxStrPtr -> Word16 -> Word32 -> Word32 -> IO ()
cctxstrSetPlain = c_nn_ctxstr_set_plain

cctxstrSetDrvOutput :: CCtxStrPtr -> Word16 -> Word32 -> Word32 -> Word32 -> IO ()
cctxstrSetDrvOutput = c_nn_ctxstr_set_drv_output

cctxstrSetAllOutputs :: CCtxStrPtr -> Word16 -> Word32 -> Word32 -> IO ()
cctxstrSetAllOutputs = c_nn_ctxstr_set_all_outputs

-- ---------------------------------------------------------------------------
-- Accessors
-- ---------------------------------------------------------------------------

cctxstrText :: CCtxStrPtr -> IO Word32
cctxstrText = c_nn_ctxstr_text

cctxstrCtxCount :: CCtxStrPtr -> IO Word16
cctxstrCtxCount = c_nn_ctxstr_ctx_count

cctxstrElemTag :: CCtxStrPtr -> Word16 -> IO Word8
cctxstrElemTag = c_nn_ctxstr_elem_tag

cctxstrElemHash :: CCtxStrPtr -> Word16 -> IO Word32
cctxstrElemHash = c_nn_ctxstr_elem_hash

cctxstrElemName :: CCtxStrPtr -> Word16 -> IO Word32
cctxstrElemName = c_nn_ctxstr_elem_name

cctxstrElemOutput :: CCtxStrPtr -> Word16 -> IO Word32
cctxstrElemOutput = c_nn_ctxstr_elem_output
