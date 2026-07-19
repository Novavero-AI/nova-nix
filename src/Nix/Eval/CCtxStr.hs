-- | C-backed context-bearing strings via FFI.
--
-- Wraps @cbits/nn_ctxstr.c@ - a string with its StringContext stored
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

import Data.Word (Word32, Word8)
import Foreign.Ptr (Ptr)

-- | Phantom type for C-side @nn_ctxstr_t@.
data NnCtxStr

-- | Pointer to a C-allocated context string.
type CCtxStrPtr = Ptr NnCtxStr

-- ---------------------------------------------------------------------------
-- FFI imports (all unsafe - no callbacks, fast data access)
-- ---------------------------------------------------------------------------

foreign import ccall unsafe "nn_ctxstr_new"
  c_nn_ctxstr_new :: Word32 -> Word32 -> IO CCtxStrPtr

foreign import ccall unsafe "nn_ctxstr_free_all"
  c_nn_ctxstr_free_all :: IO ()

foreign import ccall unsafe "nn_ctxstr_set_plain"
  c_nn_ctxstr_set_plain :: CCtxStrPtr -> Word32 -> Word32 -> Word32 -> IO ()

foreign import ccall unsafe "nn_ctxstr_set_drv_output"
  c_nn_ctxstr_set_drv_output :: CCtxStrPtr -> Word32 -> Word32 -> Word32 -> Word32 -> IO ()

foreign import ccall unsafe "nn_ctxstr_set_all_outputs"
  c_nn_ctxstr_set_all_outputs :: CCtxStrPtr -> Word32 -> Word32 -> Word32 -> IO ()

foreign import ccall unsafe "nn_ctxstr_text"
  c_nn_ctxstr_text :: CCtxStrPtr -> IO Word32

foreign import ccall unsafe "nn_ctxstr_ctx_count"
  c_nn_ctxstr_ctx_count :: CCtxStrPtr -> IO Word32

foreign import ccall unsafe "nn_ctxstr_elem_tag"
  c_nn_ctxstr_elem_tag :: CCtxStrPtr -> Word32 -> IO Word8

foreign import ccall unsafe "nn_ctxstr_elem_hash"
  c_nn_ctxstr_elem_hash :: CCtxStrPtr -> Word32 -> IO Word32

foreign import ccall unsafe "nn_ctxstr_elem_name"
  c_nn_ctxstr_elem_name :: CCtxStrPtr -> Word32 -> IO Word32

foreign import ccall unsafe "nn_ctxstr_elem_output"
  c_nn_ctxstr_elem_output :: CCtxStrPtr -> Word32 -> IO Word32

-- ---------------------------------------------------------------------------
-- Lifecycle
-- ---------------------------------------------------------------------------

-- | Allocate a new context string with space for @ctxCount@ elements.
-- Elements are uninitialized - caller must fill via set functions.
-- Returns 'Foreign.Ptr.nullPtr' on allocation failure or if the
-- element array size would overflow - the caller must check.
cctxstrNew :: Word32 -> Word32 -> IO CCtxStrPtr
cctxstrNew = c_nn_ctxstr_new

-- | Free all tracked nn_ctxstr_t allocations (arena-style cleanup).
cctxstrFreeAll :: IO ()
cctxstrFreeAll = c_nn_ctxstr_free_all

-- ---------------------------------------------------------------------------
-- Element setters
-- ---------------------------------------------------------------------------

-- | Set context element @i@ to a plain store-path reference (@SCPlain@),
-- identified by its name and hash symbols.
cctxstrSetPlain :: CCtxStrPtr -> Word32 -> Word32 -> Word32 -> IO ()
cctxstrSetPlain = c_nn_ctxstr_set_plain

-- | Set context element @i@ to a derivation-output reference (@SCDrvOutput@):
-- name and hash symbols plus the output-name symbol.
cctxstrSetDrvOutput :: CCtxStrPtr -> Word32 -> Word32 -> Word32 -> Word32 -> IO ()
cctxstrSetDrvOutput = c_nn_ctxstr_set_drv_output

-- | Set context element @i@ to an all-outputs reference (@SCAllOutputs@),
-- identified by its name and hash symbols.
cctxstrSetAllOutputs :: CCtxStrPtr -> Word32 -> Word32 -> Word32 -> IO ()
cctxstrSetAllOutputs = c_nn_ctxstr_set_all_outputs

-- ---------------------------------------------------------------------------
-- Accessors
-- ---------------------------------------------------------------------------

-- | The interned-symbol id of the context string's text.
cctxstrText :: CCtxStrPtr -> IO Word32
cctxstrText = c_nn_ctxstr_text

-- | The number of context elements attached to the string.
cctxstrCtxCount :: CCtxStrPtr -> IO Word32
cctxstrCtxCount = c_nn_ctxstr_ctx_count

-- | The tag of context element @i@ (plain / drv-output / all-outputs).
cctxstrElemTag :: CCtxStrPtr -> Word32 -> IO Word8
cctxstrElemTag = c_nn_ctxstr_elem_tag

-- | The store-path-hash symbol of context element @i@.
cctxstrElemHash :: CCtxStrPtr -> Word32 -> IO Word32
cctxstrElemHash = c_nn_ctxstr_elem_hash

-- | The store-path-name symbol of context element @i@.
cctxstrElemName :: CCtxStrPtr -> Word32 -> IO Word32
cctxstrElemName = c_nn_ctxstr_elem_name

-- | The output-name symbol of context element @i@ (drv-output elements only).
cctxstrElemOutput :: CCtxStrPtr -> Word32 -> IO Word32
cctxstrElemOutput = c_nn_ctxstr_elem_output
