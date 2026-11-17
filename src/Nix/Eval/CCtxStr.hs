-- | C-backed context-bearing strings via FFI.
--
-- Wraps @cbits/nn_ctxstr.c@ - a string with its StringContext stored
-- as a contiguous C struct, its text and each store path's hash and name
-- and output name a string's bytes ("Nix.Eval.CStr").  Context-free
-- strings use thunk tag 4 (a string's bytes alone); this module handles
-- tag 8 for strings with non-empty context.
--
-- This module provides raw FFI bindings only.  The higher-level
-- marshalling between t'Nix.Eval.Types.StringContext' and C is in "Nix.Eval.Types"
-- (to avoid circular imports).
module Nix.Eval.CCtxStr
  ( -- * Opaque handle
    NnCtxStr,
    CCtxStrPtr,

    -- * Lifecycle
    cctxstrNew,

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
import Nix.Eval.CData (MonadCData, cdataIO)
import Nix.Eval.CStr (CStrPtr)

-- | Phantom type for C-side @nn_ctxstr_t@.
data NnCtxStr

-- | Pointer to a C-allocated context string.
type CCtxStrPtr = Ptr NnCtxStr

-- ---------------------------------------------------------------------------
-- FFI imports (all unsafe - no callbacks, fast data access)
-- ---------------------------------------------------------------------------

foreign import ccall unsafe "nn_ctxstr_new"
  c_nn_ctxstr_new :: CStrPtr -> Word32 -> IO CCtxStrPtr

foreign import ccall unsafe "nn_ctxstr_set_plain"
  c_nn_ctxstr_set_plain :: CCtxStrPtr -> Word32 -> CStrPtr -> CStrPtr -> IO ()

foreign import ccall unsafe "nn_ctxstr_set_drv_output"
  c_nn_ctxstr_set_drv_output :: CCtxStrPtr -> Word32 -> CStrPtr -> CStrPtr -> CStrPtr -> IO ()

foreign import ccall unsafe "nn_ctxstr_set_all_outputs"
  c_nn_ctxstr_set_all_outputs :: CCtxStrPtr -> Word32 -> CStrPtr -> CStrPtr -> IO ()

foreign import ccall unsafe "nn_ctxstr_text"
  c_nn_ctxstr_text :: CCtxStrPtr -> IO CStrPtr

foreign import ccall unsafe "nn_ctxstr_ctx_count"
  c_nn_ctxstr_ctx_count :: CCtxStrPtr -> IO Word32

foreign import ccall unsafe "nn_ctxstr_elem_tag"
  c_nn_ctxstr_elem_tag :: CCtxStrPtr -> Word32 -> IO Word8

foreign import ccall unsafe "nn_ctxstr_elem_hash"
  c_nn_ctxstr_elem_hash :: CCtxStrPtr -> Word32 -> IO CStrPtr

foreign import ccall unsafe "nn_ctxstr_elem_name"
  c_nn_ctxstr_elem_name :: CCtxStrPtr -> Word32 -> IO CStrPtr

foreign import ccall unsafe "nn_ctxstr_elem_output"
  c_nn_ctxstr_elem_output :: CCtxStrPtr -> Word32 -> IO CStrPtr

-- ---------------------------------------------------------------------------
-- Lifecycle
-- ---------------------------------------------------------------------------

-- | Allocate a new context string over its text's bytes with space for
-- @ctxCount@ elements, which the caller fills via the set functions.
-- Returns 'Foreign.Ptr.nullPtr' on allocation failure or if the
-- element array size would overflow - the caller must check.
cctxstrNew :: (MonadCData m) => CStrPtr -> Word32 -> m CCtxStrPtr
cctxstrNew text ctxCount = cdataIO (c_nn_ctxstr_new text ctxCount)

-- ---------------------------------------------------------------------------
-- Element setters
-- ---------------------------------------------------------------------------

-- | Set context element @i@ to a plain store-path reference (@SCPlain@),
-- identified by its hash and name.
cctxstrSetPlain :: (MonadCData m) => CCtxStrPtr -> Word32 -> CStrPtr -> CStrPtr -> m ()
cctxstrSetPlain ptr idx hash name = cdataIO (c_nn_ctxstr_set_plain ptr idx hash name)

-- | Set context element @i@ to a derivation-output reference (@SCDrvOutput@):
-- hash and name plus the output name.
cctxstrSetDrvOutput :: (MonadCData m) => CCtxStrPtr -> Word32 -> CStrPtr -> CStrPtr -> CStrPtr -> m ()
cctxstrSetDrvOutput ptr idx hash name output = cdataIO (c_nn_ctxstr_set_drv_output ptr idx hash name output)

-- | Set context element @i@ to an all-outputs reference (@SCAllOutputs@),
-- identified by its hash and name.
cctxstrSetAllOutputs :: (MonadCData m) => CCtxStrPtr -> Word32 -> CStrPtr -> CStrPtr -> m ()
cctxstrSetAllOutputs ptr idx hash name = cdataIO (c_nn_ctxstr_set_all_outputs ptr idx hash name)

-- ---------------------------------------------------------------------------
-- Accessors
-- ---------------------------------------------------------------------------

-- | The context string's text.
cctxstrText :: (MonadCData m) => CCtxStrPtr -> m CStrPtr
cctxstrText = cdataIO . c_nn_ctxstr_text

-- | The number of context elements attached to the string.
cctxstrCtxCount :: (MonadCData m) => CCtxStrPtr -> m Word32
cctxstrCtxCount = cdataIO . c_nn_ctxstr_ctx_count

-- | The tag of context element @i@ (plain / drv-output / all-outputs).
cctxstrElemTag :: (MonadCData m) => CCtxStrPtr -> Word32 -> m Word8
cctxstrElemTag ptr idx = cdataIO (c_nn_ctxstr_elem_tag ptr idx)

-- | The store path's hash of context element @i@.
cctxstrElemHash :: (MonadCData m) => CCtxStrPtr -> Word32 -> m CStrPtr
cctxstrElemHash ptr idx = cdataIO (c_nn_ctxstr_elem_hash ptr idx)

-- | The store path's name of context element @i@.
cctxstrElemName :: (MonadCData m) => CCtxStrPtr -> Word32 -> m CStrPtr
cctxstrElemName ptr idx = cdataIO (c_nn_ctxstr_elem_name ptr idx)

-- | The output name of context element @i@ (drv-output elements only).
cctxstrElemOutput :: (MonadCData m) => CCtxStrPtr -> Word32 -> m CStrPtr
cctxstrElemOutput ptr idx = cdataIO (c_nn_ctxstr_elem_output ptr idx)
