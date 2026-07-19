-- | C-backed lambda closure structs via FFI.
--
-- Wraps @cbits/nn_lambda.c@ - a malloc'd struct holding the closure
-- environment (nn_env_t*), body bytecode index, and formal parameter
-- specification.  Replaces StablePtr NixValue for VLambda (~25% of
-- remaining GHC heap after M6) with C-native tag 9 in the thunk system.
--
-- Lambda structs are tracked for bulk cleanup via 'clambdaFreeAll'
-- at evaluation end.
module Nix.Eval.CLambda
  ( -- * Opaque handle
    NnLambda,
    CLambdaPtr,

    -- * Lifecycle
    clambdaNew,
    clambdaSetEntry,
    clambdaFreeAll,

    -- * Accessors
    clambdaEnv,
    clambdaBody,
    clambdaFormalsType,
    clambdaNameSym,
    clambdaAllowExtra,
    clambdaFormalCount,
    clambdaEntryName,
    clambdaEntryHasDefault,
    clambdaEntryDefault,
  )
where

import Data.Word (Word32, Word8)
import Foreign.Ptr (Ptr)
import Nix.Eval.CEnv (NnEnv)

-- | Phantom type for C-side @nn_lambda_t@.
data NnLambda

-- | Pointer to a C-allocated lambda closure.
type CLambdaPtr = Ptr NnLambda

-- ---------------------------------------------------------------------------
-- FFI imports (all unsafe - no callbacks, fast data access)
-- ---------------------------------------------------------------------------

foreign import ccall unsafe "nn_lambda_new"
  c_nn_lambda_new :: Ptr NnEnv -> Word32 -> Word8 -> Word32 -> Word8 -> Word32 -> IO CLambdaPtr

foreign import ccall unsafe "nn_lambda_set_entry"
  c_nn_lambda_set_entry :: CLambdaPtr -> Word32 -> Word32 -> Word32 -> Word32 -> IO ()

foreign import ccall unsafe "nn_lambda_free_all"
  c_nn_lambda_free_all :: IO ()

foreign import ccall unsafe "nn_lambda_env"
  c_nn_lambda_env :: CLambdaPtr -> IO (Ptr NnEnv)

foreign import ccall unsafe "nn_lambda_body"
  c_nn_lambda_body :: CLambdaPtr -> IO Word32

foreign import ccall unsafe "nn_lambda_formals_type"
  c_nn_lambda_formals_type :: CLambdaPtr -> IO Word8

foreign import ccall unsafe "nn_lambda_name_sym"
  c_nn_lambda_name_sym :: CLambdaPtr -> IO Word32

foreign import ccall unsafe "nn_lambda_allow_extra"
  c_nn_lambda_allow_extra :: CLambdaPtr -> IO Word8

foreign import ccall unsafe "nn_lambda_formal_count"
  c_nn_lambda_formal_count :: CLambdaPtr -> IO Word32

foreign import ccall unsafe "nn_lambda_entry_name"
  c_nn_lambda_entry_name :: CLambdaPtr -> Word32 -> IO Word32

foreign import ccall unsafe "nn_lambda_entry_has_default"
  c_nn_lambda_entry_has_default :: CLambdaPtr -> Word32 -> IO Word32

foreign import ccall unsafe "nn_lambda_entry_default"
  c_nn_lambda_entry_default :: CLambdaPtr -> Word32 -> IO Word32

-- ---------------------------------------------------------------------------
-- Lifecycle
-- ---------------------------------------------------------------------------

-- | Allocate a new lambda closure with space for @formalCount@ entries.
-- Fill entries via 'clambdaSetEntry' after construction.
-- Returns 'Foreign.Ptr.nullPtr' on allocation failure or if the
-- entries array size would overflow - the caller must check.
clambdaNew :: Ptr NnEnv -> Word32 -> Word8 -> Word32 -> Word8 -> Word32 -> IO CLambdaPtr
clambdaNew = c_nn_lambda_new

-- | Set a formal entry at the given index.
clambdaSetEntry :: CLambdaPtr -> Word32 -> Word32 -> Word32 -> Word32 -> IO ()
clambdaSetEntry = c_nn_lambda_set_entry

-- | Free all tracked lambda structs (arena-style cleanup).
clambdaFreeAll :: IO ()
clambdaFreeAll = c_nn_lambda_free_all

-- ---------------------------------------------------------------------------
-- Accessors
-- ---------------------------------------------------------------------------

-- | Read the closure environment pointer.
clambdaEnv :: CLambdaPtr -> IO (Ptr NnEnv)
clambdaEnv = c_nn_lambda_env

-- | Read the body bytecode index.
clambdaBody :: CLambdaPtr -> IO Word32
clambdaBody = c_nn_lambda_body

-- | Read the formals type (0=Name, 1=Set, 2=NamedSet).
clambdaFormalsType :: CLambdaPtr -> IO Word8
clambdaFormalsType = c_nn_lambda_formals_type

-- | Read the binding name symbol (for Name/NamedSet).
clambdaNameSym :: CLambdaPtr -> IO Word32
clambdaNameSym = c_nn_lambda_name_sym

-- | Read the ellipsis flag (1 if ... present).
clambdaAllowExtra :: CLambdaPtr -> IO Word8
clambdaAllowExtra = c_nn_lambda_allow_extra

-- | Read the number of formal entries.
clambdaFormalCount :: CLambdaPtr -> IO Word32
clambdaFormalCount = c_nn_lambda_formal_count

-- | Read a formal entry's name symbol.
clambdaEntryName :: CLambdaPtr -> Word32 -> IO Word32
clambdaEntryName = c_nn_lambda_entry_name

-- | Read whether a formal entry has a default.
clambdaEntryHasDefault :: CLambdaPtr -> Word32 -> IO Word32
clambdaEntryHasDefault = c_nn_lambda_entry_has_default

-- | Read a formal entry's default bytecode index.
clambdaEntryDefault :: CLambdaPtr -> Word32 -> IO Word32
clambdaEntryDefault = c_nn_lambda_entry_default
