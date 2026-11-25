{-# LANGUAGE PatternSynonyms #-}

-- | C-backed lambda closure structs via FFI.
--
-- Wraps @cbits/nn_lambda.c@ - a malloc'd struct holding the closure
-- environment (nn_env_t*), body bytecode index, and formal parameter
-- specification.  A thunk holds one under tag 9, so a closure takes
-- nothing on GHC's heap.
--
-- Lambda structs are tracked and freed in bulk at arena teardown, or one
-- by one by the region collector ("Nix.Eval.CRegion") once nothing
-- reaches them.
module Nix.Eval.CLambda
  ( -- * Opaque handle
    NnLambda,
    CLambdaPtr,

    -- * Lifecycle
    clambdaNew,
    clambdaSetEntry,

    -- * Accessors
    clambdaEnv,
    clambdaBody,
    clambdaFormalsType,
    clambdaNameSym,
    clambdaLambdaNameSym,
    clambdaAllowExtra,
    clambdaFormalCount,
    clambdaEntryName,
    clambdaEntryHasDefault,
    clambdaEntryDefault,

    -- * Shape
    clambdaShape,
    clambdaShapeFormalsType,
    clambdaShapeAllowExtra,
    clambdaShapeFormalCount,

    -- * Formals types
    pattern FormalsName,
    pattern FormalsSet,
    pattern FormalsNamedSet,
  )
where

import Data.Word (Word32, Word8)
import Foreign.Ptr (Ptr)
import Nix.Eval.CData (MonadCData, cdataIO)
import Nix.Eval.CEnv (NnEnv)
import Nix.Eval.CStatus (checkedCPtr)

-- | Phantom type for C-side @nn_lambda_t@.
data NnLambda

-- | Pointer to a C-allocated lambda closure.
type CLambdaPtr = Ptr NnLambda

-- ---------------------------------------------------------------------------
-- FFI imports (all unsafe - no callbacks, fast data access)
-- ---------------------------------------------------------------------------

foreign import ccall unsafe "nn_lambda_new"
  c_nn_lambda_new :: Ptr NnEnv -> Word32 -> Word8 -> Word32 -> Word32 -> Word8 -> Word32 -> IO CLambdaPtr

foreign import ccall unsafe "nn_lambda_set_entry"
  c_nn_lambda_set_entry :: CLambdaPtr -> Word32 -> Word32 -> Word32 -> Word32 -> IO ()

foreign import ccall unsafe "nn_lambda_env"
  c_nn_lambda_env :: CLambdaPtr -> IO (Ptr NnEnv)

foreign import ccall unsafe "nn_lambda_body"
  c_nn_lambda_body :: CLambdaPtr -> IO Word32

foreign import ccall unsafe "nn_lambda_formals_type"
  c_nn_lambda_formals_type :: CLambdaPtr -> IO Word8

foreign import ccall unsafe "nn_lambda_name_sym"
  c_nn_lambda_name_sym :: CLambdaPtr -> IO Word32

foreign import ccall unsafe "nn_lambda_lambda_name_sym"
  c_nn_lambda_lambda_name_sym :: CLambdaPtr -> IO Word32

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

-- The shape functions read and write no memory, so they are pure.
foreign import ccall unsafe "nn_lambda_shape"
  c_nn_lambda_shape :: Word8 -> Word8 -> Word32 -> Word32

foreign import ccall unsafe "nn_lambda_shape_formals_type"
  c_nn_lambda_shape_formals_type :: Word32 -> Word8

foreign import ccall unsafe "nn_lambda_shape_allow_extra"
  c_nn_lambda_shape_allow_extra :: Word32 -> Word8

foreign import ccall unsafe "nn_lambda_shape_formal_count"
  c_nn_lambda_shape_formal_count :: Word32 -> Word32

-- ---------------------------------------------------------------------------
-- Lifecycle
-- ---------------------------------------------------------------------------

-- | Allocate a new lambda closure with space for @formalCount@ entries:
-- its env, body, formals type, binding name symbol, the function's own
-- name symbol ('Nix.Eval.EvalFormals.lambdaNameSymbol'), ellipsis flag
-- and formal count.  Fill entries via 'clambdaSetEntry' after
-- construction.  The NULL @nn_lambda_new@ answers when it refuses
-- (@cbits/nn_lambda.h@ says when) is raised as
-- 'Nix.Eval.CStatus.CStatusError'.
clambdaNew :: (MonadCData m) => Ptr NnEnv -> Word32 -> Word8 -> Word32 -> Word32 -> Word8 -> Word32 -> m CLambdaPtr
clambdaNew env body formalsType nameSym lambdaNameSym allowExtra formalCount =
  cdataIO (checkedCPtr "nn_lambda_new" =<< c_nn_lambda_new env body formalsType nameSym lambdaNameSym allowExtra formalCount)

-- | Set a formal entry at the given index: its name's symbol, then its
-- default's kind ('Nix.Eval.CBytecode.formalNoDefault' and its siblings)
-- and bytecode index.
clambdaSetEntry :: (MonadCData m) => CLambdaPtr -> Word32 -> Word32 -> Word32 -> Word32 -> m ()
clambdaSetEntry lam idx nameSym defaultKind defaultIdx = cdataIO (c_nn_lambda_set_entry lam idx nameSym defaultKind defaultIdx)

-- ---------------------------------------------------------------------------
-- Accessors
-- ---------------------------------------------------------------------------

-- | Read the closure environment pointer.
clambdaEnv :: (MonadCData m) => CLambdaPtr -> m (Ptr NnEnv)
clambdaEnv = cdataIO . c_nn_lambda_env

-- | Read the body bytecode index.
clambdaBody :: (MonadCData m) => CLambdaPtr -> m Word32
clambdaBody = cdataIO . c_nn_lambda_body

-- | Read the formals type: 'FormalsName', 'FormalsSet' or
-- 'FormalsNamedSet'.
clambdaFormalsType :: (MonadCData m) => CLambdaPtr -> m Word8
clambdaFormalsType = cdataIO . c_nn_lambda_formals_type

-- | Read the binding name symbol (for Name/NamedSet).
clambdaNameSym :: (MonadCData m) => CLambdaPtr -> m Word32
clambdaNameSym = cdataIO . c_nn_lambda_name_sym

-- | Read the function's own name symbol (for Set/NamedSet), the one
-- 'clambdaNew' was given.
clambdaLambdaNameSym :: (MonadCData m) => CLambdaPtr -> m Word32
clambdaLambdaNameSym = cdataIO . c_nn_lambda_lambda_name_sym

-- | Read the ellipsis flag (1 if ... present).
clambdaAllowExtra :: (MonadCData m) => CLambdaPtr -> m Word8
clambdaAllowExtra = cdataIO . c_nn_lambda_allow_extra

-- | Read the number of formal entries.
clambdaFormalCount :: (MonadCData m) => CLambdaPtr -> m Word32
clambdaFormalCount = cdataIO . c_nn_lambda_formal_count

-- | Read a formal entry's name symbol.
clambdaEntryName :: (MonadCData m) => CLambdaPtr -> Word32 -> m Word32
clambdaEntryName lam idx = cdataIO (c_nn_lambda_entry_name lam idx)

-- | Read the kind of a formal entry's default
-- ('Nix.Eval.CBytecode.formalNoDefault' and its siblings).
clambdaEntryHasDefault :: (MonadCData m) => CLambdaPtr -> Word32 -> m Word32
clambdaEntryHasDefault lam idx = cdataIO (c_nn_lambda_entry_has_default lam idx)

-- | Read a formal entry's default bytecode index.
clambdaEntryDefault :: (MonadCData m) => CLambdaPtr -> Word32 -> m Word32
clambdaEntryDefault lam idx = cdataIO (c_nn_lambda_entry_default lam idx)

-- ---------------------------------------------------------------------------
-- Shape
-- ---------------------------------------------------------------------------

-- | The word a closure keeps its formals type, ellipsis flag and formal
-- count in (@nn_lambda_shape@): the type taken mod 4, any nonzero flag as
-- 1 and the count mod 2^29, the widths @cbits\/nn_lambda.h@ gives them.
-- 'clambdaNew' takes only a type and a count that fit.
clambdaShape :: Word8 -> Word8 -> Word32 -> Word32
clambdaShape = c_nn_lambda_shape

-- | The formals type a shape holds.
clambdaShapeFormalsType :: Word32 -> Word8
clambdaShapeFormalsType = c_nn_lambda_shape_formals_type

-- | The ellipsis flag a shape holds, 0 or 1.
clambdaShapeAllowExtra :: Word32 -> Word8
clambdaShapeAllowExtra = c_nn_lambda_shape_allow_extra

-- | The formal count a shape holds.
clambdaShapeFormalCount :: Word32 -> Word32
clambdaShapeFormalCount = c_nn_lambda_shape_formal_count

-- ---------------------------------------------------------------------------
-- Formals types
-- ---------------------------------------------------------------------------

-- | The formals types of a closure, mirroring the @NN_FORMALS_*@ macros
-- in @cbits\/nn_lambda.h@, with which they stay in lockstep: a plain
-- name (@x: body@), a set pattern (@{ a, b }: body@) and a named set
-- pattern (@x\@{ a, b }: body@).
pattern FormalsName, FormalsSet, FormalsNamedSet :: Word8
pattern FormalsName = 0
pattern FormalsSet = 1
pattern FormalsNamedSet = 2
