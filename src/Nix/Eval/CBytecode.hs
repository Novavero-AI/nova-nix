{-# LANGUAGE PatternSynonyms #-}

-- | C-backed bytecode storage via FFI.
--
-- Wraps @cbits/nn_bytecode.c@ — two growable arrays storing flat Nix
-- expression bytecode: an @nn_op_t[]@ instruction array (16 bytes each)
-- and a @uint32_t[]@ data buffer for variable-length operands.
--
-- @
-- cbcInit 0 0
-- idx <- cbcEmit OpLitInt 0 0 42 0 0
-- cbcOpcode idx   -- NN_OP_LIT_INT (0)
-- cbcArg1 idx     -- 42
-- cbcDestroy
-- @
module Nix.Eval.CBytecode
  ( -- * Lifecycle
    cbcInit,
    cbcDestroy,

    -- * Emit
    cbcEmit,
    cbcEmitData,

    -- * Read instructions
    cbcOpcode,
    cbcFlags,
    cbcShortArg,
    cbcArg1,
    cbcArg2,
    cbcArg3,

    -- * Read data
    cbcData,

    -- * Diagnostics
    cbcOpCount,
    cbcDataCount,

    -- * Opcodes
    pattern OpLitInt,
    pattern OpLitFloat,
    pattern OpLitBool,
    pattern OpLitNull,
    pattern OpLitUri,
    pattern OpLitPath,
    pattern OpStr,
    pattern OpIndStr,
    pattern OpVar,
    pattern OpWithVar,
    pattern OpResolvedVar,
    pattern OpAttrs,
    pattern OpList,
    pattern OpSelect,
    pattern OpHasAttr,
    pattern OpApp,
    pattern OpLambda,
    pattern OpLet,
    pattern OpIf,
    pattern OpWith,
    pattern OpAssert,
    pattern OpUnary,
    pattern OpBinary,
    pattern OpSearchPath,

    -- * UnaryOp flags
    unaryNot,
    unaryNegate,

    -- * BinaryOp flags
    binaryAdd,
    binarySub,
    binaryMul,
    binaryDiv,
    binaryAnd,
    binaryOr,
    binaryImpl,
    binaryEq,
    binaryNeq,
    binaryLt,
    binaryLte,
    binaryGt,
    binaryGte,
    binaryConcat,
    binaryUpdate,

    -- * Formal type flags
    formalName,
    formalSet,
    formalNamedSet,

    -- * String part tags
    strpartLit,
    strpartInterp,

    -- * Binding type tags
    bindNamed,
    bindInherit,

    -- * CaptureInfo type tags
    captureNone,
    captureSlots,
    captureWithScopes,

    -- * Attr key tags
    attrkeyStatic,
    attrkeyDynamic,
  )
where

import Data.Word (Word16, Word32, Word8)

-- ---------------------------------------------------------------------------
-- FFI imports (all unsafe — no callbacks, fast data access)
-- ---------------------------------------------------------------------------

foreign import ccall unsafe "nn_bytecode_init"
  c_nn_bytecode_init :: Word32 -> Word32 -> IO ()

foreign import ccall unsafe "nn_bytecode_destroy"
  c_nn_bytecode_destroy :: IO ()

foreign import ccall unsafe "nn_bc_emit"
  c_nn_bc_emit :: Word8 -> Word8 -> Word16 -> Word32 -> Word32 -> Word32 -> IO Word32

foreign import ccall unsafe "nn_bc_emit_data"
  c_nn_bc_emit_data :: Word32 -> IO Word32

foreign import ccall unsafe "nn_bc_opcode"
  c_nn_bc_opcode :: Word32 -> IO Word8

foreign import ccall unsafe "nn_bc_flags"
  c_nn_bc_flags :: Word32 -> IO Word8

foreign import ccall unsafe "nn_bc_short_arg"
  c_nn_bc_short_arg :: Word32 -> IO Word16

foreign import ccall unsafe "nn_bc_arg1"
  c_nn_bc_arg1 :: Word32 -> IO Word32

foreign import ccall unsafe "nn_bc_arg2"
  c_nn_bc_arg2 :: Word32 -> IO Word32

foreign import ccall unsafe "nn_bc_arg3"
  c_nn_bc_arg3 :: Word32 -> IO Word32

foreign import ccall unsafe "nn_bc_data"
  c_nn_bc_data :: Word32 -> IO Word32

foreign import ccall unsafe "nn_bc_op_count"
  c_nn_bc_op_count :: IO Word32

foreign import ccall unsafe "nn_bc_data_count"
  c_nn_bc_data_count :: IO Word32

-- ---------------------------------------------------------------------------
-- Lifecycle
-- ---------------------------------------------------------------------------

-- | Initialize the global bytecode store.  Call once before evaluation.
-- Arguments are capacity hints (0 = defaults: 65536 ops, 131072 data).
cbcInit :: Word32 -> Word32 -> IO ()
cbcInit = c_nn_bytecode_init

-- | Destroy the global bytecode store, freeing all memory.
cbcDestroy :: IO ()
cbcDestroy = c_nn_bytecode_destroy

-- ---------------------------------------------------------------------------
-- Emit
-- ---------------------------------------------------------------------------

-- | Append one instruction.  Returns the instruction index.
cbcEmit :: Word8 -> Word8 -> Word16 -> Word32 -> Word32 -> Word32 -> IO Word32
cbcEmit = c_nn_bc_emit

-- | Append one uint32 to the data buffer.  Returns the data offset.
cbcEmitData :: Word32 -> IO Word32
cbcEmitData = c_nn_bc_emit_data

-- ---------------------------------------------------------------------------
-- Read instructions
-- ---------------------------------------------------------------------------

cbcOpcode :: Word32 -> IO Word8
cbcOpcode = c_nn_bc_opcode

cbcFlags :: Word32 -> IO Word8
cbcFlags = c_nn_bc_flags

cbcShortArg :: Word32 -> IO Word16
cbcShortArg = c_nn_bc_short_arg

cbcArg1 :: Word32 -> IO Word32
cbcArg1 = c_nn_bc_arg1

cbcArg2 :: Word32 -> IO Word32
cbcArg2 = c_nn_bc_arg2

cbcArg3 :: Word32 -> IO Word32
cbcArg3 = c_nn_bc_arg3

-- ---------------------------------------------------------------------------
-- Read data
-- ---------------------------------------------------------------------------

cbcData :: Word32 -> IO Word32
cbcData = c_nn_bc_data

-- ---------------------------------------------------------------------------
-- Diagnostics
-- ---------------------------------------------------------------------------

cbcOpCount :: IO Word32
cbcOpCount = c_nn_bc_op_count

cbcDataCount :: IO Word32
cbcDataCount = c_nn_bc_data_count

-- ---------------------------------------------------------------------------
-- Opcode constants
-- ---------------------------------------------------------------------------

pattern OpLitInt, OpLitFloat, OpLitBool, OpLitNull :: Word8
pattern OpLitInt = 0
pattern OpLitFloat = 1
pattern OpLitBool = 2
pattern OpLitNull = 3

pattern OpLitUri, OpLitPath :: Word8
pattern OpLitUri = 4
pattern OpLitPath = 5

pattern OpStr, OpIndStr :: Word8
pattern OpStr = 6
pattern OpIndStr = 7

pattern OpVar, OpWithVar, OpResolvedVar :: Word8
pattern OpVar = 8
pattern OpWithVar = 9
pattern OpResolvedVar = 10

pattern OpAttrs, OpList :: Word8
pattern OpAttrs = 11
pattern OpList = 12

pattern OpSelect, OpHasAttr, OpApp :: Word8
pattern OpSelect = 13
pattern OpHasAttr = 14
pattern OpApp = 15

pattern OpLambda, OpLet :: Word8
pattern OpLambda = 16
pattern OpLet = 17

pattern OpIf, OpWith, OpAssert :: Word8
pattern OpIf = 18
pattern OpWith = 19
pattern OpAssert = 20

pattern OpUnary, OpBinary :: Word8
pattern OpUnary = 21
pattern OpBinary = 22

pattern OpSearchPath :: Word8
pattern OpSearchPath = 23

-- ---------------------------------------------------------------------------
-- Sub-type flags
-- ---------------------------------------------------------------------------

unaryNot, unaryNegate :: Word8
unaryNot = 0
unaryNegate = 1

binaryAdd, binarySub, binaryMul, binaryDiv :: Word8
binaryAdd = 0
binarySub = 1
binaryMul = 2
binaryDiv = 3

binaryAnd, binaryOr, binaryImpl :: Word8
binaryAnd = 4
binaryOr = 5
binaryImpl = 6

binaryEq, binaryNeq :: Word8
binaryEq = 7
binaryNeq = 8

binaryLt, binaryLte, binaryGt, binaryGte :: Word8
binaryLt = 9
binaryLte = 10
binaryGt = 11
binaryGte = 12

binaryConcat, binaryUpdate :: Word8
binaryConcat = 13
binaryUpdate = 14

formalName, formalSet, formalNamedSet :: Word8
formalName = 0
formalSet = 1
formalNamedSet = 2

strpartLit, strpartInterp :: Word32
strpartLit = 0
strpartInterp = 1

bindNamed, bindInherit :: Word32
bindNamed = 0
bindInherit = 1

captureNone, captureSlots, captureWithScopes :: Word32
captureNone = 0
captureSlots = 1
captureWithScopes = 2

attrkeyStatic, attrkeyDynamic :: Word32
attrkeyStatic = 0
attrkeyDynamic = 1
