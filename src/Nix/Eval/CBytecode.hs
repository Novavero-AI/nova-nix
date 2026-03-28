-- | C-backed bytecode storage via FFI.
--
-- Wraps @cbits/nn_bytecode.c@ — two growable arrays storing flat Nix
-- expression bytecode: an @nn_op_t[]@ instruction array (16 bytes each)
-- and a @uint32_t[]@ data buffer for variable-length operands.
--
-- @
-- cbcInit 0 0
-- idx <- cbcEmit opLitInt 0 0 42 0 0
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
    opLitInt,
    opLitFloat,
    opLitBool,
    opLitNull,
    opLitUri,
    opLitPath,
    opStr,
    opIndStr,
    opVar,
    opWithVar,
    opResolvedVar,
    opAttrs,
    opList,
    opSelect,
    opHasAttr,
    opApp,
    opLambda,
    opLet,
    opIf,
    opWith,
    opAssert,
    opUnary,
    opBinary,
    opSearchPath,

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

opLitInt, opLitFloat, opLitBool, opLitNull :: Word8
opLitInt = 0
opLitFloat = 1
opLitBool = 2
opLitNull = 3

opLitUri, opLitPath :: Word8
opLitUri = 4
opLitPath = 5

opStr, opIndStr :: Word8
opStr = 6
opIndStr = 7

opVar, opWithVar, opResolvedVar :: Word8
opVar = 8
opWithVar = 9
opResolvedVar = 10

opAttrs, opList :: Word8
opAttrs = 11
opList = 12

opSelect, opHasAttr, opApp :: Word8
opSelect = 13
opHasAttr = 14
opApp = 15

opLambda, opLet :: Word8
opLambda = 16
opLet = 17

opIf, opWith, opAssert :: Word8
opIf = 18
opWith = 19
opAssert = 20

opUnary, opBinary :: Word8
opUnary = 21
opBinary = 22

opSearchPath :: Word8
opSearchPath = 23

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
