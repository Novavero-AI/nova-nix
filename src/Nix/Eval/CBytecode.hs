{-# LANGUAGE PatternSynonyms #-}

-- | C-backed bytecode storage via FFI.
--
-- Wraps @cbits/nn_bytecode.c@ - two growable arrays storing flat Nix
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

    -- * Counted payloads (short_arg spill)
    spilledCountSentinel,
    cbcCountedPayload,

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
    pattern OpPathStr,

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
-- FFI imports (all unsafe - no callbacks, fast data access)
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
cbcEmit opcode flags shortArg arg1 arg2 arg3 =
  checkedEmit "nn_bc_emit" =<< c_nn_bc_emit opcode flags shortArg arg1 arg2 arg3

-- | Append one uint32 to the data buffer.  Returns the data offset.
cbcEmitData :: Word32 -> IO Word32
cbcEmitData value = checkedEmit "nn_bc_emit_data" =<< c_nn_bc_emit_data value

-- | @UINT32_MAX@ from a C emit function signals a failed append (realloc
-- exhaustion or the uint32 index ceiling), not a valid index.  Must stay
-- in lockstep with the emit docs in @cbits\/nn_bytecode.h@.
emitFailedSentinel :: Word32
emitFailedSentinel = 0xFFFFFFFF

-- | Reject the failure sentinel before it can flow onward as an index:
-- the bytecode arrays are global C state, so a failed append leaves
-- nothing to recover, and the read-side bounds on a sentinel index do
-- not survive a release build.
checkedEmit :: String -> Word32 -> IO Word32
checkedEmit site idx
  | idx == emitFailedSentinel =
      ioError (userError (site <> ": bytecode append failed (allocation failure or index ceiling)"))
  | otherwise = pure idx

-- ---------------------------------------------------------------------------
-- Read instructions
-- ---------------------------------------------------------------------------

-- | The opcode byte of instruction @i@ (one of the @Op*@ constants).
cbcOpcode :: Word32 -> IO Word8
cbcOpcode = c_nn_bc_opcode

-- | The flags byte of instruction @i@ - the sub-type selector (e.g. which
-- unary/binary operator), interpreted via the sub-type-flag constants below.
cbcFlags :: Word32 -> IO Word8
cbcFlags = c_nn_bc_flags

-- | The inline 16-bit argument of instruction @i@ (e.g. a de Bruijn slot).
cbcShortArg :: Word32 -> IO Word16
cbcShortArg = c_nn_bc_short_arg

-- | The first 32-bit argument of instruction @i@.
cbcArg1 :: Word32 -> IO Word32
cbcArg1 = c_nn_bc_arg1

-- | The second 32-bit argument of instruction @i@.
cbcArg2 :: Word32 -> IO Word32
cbcArg2 = c_nn_bc_arg2

-- | The third 32-bit argument of instruction @i@.
cbcArg3 :: Word32 -> IO Word32
cbcArg3 = c_nn_bc_arg3

-- ---------------------------------------------------------------------------
-- Read data
-- ---------------------------------------------------------------------------

-- | Read entry @i@ of the bytecode data pool (interned literals, symbols).
cbcData :: Word32 -> IO Word32
cbcData = c_nn_bc_data

-- ---------------------------------------------------------------------------
-- Counted payloads (short_arg spill)
-- ---------------------------------------------------------------------------

-- | @short_arg@ value marking a spilled payload count: the true count is
-- the FIRST word of the op's data region and the payload begins one word
-- later.  Counts below the sentinel travel inline, so @nn_op_t@ stays 16
-- bytes (four ops per cache line) and only an oversized literal pays the
-- extra data word.  Must stay in lockstep with the @short_arg@ doc in
-- @cbits\/nn_bytecode.h@.
spilledCountSentinel :: Word16
spilledCountSentinel = 0xFFFF

-- | Read an op's payload count and payload start offset, resolving the
-- spill convention.  @dataOff@ is the raw offset stored in the op's arg
-- word (arg1 for most counted ops, arg2 for select\/hasAttr paths).
-- The inline case costs one compare.
cbcCountedPayload :: Word32 -> Word32 -> IO (Int, Word32)
cbcCountedPayload bcIdx dataOff = do
  inline <- cbcShortArg bcIdx
  if inline == spilledCountSentinel
    then do
      spilled <- cbcData dataOff
      pure (fromIntegral spilled, dataOff + 1)
    else pure (fromIntegral inline, dataOff)

-- ---------------------------------------------------------------------------
-- Diagnostics
-- ---------------------------------------------------------------------------

-- | Total number of compiled instructions.
cbcOpCount :: IO Word32
cbcOpCount = c_nn_bc_op_count

-- | Total number of data-pool entries.
cbcDataCount :: IO Word32
cbcDataCount = c_nn_bc_data_count

-- ---------------------------------------------------------------------------
-- Opcode constants
-- ---------------------------------------------------------------------------

-- | Literal-constant opcodes: integer, float, boolean, and null.
pattern OpLitInt, OpLitFloat, OpLitBool, OpLitNull :: Word8
pattern OpLitInt = 0
pattern OpLitFloat = 1
pattern OpLitBool = 2
pattern OpLitNull = 3

-- | Literal URI and path opcodes.
pattern OpLitUri, OpLitPath :: Word8
pattern OpLitUri = 4
pattern OpLitPath = 5

-- | String opcodes: an ordinary @"..."@ string and an indented @''...''@ string.
pattern OpStr, OpIndStr :: Word8
pattern OpStr = 6
pattern OpIndStr = 7

-- | Variable-reference opcodes: a lexical variable, a @with@-scope lookup, and a
-- resolved (de Bruijn) variable.
pattern OpVar, OpWithVar, OpResolvedVar :: Word8
pattern OpVar = 8
pattern OpWithVar = 9
pattern OpResolvedVar = 10

-- | Collection-construction opcodes: attribute set and list.
pattern OpAttrs, OpList :: Word8
pattern OpAttrs = 11
pattern OpList = 12

-- | Attribute-select (@a.b@), has-attribute (@a ? b@), and function-application
-- opcodes.
pattern OpSelect, OpHasAttr, OpApp :: Word8
pattern OpSelect = 13
pattern OpHasAttr = 14
pattern OpApp = 15

-- | Binding-form opcodes: lambda and @let@.
pattern OpLambda, OpLet :: Word8
pattern OpLambda = 16
pattern OpLet = 17

-- | Control-form opcodes: @if@, @with@, and @assert@.
pattern OpIf, OpWith, OpAssert :: Word8
pattern OpIf = 18
pattern OpWith = 19
pattern OpAssert = 20

-- | Operator opcodes: unary and binary (the specific operator is in the flags
-- byte, decoded via the sub-type-flag constants below).
pattern OpUnary, OpBinary :: Word8
pattern OpUnary = 21
pattern OpBinary = 22

-- | Search-path opcode: an angle-bracket path lookup like @\<nixpkgs\>@.
pattern OpSearchPath :: Word8
pattern OpSearchPath = 23

pattern OpPathStr :: Word8
pattern OpPathStr = 24

-- ---------------------------------------------------------------------------
-- Sub-type flags
-- ---------------------------------------------------------------------------

-- | @OpUnary@ flag values: which unary operator to apply.
unaryNot, unaryNegate :: Word8
unaryNot = 0
unaryNegate = 1

-- | @OpBinary@ flag values, arithmetic: @+@ @-@ @*@ @/@.
binaryAdd, binarySub, binaryMul, binaryDiv :: Word8
binaryAdd = 0
binarySub = 1
binaryMul = 2
binaryDiv = 3

-- | @OpBinary@ flag values, logical: @&&@ @||@ @->@.
binaryAnd, binaryOr, binaryImpl :: Word8
binaryAnd = 4
binaryOr = 5
binaryImpl = 6

-- | @OpBinary@ flag values, equality: @==@ @!=@.
binaryEq, binaryNeq :: Word8
binaryEq = 7
binaryNeq = 8

-- | @OpBinary@ flag values, ordering: @<@ @<=@ @>@ @>=@.
binaryLt, binaryLte, binaryGt, binaryGte :: Word8
binaryLt = 9
binaryLte = 10
binaryGt = 11
binaryGte = 12

-- | @OpBinary@ flag values: list concatenation (@++@) and attrset update (@//@).
binaryConcat, binaryUpdate :: Word8
binaryConcat = 13
binaryUpdate = 14

-- | Lambda formal kinds: a plain name, a @{ ... }@ pattern, or a named pattern
-- (@args@@{ ... }@).
formalName, formalSet, formalNamedSet :: Word8
formalName = 0
formalSet = 1
formalNamedSet = 2

-- | String-part kinds: a literal chunk or an interpolated @${...}@ expression.
strpartLit, strpartInterp :: Word32
strpartLit = 0
strpartInterp = 1

-- | Attribute-binding kinds: a @name = value@ binding or an @inherit@.
bindNamed, bindInherit :: Word32
bindNamed = 0
bindInherit = 1

-- | Closure capture kinds: capture nothing, capture specific slots, or capture
-- slots plus the enclosing @with@ scopes.
captureNone, captureSlots, captureWithScopes :: Word32
captureNone = 0
captureSlots = 1
captureWithScopes = 2

-- | Attribute-key kinds: a static (compile-time) key or a dynamic @${...}@ key.
attrkeyStatic, attrkeyDynamic :: Word32
attrkeyStatic = 0
attrkeyDynamic = 1
