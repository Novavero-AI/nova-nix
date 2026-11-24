{-# LANGUAGE PatternSynonyms #-}

-- | C-backed bytecode storage via FFI.
--
-- Wraps @cbits/nn_bytecode.c@ - growable arrays storing flat Nix
-- expression bytecode: an @nn_op_t[]@ instruction array (16 bytes each),
-- a @uint32_t[]@ data buffer for variable-length operands, and a table of
-- the code's strings, the bytes of its literals, which an instruction or
-- a data word names by index.  The code's strings live as long as the
-- code, so a value a literal evaluates to can point at one rather than
-- copy it.
--
-- In an evaluation session ('Nix.Eval.IO.withEvalSession'):
--
-- @
-- idx <- cbcEmit OpLitInt 0 0 42 0 0
-- cbcOpcode idx   -- OpLitInt (0)
-- cbcArg1 idx     -- 42
-- @
module Nix.Eval.CBytecode
  ( -- * Emit
    cbcEmit,
    cbcEmitData,
    cbcEmitString,
    cbcEmitText,

    -- * Read instructions
    cbcOpcode,
    cbcFlags,
    cbcShortArg,
    cbcArg1,
    cbcArg2,
    cbcArg3,

    -- * Read data
    cbcData,

    -- * Read strings
    cbcString,
    cbcStringBytes,
    cbcStringText,

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
    pattern OpPathStr,
    pattern OpPos,

    -- * Attribute set flags
    attrsPlain,
    attrsRec,
    attrsRecOverrides,

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

    -- * Formal default kinds
    formalNoDefault,
    formalDefault,
    formalSharedDefault,

    -- * String part tags
    strpartLit,
    strpartInterp,
    strpartEsc,

    -- * Binding type tags
    bindNamed,
    bindInherit,
    bindInheritFrom,

    -- * CaptureInfo type tags
    captureNone,
    captureSlots,
    captureWithScopes,

    -- * Attr key tags
    attrkeyStatic,
    attrkeyDynamic,
  )
where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Unsafe as BSU
import Data.Text (Text)
import qualified Data.Text.Foreign as TF
import Data.Word (Word16, Word32, Word8)
import Foreign.C.Types (CChar, CSize (..))
import Foreign.Ptr (Ptr, nullPtr)
import Nix.Eval.CData (MonadCData, cdataIO)
import Nix.Eval.CStatus (cInvariantViolated, cStatusFailure)
import Nix.Eval.CStr (CStrPtr, cstrBytes, cstrText)

-- ---------------------------------------------------------------------------
-- FFI imports (all unsafe - no callbacks, fast data access)
-- ---------------------------------------------------------------------------

foreign import ccall unsafe "nn_bc_emit"
  c_nn_bc_emit :: Word8 -> Word8 -> Word16 -> Word32 -> Word32 -> Word32 -> IO Word32

foreign import ccall unsafe "nn_bc_emit_data"
  c_nn_bc_emit_data :: Word32 -> IO Word32

foreign import ccall unsafe "nn_bc_emit_string"
  c_nn_bc_emit_string :: Ptr CChar -> CSize -> IO Word32

foreign import ccall unsafe "nn_bc_string"
  c_nn_bc_string :: Word32 -> IO CStrPtr

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
-- Emit
-- ---------------------------------------------------------------------------

-- | Append one instruction.  Returns the instruction index.
cbcEmit :: (MonadCData m) => Word8 -> Word8 -> Word16 -> Word32 -> Word32 -> Word32 -> m Word32
cbcEmit opcode flags shortArg arg1 arg2 arg3 =
  cdataIO $ checkedEmit "nn_bc_emit" =<< c_nn_bc_emit opcode flags shortArg arg1 arg2 arg3

-- | Append one uint32 to the data buffer.  Returns the data offset.
cbcEmitData :: (MonadCData m) => Word32 -> m Word32
cbcEmitData value = cdataIO (checkedEmit "nn_bc_emit_data" =<< c_nn_bc_emit_data value)

-- | Copy bytes into a new string of the code's and answer its index,
-- which 'cbcString' reads back.  'BSU.unsafeUseAsCStringLen' is safe
-- here: @nn_bc_emit_string@ copies the bytes and keeps no pointer to
-- them.
cbcEmitString :: (MonadCData m) => ByteString -> m Word32
cbcEmitString bytes =
  cdataIO . BSU.unsafeUseAsCStringLen bytes $ \(ptr, len) ->
    checkedEmit "nn_bc_emit_string" =<< c_nn_bc_emit_string ptr (fromIntegral len)

-- | 'cbcEmitString' for text's UTF-8 bytes: a path, a URI or a source
-- file's path, which the evaluator reads back as text ('cbcStringText').
cbcEmitText :: (MonadCData m) => Text -> m Word32
cbcEmitText text =
  cdataIO . TF.withCStringLen text $ \(ptr, len) ->
    checkedEmit "nn_bc_emit_string" =<< c_nn_bc_emit_string ptr (fromIntegral len)

-- | @UINT32_MAX@ from a C emit function signals a failed append (realloc
-- exhaustion or the uint32 index ceiling), not a valid index.  Must stay
-- in lockstep with the emit docs in @cbits\/nn_bytecode.h@.
emitFailedSentinel :: Word32
emitFailedSentinel = 0xFFFFFFFF

-- | Reject the failure sentinel before it can flow onward as an index:
-- the bytecode arrays are global C state, so a failed append leaves
-- nothing to recover, and the sentinel read back as an index would abort
-- at the first read's bound (@NN_ASSERT@) instead of raising here.
checkedEmit :: String -> Word32 -> IO Word32
checkedEmit site idx
  | idx == emitFailedSentinel = cStatusFailure site "bytecode append failed (allocation failure or index ceiling)"
  | otherwise = pure idx

-- ---------------------------------------------------------------------------
-- Read instructions
-- ---------------------------------------------------------------------------

-- | The opcode byte of instruction @i@ (one of the @Op*@ constants).
cbcOpcode :: (MonadCData m) => Word32 -> m Word8
cbcOpcode = cdataIO . c_nn_bc_opcode

-- | The flags byte of instruction @i@ - the sub-type selector (e.g. which
-- unary/binary operator), interpreted via the sub-type-flag constants below.
cbcFlags :: (MonadCData m) => Word32 -> m Word8
cbcFlags = cdataIO . c_nn_bc_flags

-- | The inline 16-bit argument of instruction @i@: a Boolean literal's
-- value, the count of a counted payload (string parts, bindings, list
-- elements, an attribute path's names) or 'spilledCountSentinel' for one
-- too long to hold ('cbcCountedPayload' reads either), and 0 for every
-- other instruction.
cbcShortArg :: (MonadCData m) => Word32 -> m Word16
cbcShortArg = cdataIO . c_nn_bc_short_arg

-- | The first 32-bit argument of instruction @i@.
cbcArg1 :: (MonadCData m) => Word32 -> m Word32
cbcArg1 = cdataIO . c_nn_bc_arg1

-- | The second 32-bit argument of instruction @i@.
cbcArg2 :: (MonadCData m) => Word32 -> m Word32
cbcArg2 = cdataIO . c_nn_bc_arg2

-- | The third 32-bit argument of instruction @i@.
cbcArg3 :: (MonadCData m) => Word32 -> m Word32
cbcArg3 = cdataIO . c_nn_bc_arg3

-- ---------------------------------------------------------------------------
-- Read data
-- ---------------------------------------------------------------------------

-- | Read entry @i@ of the bytecode data pool (symbols, string indices,
-- child indices and the rest).
cbcData :: (MonadCData m) => Word32 -> m Word32
cbcData = cdataIO . c_nn_bc_data

-- ---------------------------------------------------------------------------
-- Read strings
-- ---------------------------------------------------------------------------

-- | The code's string at an index 'cbcEmitString' answered.  It lives as
-- long as the code, so a cell may point at it.  The compiler writes every
-- index the evaluator reads, so an index with no string is a corrupt
-- store.
cbcString :: (MonadCData m) => Word32 -> m CStrPtr
cbcString index = cdataIO $ do
  str <- c_nn_bc_string index
  if str == nullPtr
    then cInvariantViolated "nn_bc_string" ("no string at index " ++ show index)
    else pure str

-- | A copy of the bytes of the code's string at an index.
cbcStringBytes :: (MonadCData m) => Word32 -> m ByteString
cbcStringBytes index = cbcString index >>= cstrBytes

-- | The text of the code's string at an index 'cbcEmitText' answered.
cbcStringText :: (MonadCData m) => Word32 -> m Text
cbcStringText index = cbcString index >>= cstrText

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
cbcCountedPayload :: (MonadCData m) => Word32 -> Word32 -> m (Int, Word32)
cbcCountedPayload bcIdx dataOff = cdataIO $ do
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

-- | String opcodes: an ordinary @"..."@ string and an indented @\'\'...''@ string.
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

-- | Interpolated-path opcode: a path holding an antiquotation, such as
-- @.\/a\/${x}@, whose string parts begin with the path's literal head.
pattern OpPathStr :: Word8
pattern OpPathStr = 24

-- | Position opcode: @__curPos@, whose first argument is the index
-- "Nix.Eval.SourcePos" recorded its position at.
pattern OpPos :: Word8
pattern OpPos = 25

-- ---------------------------------------------------------------------------
-- Sub-type flags
-- ---------------------------------------------------------------------------

-- | @OpAttrs@ flag values: a set, a recursive set, or a recursive set
-- that binds @__overrides@ statically, in which upstream makes every
-- static value but an inherited name's a thunk of its own
-- (@ExprAttrs::eval@ in eval.cc).
attrsPlain, attrsRec, attrsRecOverrides :: Word8
attrsPlain = 0
attrsRec = 1
attrsRecOverrides = 2

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

-- | Kinds of a set pattern formal's default, the second of the three
-- words of its entry, the formal's name before it and its code after it:
-- none, code made in the call's frame, or a variable naming a slot of the
-- call's frame whose cell the formal's slot shares
-- ('Nix.Eval.EvalFormals.SharedDefault').  A C lambda's formal entries
-- hold the same words (@nn_formal_entry_t@, cbits/nn_lambda.h).
formalNoDefault, formalDefault, formalSharedDefault :: Word32
formalNoDefault = 0
formalDefault = 1
formalSharedDefault = 2

-- | String-part kinds: a literal chunk, an interpolated @${...}@
-- expression, or an indented-string escape (literal text the indentation
-- scan must not measure).
strpartLit, strpartInterp, strpartEsc :: Word32
strpartLit = 0
strpartInterp = 1
strpartEsc = 2

-- | Attribute-binding kinds: a @name = value@ binding, one name of an
-- @inherit@, or an @inherit (from) ...@.
--
-- A binding list's words are the count of the names it binds statically,
-- the count of its bindings with a dynamic name, the count of its shares
-- and the shares, two words each, then the bindings, each led by its
-- kind.  A share is a binding whose whole value is a variable naming
-- another of the frame's names, which takes that name's cell
-- ('Nix.Eval.Compile.siblingShares'): its slot and the slot whose cell it
-- takes in a frame that holds its names in slots, and their symbols in
-- one that holds them in a name table.
bindNamed, bindInherit, bindInheritFrom :: Word32
bindNamed = 0
bindInherit = 1
bindInheritFrom = 2

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
