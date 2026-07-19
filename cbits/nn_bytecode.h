/*
 * nn_bytecode.h -- Flat bytecode representation for Nix expressions.
 *
 * Compiles the 19-constructor Expr AST into a contiguous instruction
 * array (nn_op_t[]) plus a variable-length data buffer (uint32_t[]).
 * Each Expr node maps to exactly one instruction.  Children are
 * referenced by their index in the instruction array (post-order
 * traversal guarantees children < parent).  Variable-length data
 * (string parts, bindings, formals, capture info, attr paths) is
 * stored in the data buffer, referenced by offset from instructions.
 *
 * Goal: eliminate StablePtr (Expr, Env) in pending thunks.  After
 * compilation, pending thunks hold (bc_idx, nn_env_t*) -- both
 * C-native, zero GHC heap pressure.
 *
 * Lifecycle: nn_bytecode_init() before evaluation, nn_bytecode_destroy()
 * after.  Not thread-safe -- single-threaded evaluation only.
 */

#ifndef NN_BYTECODE_H
#define NN_BYTECODE_H

#include <stdint.h>

/* --- Opcodes (one per Expr constructor) --- */

#define NN_OP_LIT_INT        0
#define NN_OP_LIT_FLOAT      1
#define NN_OP_LIT_BOOL       2
#define NN_OP_LIT_NULL       3
#define NN_OP_LIT_URI        4
#define NN_OP_LIT_PATH       5
#define NN_OP_STR            6
#define NN_OP_IND_STR        7
#define NN_OP_VAR            8
#define NN_OP_WITH_VAR       9
#define NN_OP_RESOLVED_VAR  10
#define NN_OP_ATTRS         11
#define NN_OP_LIST          12
#define NN_OP_SELECT        13
#define NN_OP_HAS_ATTR      14
#define NN_OP_APP           15
#define NN_OP_LAMBDA        16
#define NN_OP_LET           17
#define NN_OP_IF            18
#define NN_OP_WITH          19
#define NN_OP_ASSERT        20
#define NN_OP_UNARY         21
#define NN_OP_BINARY        22
#define NN_OP_SEARCH_PATH   23

/* --- UnaryOp flags (NN_OP_UNARY) --- */

#define NN_UNARY_NOT     0
#define NN_UNARY_NEGATE  1

/* --- BinaryOp flags (NN_OP_BINARY) --- */

#define NN_BINARY_ADD     0
#define NN_BINARY_SUB     1
#define NN_BINARY_MUL     2
#define NN_BINARY_DIV     3
#define NN_BINARY_AND     4
#define NN_BINARY_OR      5
#define NN_BINARY_IMPL    6
#define NN_BINARY_EQ      7
#define NN_BINARY_NEQ     8
#define NN_BINARY_LT      9
#define NN_BINARY_LTE    10
#define NN_BINARY_GT     11
#define NN_BINARY_GTE    12
#define NN_BINARY_CONCAT 13
#define NN_BINARY_UPDATE 14

/* --- Formal type flags (NN_OP_LAMBDA) --- */

#define NN_FORMAL_NAME       0
#define NN_FORMAL_SET        1
#define NN_FORMAL_NAMED_SET  2

/* --- String part tags (data buffer) --- */

#define NN_STRPART_LIT    0
#define NN_STRPART_INTERP 1

/* --- Binding type tags (data buffer) --- */

#define NN_BIND_NAMED    0
#define NN_BIND_INHERIT  1

/* --- CaptureInfo type tags (data buffer) --- */

#define NN_CAPTURE_NONE         0
#define NN_CAPTURE_SLOTS        1
#define NN_CAPTURE_WITH_SCOPES  2

/* --- Attr path element tags (data buffer) --- */

#define NN_ATTRKEY_STATIC  0
#define NN_ATTRKEY_DYNAMIC 1

/* --- Instruction (16 bytes, naturally aligned) --- */

typedef struct nn_op {
    uint8_t  opcode;     /* Which Expr constructor */
    uint8_t  flags;      /* Sub-type: BinaryOp, UnaryOp, formal type, etc. */
    uint16_t short_arg;  /* Small immediate: count, bool flag, etc.
                            Payload counts use a spill convention so the
                            op stays 16 bytes: 0xFFFF here means the true
                            count is the FIRST uint32 of the op's data
                            region and the payload starts one word later
                            (emit/decode live on the Haskell side:
                            Nix.Eval.CBytecode.cbcCountedPayload). */
    uint32_t arg1;       /* Child index, symbol, data offset, lo32, etc. */
    uint32_t arg2;       /* Child index, symbol, hi32, etc. */
    uint32_t arg3;       /* Child index, data offset, etc. */
} nn_op_t;

/* --- Lifecycle --- */

/* Initialize the global bytecode store.  op_capacity is initial
 * instruction slots (0 = default 65536).  data_capacity is initial
 * data buffer slots (0 = default 131072).  Both grow automatically. */
void nn_bytecode_init(uint32_t op_capacity, uint32_t data_capacity);

/* Destroy the global bytecode store, freeing all memory. */
void nn_bytecode_destroy(void);

/* --- Emit --- */

/* Append one instruction.  Returns the instruction index, or
 * UINT32_MAX when the array cannot grow (allocation failure or the
 * uint32 index ceiling) - the caller must check before using the
 * result as an index. */
uint32_t nn_bc_emit(uint8_t opcode, uint8_t flags, uint16_t short_arg,
                    uint32_t arg1, uint32_t arg2, uint32_t arg3);

/* Append one uint32 to the data buffer.  Returns the data offset, or
 * UINT32_MAX on failure (same contract as nn_bc_emit). */
uint32_t nn_bc_emit_data(uint32_t value);

/* --- Read instructions --- */

uint8_t  nn_bc_opcode(uint32_t idx);
uint8_t  nn_bc_flags(uint32_t idx);
uint16_t nn_bc_short_arg(uint32_t idx);
uint32_t nn_bc_arg1(uint32_t idx);
uint32_t nn_bc_arg2(uint32_t idx);
uint32_t nn_bc_arg3(uint32_t idx);

/* --- Read data --- */

uint32_t nn_bc_data(uint32_t offset);

/* --- Diagnostics --- */

uint32_t nn_bc_op_count(void);
uint32_t nn_bc_data_count(void);

#endif /* NN_BYTECODE_H */
