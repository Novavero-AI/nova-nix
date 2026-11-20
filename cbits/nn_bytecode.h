/*
 * nn_bytecode.h -- Flat bytecode representation for Nix expressions.
 *
 * Compiles the Expr AST into a contiguous instruction array (nn_op_t[])
 * plus a variable-length data buffer (uint32_t[]).
 * Each Expr node maps to exactly one instruction.  Children are
 * referenced by their index in the instruction array (post-order
 * traversal guarantees children < parent).  Variable-length data
 * (string parts, bindings, formals, capture info, attr paths) is
 * stored in the data buffer, referenced by offset from instructions,
 * and so are the source positions bindings refer to.
 *
 * The bytes of the code's literals (a string's literal parts, a URI, a
 * path, a source file's path) are strings of the code's own (nn_str.h),
 * named by their index in a string table: an instruction or data word
 * holds the index, and nn_bc_string answers the string.  A value a
 * literal evaluates to can point at that string rather than copy it,
 * since the code's strings live as long as the code.
 *
 * A pending thunk holds an instruction's index and the nn_env_t * it
 * runs in, both C, so pending code takes nothing on GHC's heap.
 *
 * Lifecycle: nn_bytecode_init() before evaluation, nn_bytecode_destroy()
 * after.  Not thread-safe -- single-threaded evaluation only.
 */

#ifndef NN_BYTECODE_H
#define NN_BYTECODE_H

#include <stddef.h>
#include <stdint.h>

#include "nn_str.h"

/* The opcodes, their flags and the data buffer's tags are the Haskell
 * side's (Nix.Eval.CBytecode), which writes every instruction and reads
 * it back; the store keeps the words without interpreting them. */

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
    uint32_t arg1;       /* Child index, symbol, string index, data offset, lo32, etc. */
    uint32_t arg2;       /* Child index, symbol, hi32, etc. */
    uint32_t arg3;       /* Child index, data offset, etc. */
} nn_op_t;

/* --- Lifecycle --- */

/* Initialize the global bytecode store: the instruction array, the data
 * buffer and the string table, each of which grows as it fills.  Called
 * only while the store is not live (nn_bytecode_live is 0), as a second
 * init would leak the live store.  Outside the init .. destroy window
 * every emitter returns UINT32_MAX. */
void nn_bytecode_init(void);

/* Nonzero between nn_bytecode_init and nn_bytecode_destroy. */
int nn_bytecode_live(void);

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

/* Copy len bytes into a new string of the code's and add it to the string
 * table.  (NULL, 0) is the empty string.  Returns its index, or
 * UINT32_MAX on failure (same contract as nn_bc_emit; a length past
 * NN_STR_MAX_LEN is refused before a byte is read). */
uint32_t nn_bc_emit_string(const char *bytes, size_t len);

/* --- Read instructions --- */

uint8_t  nn_bc_opcode(uint32_t idx);
uint8_t  nn_bc_flags(uint32_t idx);
uint16_t nn_bc_short_arg(uint32_t idx);
uint32_t nn_bc_arg1(uint32_t idx);
uint32_t nn_bc_arg2(uint32_t idx);
uint32_t nn_bc_arg3(uint32_t idx);

/* --- Read data --- */

uint32_t nn_bc_data(uint32_t offset);

/* The string at an index nn_bc_emit_string returned, valid until
 * nn_bytecode_destroy(), or NULL for an index it did not return. */
nn_str_t *nn_bc_string(uint32_t index);

/* --- Diagnostics --- */

uint32_t nn_bc_op_count(void);
uint32_t nn_bc_data_count(void);

/* Bytes the store holds from the allocator: the instruction and data
 * arrays and the string table at their capacities, and the arena of the
 * code's strings (nn_stats.h). */
uint64_t nn_bc_reserved_bytes(void);

/* The arena of the code's strings, which the census places a string in
 * without counting it (nn_stats.h). */
const nn_str_arena_t *nn_bc_strings(void);

#endif /* NN_BYTECODE_H */
