/*
 * nn_bytecode.c -- Flat bytecode storage for Nix expressions.
 *
 * Three growable arrays:
 *   - g_ops:     nn_op_t instructions (16 bytes each), indexed by uint32
 *   - g_data:    uint32_t values for variable-length operands
 *   - g_strings: the code's strings, indexed by uint32
 *
 * All use realloc-doubling.  Write-once during compilation, random
 * access during evaluation.  Indices remain stable across realloc, and
 * the strings the table points at live in an arena, so they never move.
 */

#include "nn_bytecode.h"
#include "nn_assert.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* --- Constants --- */

#define NN_BC_OP_INITIAL_CAPACITY      65536u
#define NN_BC_DATA_INITIAL_CAPACITY   131072u
#define NN_BC_STRING_INITIAL_CAPACITY   4096u

/* --- Global state --- */

static nn_op_t  *g_ops          = NULL;
static uint32_t  g_op_count     = 0;
static uint32_t  g_op_capacity  = 0;

static uint32_t *g_data          = NULL;
static uint32_t  g_data_count    = 0;
static uint32_t  g_data_capacity = 0;

static nn_str_t      **g_strings          = NULL;
static uint32_t        g_string_count     = 0;
static uint32_t        g_string_capacity  = 0;
static nn_str_arena_t  g_string_arena;

/* --- Lifecycle --- */

void nn_bytecode_init(void)
{
    g_ops         = (nn_op_t *)malloc((size_t)NN_BC_OP_INITIAL_CAPACITY * sizeof(nn_op_t));
    if (!g_ops) { fprintf(stderr, "nn_bytecode_init: ops alloc failed\n"); abort(); }
    g_op_count    = 0;
    g_op_capacity = NN_BC_OP_INITIAL_CAPACITY;

    g_data          = (uint32_t *)malloc((size_t)NN_BC_DATA_INITIAL_CAPACITY * sizeof(uint32_t));
    if (!g_data) { fprintf(stderr, "nn_bytecode_init: data alloc failed\n"); abort(); }
    g_data_count    = 0;
    g_data_capacity = NN_BC_DATA_INITIAL_CAPACITY;

    g_strings         = (nn_str_t **)malloc((size_t)NN_BC_STRING_INITIAL_CAPACITY * sizeof(nn_str_t *));
    if (!g_strings) { fprintf(stderr, "nn_bytecode_init: strings alloc failed\n"); abort(); }
    g_string_count    = 0;
    g_string_capacity = NN_BC_STRING_INITIAL_CAPACITY;
}

int nn_bytecode_live(void)
{
    return g_ops != NULL;
}

void nn_bytecode_destroy(void)
{
    free(g_ops);
    g_ops         = NULL;
    g_op_count    = 0;
    g_op_capacity = 0;

    free(g_data);
    g_data          = NULL;
    g_data_count    = 0;
    g_data_capacity = 0;

    free(g_strings);
    g_strings         = NULL;
    g_string_count    = 0;
    g_string_capacity = 0;
    nn_str_arena_free(&g_string_arena);
}

/* --- Internal: grow arrays --- */

/* Room for one more element in an array of *cap elements of `size`
 * bytes, `count` of them used: the array, or the array moved to double
 * its capacity.  NULL, leaving the array and *cap as they were, for no
 * array (before nn_bytecode_init or after nn_bytecode_destroy), when
 * doubling would pass UINT32_MAX elements, or when the allocator
 * refuses.  A capacity stops at 2^31, so UINT32_MAX, the emitters'
 * failure answer, is never an index. */
static void *reserve(void *items, uint32_t count, uint32_t *cap, size_t size)
{
    void *grown;
    if (!items) return NULL;
    if (count < *cap) return items;
    if (*cap > UINT32_MAX / 2) return NULL;
    grown = realloc(items, (size_t)*cap * 2 * size);
    if (grown) *cap *= 2;
    return grown;
}

/* --- Emit --- */

uint32_t nn_bc_emit(uint8_t opcode, uint8_t flags, uint16_t short_arg,
                    uint32_t arg1, uint32_t arg2, uint32_t arg3)
{
    nn_op_t *ops = (nn_op_t *)reserve(g_ops, g_op_count, &g_op_capacity, sizeof(nn_op_t));
    if (!ops) return UINT32_MAX;
    g_ops = ops;
    uint32_t idx = g_op_count++;
    nn_op_t *op  = &g_ops[idx];
    op->opcode    = opcode;
    op->flags     = flags;
    op->short_arg = short_arg;
    op->arg1      = arg1;
    op->arg2      = arg2;
    op->arg3      = arg3;
    return idx;
}

uint32_t nn_bc_emit_data(uint32_t value)
{
    uint32_t *data = (uint32_t *)reserve(g_data, g_data_count, &g_data_capacity, sizeof(uint32_t));
    if (!data) return UINT32_MAX;
    g_data = data;
    uint32_t offset = g_data_count++;
    g_data[offset]  = value;
    return offset;
}

uint32_t nn_bc_emit_string(const char *bytes, size_t len)
{
    nn_str_t **strings = (nn_str_t **)reserve(g_strings, g_string_count, &g_string_capacity, sizeof(nn_str_t *));
    if (!strings) return UINT32_MAX;
    g_strings = strings;
    nn_str_t *s = nn_str_arena_new(&g_string_arena, bytes, len);
    if (!s) return UINT32_MAX;
    uint32_t index = g_string_count++;
    g_strings[index] = s;
    return index;
}

/* --- Read instructions --- */

uint8_t  nn_bc_opcode(uint32_t idx)    { NN_ASSERT(idx < g_op_count, "nn_bc_opcode: idx out of bounds"); return g_ops[idx].opcode;    }
uint8_t  nn_bc_flags(uint32_t idx)     { NN_ASSERT(idx < g_op_count, "nn_bc_flags: idx out of bounds"); return g_ops[idx].flags;     }
uint16_t nn_bc_short_arg(uint32_t idx) { NN_ASSERT(idx < g_op_count, "nn_bc_short_arg: idx out of bounds"); return g_ops[idx].short_arg; }
uint32_t nn_bc_arg1(uint32_t idx)      { NN_ASSERT(idx < g_op_count, "nn_bc_arg1: idx out of bounds"); return g_ops[idx].arg1;      }
uint32_t nn_bc_arg2(uint32_t idx)      { NN_ASSERT(idx < g_op_count, "nn_bc_arg2: idx out of bounds"); return g_ops[idx].arg2;      }
uint32_t nn_bc_arg3(uint32_t idx)      { NN_ASSERT(idx < g_op_count, "nn_bc_arg3: idx out of bounds"); return g_ops[idx].arg3;      }

/* --- Read data --- */

uint32_t nn_bc_data(uint32_t offset)   { NN_ASSERT(offset < g_data_count, "nn_bc_data: offset out of bounds"); return g_data[offset]; }

/* The index comes from a data word or an argument the compiler wrote, so
 * only a corrupt store holds one out of range; it answers NULL, which the
 * Haskell boundary raises as CInvariantViolated. */
nn_str_t *nn_bc_string(uint32_t index)
{
    return index < g_string_count ? g_strings[index] : NULL;
}

/* --- Diagnostics --- */

uint32_t nn_bc_op_count(void)   { return g_op_count;   }
uint32_t nn_bc_data_count(void) { return g_data_count; }

uint64_t nn_bc_reserved_bytes(void)
{
    return (uint64_t)g_op_capacity * sizeof(nn_op_t)
         + (uint64_t)g_data_capacity * sizeof(uint32_t)
         + (uint64_t)g_string_capacity * sizeof(nn_str_t *)
         + nn_str_arena_reserved_bytes(&g_string_arena);
}

const nn_str_arena_t *nn_bc_strings(void)
{
    return &g_string_arena;
}
