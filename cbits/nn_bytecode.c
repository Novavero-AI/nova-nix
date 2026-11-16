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

#define NN_BC_DEFAULT_OP_CAPACITY    65536u
#define NN_BC_DEFAULT_DATA_CAPACITY 131072u
#define NN_BC_STRING_INITIAL_CAPACITY 4096u

/* --- Reserved instructions (layout in nn_bytecode.h) --- */

static const nn_op_t g_reserved_ops[] = {
    { .opcode = NN_OP_RESOLVED_VAR, .arg1 = 0, .arg2 = 0 },
    { .opcode = NN_OP_RESOLVED_VAR, .arg1 = 0, .arg2 = 1 },
    { .opcode = NN_OP_APP, .flags = NN_APP_DEFERRED, .arg1 = 0, .arg2 = 1 },
    { .opcode = NN_OP_RESOLVED_VAR, .arg1 = 0, .arg2 = 2 },
    { .opcode = NN_OP_APP, .flags = NN_APP_DEFERRED,
      .arg1 = NN_BC_RESERVED_APPLY1, .arg2 = 3 },
};

typedef char nn_bc_reserved_count_check_[
    (sizeof g_reserved_ops / sizeof g_reserved_ops[0] == NN_BC_RESERVED_COUNT) ? 1 : -1];

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

void nn_bytecode_init(uint32_t op_capacity, uint32_t data_capacity)
{
    if (g_ops) nn_bytecode_destroy();

    if (op_capacity == 0)   op_capacity   = NN_BC_DEFAULT_OP_CAPACITY;
    if (op_capacity < NN_BC_RESERVED_COUNT) op_capacity = NN_BC_RESERVED_COUNT;
    if (data_capacity == 0) data_capacity = NN_BC_DEFAULT_DATA_CAPACITY;

    g_ops         = (nn_op_t *)malloc((size_t)op_capacity * sizeof(nn_op_t));
    if (!g_ops) { fprintf(stderr, "nn_bytecode_init: ops alloc failed\n"); abort(); }
    memcpy(g_ops, g_reserved_ops, sizeof g_reserved_ops);
    g_op_count    = NN_BC_RESERVED_COUNT;
    g_op_capacity = op_capacity;

    g_data          = (uint32_t *)malloc((size_t)data_capacity * sizeof(uint32_t));
    if (!g_data) { fprintf(stderr, "nn_bytecode_init: data alloc failed\n"); abort(); }
    g_data_count    = 0;
    g_data_capacity = data_capacity;
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

static int ensure_op_space(void)
{
    /* No store before nn_bytecode_init or after nn_bytecode_destroy.
     * Doubling a zero capacity would realloc to zero bytes and the
     * emit would then write past it. */
    if (!g_ops) return -1;
    if (g_op_count < g_op_capacity) return 0;
    uint32_t new_cap = g_op_capacity * 2;
    if (new_cap < g_op_capacity) return -1; /* overflow guard */
    nn_op_t *new_ops = (nn_op_t *)realloc(g_ops, (size_t)new_cap * sizeof(nn_op_t));
    if (!new_ops) return -1;
    g_ops = new_ops;
    g_op_capacity = new_cap;
    return 0;
}

static int ensure_data_space(void)
{
    if (!g_data) return -1;
    if (g_data_count < g_data_capacity) return 0;
    uint32_t new_cap = g_data_capacity * 2;
    if (new_cap < g_data_capacity) return -1; /* overflow guard */
    uint32_t *new_data = (uint32_t *)realloc(g_data, (size_t)new_cap * sizeof(uint32_t));
    if (!new_data) return -1;
    g_data = new_data;
    g_data_capacity = new_cap;
    return 0;
}

/* Room for one more string table entry.  The table starts on the first
 * string, and UINT32_MAX is never an index, as it is the emitters'
 * failure answer. */
static int ensure_string_space(void)
{
    if (!g_ops) return -1;
    if (g_string_count < g_string_capacity) return 0;
    if (g_string_capacity > UINT32_MAX / 2) return -1;
    uint32_t new_cap = g_string_capacity == 0
                           ? NN_BC_STRING_INITIAL_CAPACITY
                           : g_string_capacity * 2;
    nn_str_t **new_strings = (nn_str_t **)realloc(
        g_strings, (size_t)new_cap * sizeof(nn_str_t *));
    if (!new_strings) return -1;
    g_strings = new_strings;
    g_string_capacity = new_cap;
    return 0;
}

/* --- Emit --- */

uint32_t nn_bc_emit(uint8_t opcode, uint8_t flags, uint16_t short_arg,
                    uint32_t arg1, uint32_t arg2, uint32_t arg3)
{
    if (ensure_op_space() != 0) return UINT32_MAX;
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
    if (ensure_data_space() != 0) return UINT32_MAX;
    uint32_t offset = g_data_count++;
    g_data[offset]  = value;
    return offset;
}

uint32_t nn_bc_emit_string(const char *bytes, size_t len)
{
    if (ensure_string_space() != 0) return UINT32_MAX;
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
 * only a corrupt store holds one out of range; the bound stays live under
 * NDEBUG and answers NULL, which the Haskell boundary refuses. */
nn_str_t *nn_bc_string(uint32_t index)
{
    NN_ASSERT(index < g_string_count, "nn_bc_string: index out of bounds");
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
