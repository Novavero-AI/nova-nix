/*
 * nn_bytecode.c -- Flat bytecode storage for Nix expressions.
 *
 * Two growable arrays:
 *   - g_ops:  nn_op_t instructions (16 bytes each), indexed by uint32
 *   - g_data: uint32_t values for variable-length operands
 *
 * Both use realloc-doubling.  Write-once during compilation, random
 * access during evaluation.  Indices remain stable across realloc.
 */

#include "nn_bytecode.h"

#include <stdlib.h>
#include <string.h>

/* --- Constants --- */

#define NN_BC_DEFAULT_OP_CAPACITY    65536u
#define NN_BC_DEFAULT_DATA_CAPACITY 131072u

/* --- Global state --- */

static nn_op_t  *g_ops          = NULL;
static uint32_t  g_op_count     = 0;
static uint32_t  g_op_capacity  = 0;

static uint32_t *g_data          = NULL;
static uint32_t  g_data_count    = 0;
static uint32_t  g_data_capacity = 0;

/* --- Lifecycle --- */

void nn_bytecode_init(uint32_t op_capacity, uint32_t data_capacity)
{
    if (op_capacity == 0)   op_capacity   = NN_BC_DEFAULT_OP_CAPACITY;
    if (data_capacity == 0) data_capacity = NN_BC_DEFAULT_DATA_CAPACITY;

    g_ops         = (nn_op_t *)malloc((size_t)op_capacity * sizeof(nn_op_t));
    g_op_count    = 0;
    g_op_capacity = op_capacity;

    g_data          = (uint32_t *)malloc((size_t)data_capacity * sizeof(uint32_t));
    g_data_count    = 0;
    g_data_capacity = data_capacity;
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
}

/* --- Internal: grow arrays --- */

static void ensure_op_space(void)
{
    if (g_op_count < g_op_capacity) return;
    uint32_t new_cap = g_op_capacity * 2;
    g_ops = (nn_op_t *)realloc(g_ops, (size_t)new_cap * sizeof(nn_op_t));
    g_op_capacity = new_cap;
}

static void ensure_data_space(void)
{
    if (g_data_count < g_data_capacity) return;
    uint32_t new_cap = g_data_capacity * 2;
    g_data = (uint32_t *)realloc(g_data, (size_t)new_cap * sizeof(uint32_t));
    g_data_capacity = new_cap;
}

/* --- Emit --- */

uint32_t nn_bc_emit(uint8_t opcode, uint8_t flags, uint16_t short_arg,
                    uint32_t arg1, uint32_t arg2, uint32_t arg3)
{
    ensure_op_space();
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
    ensure_data_space();
    uint32_t offset = g_data_count++;
    g_data[offset]  = value;
    return offset;
}

/* --- Read instructions --- */

uint8_t  nn_bc_opcode(uint32_t idx)    { return g_ops[idx].opcode;    }
uint8_t  nn_bc_flags(uint32_t idx)     { return g_ops[idx].flags;     }
uint16_t nn_bc_short_arg(uint32_t idx) { return g_ops[idx].short_arg; }
uint32_t nn_bc_arg1(uint32_t idx)      { return g_ops[idx].arg1;      }
uint32_t nn_bc_arg2(uint32_t idx)      { return g_ops[idx].arg2;      }
uint32_t nn_bc_arg3(uint32_t idx)      { return g_ops[idx].arg3;      }

/* --- Read data --- */

uint32_t nn_bc_data(uint32_t offset)   { return g_data[offset]; }

/* --- Diagnostics --- */

uint32_t nn_bc_op_count(void)   { return g_op_count;   }
uint32_t nn_bc_data_count(void) { return g_data_count; }
