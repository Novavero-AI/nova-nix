/*
 * nn_thunk.c - Arena-allocated thunk memoization cells.
 *
 * Chunked bump allocator: linked list of blocks, each containing a
 * contiguous array of nn_thunk_t slots.  When the current block is
 * full, a new block is allocated (same capacity).  Pointers into any
 * block remain valid until the arena is destroyed.
 *
 * Memory layout per block:
 *   [nn_thunk_block header] [slot 0] [slot 1] ... [slot N-1]
 *
 * Block header uses C99 flexible array member for the slots array,
 * so the entire block (header + slots) is one contiguous allocation.
 */

#include "nn_thunk.h"
#include "nn_env.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* --- Constants --- */

#define NN_THUNK_DEFAULT_BLOCK_CAPACITY 65536u  /* 64k thunks = 1 MB */

/* --- Internal types --- */

struct nn_thunk_block {
    struct nn_thunk_block *next;      /* NULL for newest block */
    uint32_t               count;     /* slots used in this block */
    uint32_t               capacity;  /* total slots in this block */
    nn_thunk_t             slots[];   /* C99 flexible array member */
};

struct nn_thunk_arena {
    struct nn_thunk_block *head;           /* first block (for iteration) */
    struct nn_thunk_block *current;        /* newest block (for allocation) */
    uint32_t               block_capacity; /* slots per new block */
};

/* --- Global arena --- */

static struct nn_thunk_arena *g_arena = NULL;

/* --- Internal helpers --- */

static struct nn_thunk_block *
alloc_block(uint32_t capacity)
{
    struct nn_thunk_block *block = (struct nn_thunk_block *)malloc(
        sizeof(struct nn_thunk_block) + (size_t)capacity * sizeof(nn_thunk_t));
    if (!block) return NULL;
    block->next = NULL;
    block->count = 0;
    block->capacity = capacity;
    return block;
}

static nn_thunk_t *
arena_alloc(struct nn_thunk_arena *arena)
{
    /* No arena before nn_thunk_init or after nn_thunk_destroy.  Report
     * it through the NULL the callers already check for rather than
     * dereferencing it; nn_arena_live is how the Haskell boundary tells
     * this apart from exhaustion. */
    if (!arena) return NULL;

    if (arena->current->count >= arena->current->capacity) {
        struct nn_thunk_block *block = alloc_block(arena->block_capacity);
        if (!block) return NULL;
        arena->current->next = block;
        arena->current = block;
    }
    nn_thunk_t *t = &arena->current->slots[arena->current->count];
    arena->current->count++;
    return t;
}

/* --- Lifecycle --- */

void
nn_thunk_init(uint32_t initial_capacity)
{
    if (g_arena) {
        nn_thunk_destroy();
    }

    uint32_t cap = initial_capacity > 0 ? initial_capacity
                                        : NN_THUNK_DEFAULT_BLOCK_CAPACITY;

    /* Fail loudly at startup rather than start with no arena, which
     * every allocation would then refuse (same policy as nn_env_init). */
    g_arena = (struct nn_thunk_arena *)malloc(sizeof(struct nn_thunk_arena));
    if (!g_arena) {
        fprintf(stderr, "nn_thunk_init: arena alloc failed\n");
        abort();
    }

    struct nn_thunk_block *first = alloc_block(cap);
    if (!first) {
        fprintf(stderr, "nn_thunk_init: block alloc failed\n");
        abort();
    }

    g_arena->head = first;
    g_arena->current = first;
    g_arena->block_capacity = cap;
}

int
nn_thunk_live(void)
{
    return g_arena != NULL;
}

void
nn_thunk_destroy(void)
{
    if (!g_arena) return;

    struct nn_thunk_block *block = g_arena->head;
    while (block) {
        struct nn_thunk_block *next = block->next;
        free(block);
        block = next;
    }

    free(g_arena);
    g_arena = NULL;
}

/* --- Allocation --- */

nn_thunk_t *
nn_thunk_new_bc(uint32_t bc_idx, void *env)
{
    nn_thunk_t *t;
    if (bc_idx == NN_THUNK_NO_CODE) return NULL;
    t = arena_alloc(g_arena);
    if (!t) return NULL;
    t->state = NN_THUNK_PENDING;
    t->val_tag = 0;
    t->form = NN_PENDING_CODE;
    t->_pad = 0;
    t->bc_idx = bc_idx;
    t->payload = env;
    return t;
}

/* A PENDING application of the `count` cells at `cells`, the function's
 * first, copied into an array of the env pages. */
static nn_thunk_t *
new_apply(nn_thunk_t *const *cells, uint32_t count, uint8_t form)
{
    uint32_t i;
    void **held;
    nn_thunk_t *t;
    for (i = 0; i < count; i++)
        if (!cells[i]) return NULL;
    held = nn_env_alloc_app_cells(count);
    if (!held) return NULL;
    t = arena_alloc(g_arena);
    if (!t) return NULL;
    for (i = 0; i < count; i++) held[i] = cells[i];
    t->state = NN_THUNK_PENDING;
    t->val_tag = 0;
    t->form = form;
    t->_pad = 0;
    t->bc_idx = 0;
    t->payload = held;
    return t;
}

nn_thunk_t *
nn_thunk_new_apply1(nn_thunk_t *fn, nn_thunk_t *arg)
{
    nn_thunk_t *cells[2];
    cells[0] = fn;
    cells[1] = arg;
    return new_apply(cells, 2, NN_PENDING_APPLY1);
}

nn_thunk_t *
nn_thunk_new_apply2(nn_thunk_t *fn, nn_thunk_t *first, nn_thunk_t *second)
{
    nn_thunk_t *cells[3];
    cells[0] = fn;
    cells[1] = first;
    cells[2] = second;
    return new_apply(cells, 3, NN_PENDING_APPLY2);
}

uint32_t
nn_thunk_get_bc_idx(const nn_thunk_t *thunk)
{
    return thunk->form == NN_PENDING_CODE ? thunk->bc_idx : NN_THUNK_NO_CODE;
}

uint8_t
nn_thunk_pending_form(const nn_thunk_t *thunk)
{
    return thunk->form;
}

nn_thunk_t *
nn_thunk_apply_cell(const nn_thunk_t *thunk, uint32_t i)
{
    if (thunk->state == NN_THUNK_COMPUTED) return NULL;
    if (thunk->form != NN_PENDING_APPLY1 && thunk->form != NN_PENDING_APPLY2) return NULL;
    if (i > thunk->form) return NULL;
    return ((nn_thunk_t *const *)thunk->payload)[i];
}

nn_thunk_t *
nn_thunk_new_computed_int(int64_t value)
{
    nn_thunk_t *t = arena_alloc(g_arena);
    if (!t) return NULL;
    t->state = NN_THUNK_COMPUTED;
    t->val_tag = NN_VALUE_INT;
    t->form = 0;
    t->_pad = 0;
    t->bc_idx = 0;
    t->payload = (void *)(intptr_t)value;
    return t;
}

nn_thunk_t *
nn_thunk_new_computed_float(double value)
{
    nn_thunk_t *t = arena_alloc(g_arena);
    if (!t) return NULL;
    t->state = NN_THUNK_COMPUTED;
    t->val_tag = NN_VALUE_FLOAT;
    t->form = 0;
    t->_pad = 0;
    t->bc_idx = 0;
    memcpy(&t->payload, &value, sizeof(double));
    return t;
}

nn_thunk_t *
nn_thunk_new_computed_primop(uint32_t primop)
{
    nn_thunk_t *t = arena_alloc(g_arena);
    if (!t) return NULL;
    t->state = NN_THUNK_COMPUTED;
    t->val_tag = NN_VALUE_PRIMOP;
    t->form = 0;
    t->_pad = 0;
    t->bc_idx = 0;
    t->payload = (void *)(uintptr_t)primop;
    return t;
}

nn_thunk_t *
nn_thunk_new_computed_str(void *str)
{
    nn_thunk_t *t = arena_alloc(g_arena);
    if (!t) return NULL;
    t->state = NN_THUNK_COMPUTED;
    t->val_tag = NN_VALUE_STR;
    t->form = 0;
    t->_pad = 0;
    t->bc_idx = 0;
    t->payload = str;
    return t;
}

nn_thunk_t *
nn_thunk_new_computed_path(void *str)
{
    nn_thunk_t *t = arena_alloc(g_arena);
    if (!t) return NULL;
    t->state = NN_THUNK_COMPUTED;
    t->val_tag = NN_VALUE_PATH;
    t->form = 0;
    t->_pad = 0;
    t->bc_idx = 0;
    t->payload = str;
    return t;
}

nn_thunk_t *
nn_thunk_new_computed_list(void *list)
{
    nn_thunk_t *t = arena_alloc(g_arena);
    if (!t) return NULL;
    t->state = NN_THUNK_COMPUTED;
    t->val_tag = NN_VALUE_LIST;
    t->form = 0;
    t->_pad = 0;
    t->bc_idx = 0;
    t->payload = list;
    return t;
}

nn_thunk_t *
nn_thunk_new_computed_attrs(void *attrset)
{
    nn_thunk_t *t = arena_alloc(g_arena);
    if (!t) return NULL;
    t->state = NN_THUNK_COMPUTED;
    t->val_tag = NN_VALUE_ATTRS;
    t->form = 0;
    t->_pad = 0;
    t->bc_idx = 0;
    t->payload = attrset;
    return t;
}

nn_thunk_t *
nn_thunk_new_computed_ctxstr(void *ctxstr)
{
    nn_thunk_t *t = arena_alloc(g_arena);
    if (!t) return NULL;
    t->state = NN_THUNK_COMPUTED;
    t->val_tag = NN_VALUE_CTXSTR;
    t->form = 0;
    t->_pad = 0;
    t->bc_idx = 0;
    t->payload = ctxstr;
    return t;
}

nn_thunk_t *
nn_thunk_new_computed_lambda(void *lambda)
{
    nn_thunk_t *t = arena_alloc(g_arena);
    if (!t) return NULL;
    t->state = NN_THUNK_COMPUTED;
    t->val_tag = NN_VALUE_LAMBDA;
    t->form = 0;
    t->_pad = 0;
    t->bc_idx = 0;
    t->payload = lambda;
    return t;
}

nn_thunk_t *
nn_thunk_new_computed_primop_app(void *app)
{
    nn_thunk_t *t = arena_alloc(g_arena);
    if (!t) return NULL;
    t->state = NN_THUNK_COMPUTED;
    t->val_tag = NN_VALUE_PRIMOP_APP;
    t->form = 0;
    t->_pad = 0;
    t->bc_idx = 0;
    t->payload = app;
    return t;
}

/* --- Shared constants --- */

static nn_thunk_t g_null_cell  = { NN_THUNK_COMPUTED, NN_VALUE_NULL, 0, 0, 0, NULL };
static nn_thunk_t g_true_cell  = { NN_THUNK_COMPUTED, NN_VALUE_BOOL, 0, 0, 0, (void *)(intptr_t)1 };
static nn_thunk_t g_false_cell = { NN_THUNK_COMPUTED, NN_VALUE_BOOL, 0, 0, 0, NULL };
/* An empty list is the NULL list (nn_list.h). */
static nn_thunk_t g_empty_list_cell = { NN_THUNK_COMPUTED, NN_VALUE_LIST, 0, 0, 0, NULL };

nn_thunk_t *
nn_thunk_null(void)
{
    return &g_null_cell;
}

nn_thunk_t *
nn_thunk_bool(uint8_t value)
{
    return value ? &g_true_cell : &g_false_cell;
}

nn_thunk_t *
nn_thunk_empty_list(void)
{
    return &g_empty_list_cell;
}

/* Zero until asked for, which is PENDING: nn_thunk_primop fills a cell
 * before it hands it out. */
static nn_thunk_t g_primop_cells[NN_PRIMOP_CELLS];

nn_thunk_t *
nn_thunk_primop(uint32_t primop)
{
    nn_thunk_t *cell;
    if (primop >= NN_PRIMOP_CELLS) return NULL;
    cell = &g_primop_cells[primop];
    if (cell->state != NN_THUNK_COMPUTED) {
        cell->val_tag = NN_VALUE_PRIMOP;
        cell->payload = (void *)(uintptr_t)primop;
        cell->state = NN_THUNK_COMPUTED;
    }
    return cell;
}

int
nn_thunk_is_static(const nn_thunk_t *thunk)
{
    /* Compared as integers: the cell may be in no array of these. */
    uintptr_t p = (uintptr_t)thunk;
    uintptr_t cells = (uintptr_t)g_primop_cells;
    return thunk == &g_null_cell || thunk == &g_true_cell || thunk == &g_false_cell
        || thunk == &g_empty_list_cell
        || (p >= cells && p - cells < sizeof(g_primop_cells));
}

/* --- State queries --- */

uint8_t
nn_thunk_state(const nn_thunk_t *thunk)
{
    return thunk->state;
}

void *
nn_thunk_payload(const nn_thunk_t *thunk)
{
    return thunk->payload;
}

uint8_t
nn_thunk_value_tag(const nn_thunk_t *thunk)
{
    return thunk->val_tag;
}

int64_t
nn_thunk_get_int(const nn_thunk_t *thunk)
{
    return (int64_t)(intptr_t)thunk->payload;
}

double
nn_thunk_get_float(const nn_thunk_t *thunk)
{
    double result;
    memcpy(&result, &thunk->payload, sizeof(double));
    return result;
}

uint8_t
nn_thunk_get_bool(const nn_thunk_t *thunk)
{
    return (uint8_t)(intptr_t)thunk->payload;
}

void *
nn_thunk_get_str(const nn_thunk_t *thunk)
{
    return thunk->payload;
}

void *
nn_thunk_get_path(const nn_thunk_t *thunk)
{
    return thunk->payload;
}

void *
nn_thunk_get_list(const nn_thunk_t *thunk)
{
    return thunk->payload;
}

void *
nn_thunk_get_attrs(const nn_thunk_t *thunk)
{
    return thunk->payload;
}

void *
nn_thunk_get_ctxstr(const nn_thunk_t *thunk)
{
    return thunk->payload;
}

void *
nn_thunk_get_lambda(const nn_thunk_t *thunk)
{
    return thunk->payload;
}

uint32_t
nn_thunk_get_primop(const nn_thunk_t *thunk)
{
    return (uint32_t)(uintptr_t)thunk->payload;
}

void *
nn_thunk_get_primop_app(const nn_thunk_t *thunk)
{
    return thunk->payload;
}

/* --- State transitions --- */

int
nn_thunk_mark_blackhole(nn_thunk_t *thunk)
{
    if (thunk->state != NN_THUNK_PENDING) return 0;
    thunk->state = NN_THUNK_BLACKHOLE;
    return 1;
}

/* Restore BLACKHOLE -> PENDING after a force threw and was caught upstream.
 * mark_blackhole only flips the state byte, so payload + bc_idx are intact and
 * the thunk is fully re-forceable; a later force re-evaluates rather than
 * misreporting infinite recursion.  Returns 1 on success, 0 if not BLACKHOLE. */
int
nn_thunk_mark_pending(nn_thunk_t *thunk)
{
    if (thunk->state != NN_THUNK_BLACKHOLE) return 0;
    thunk->state = NN_THUNK_PENDING;
    return 1;
}

int
nn_thunk_set_computed_int(nn_thunk_t *thunk, int64_t value)
{
    if (thunk->state == NN_THUNK_COMPUTED) return 0;
    thunk->state = NN_THUNK_COMPUTED;
    thunk->val_tag = NN_VALUE_INT;
    thunk->payload = (void *)(intptr_t)value;
    return 1;
}

int
nn_thunk_set_computed_float(nn_thunk_t *thunk, double value)
{
    if (thunk->state == NN_THUNK_COMPUTED) return 0;
    thunk->state = NN_THUNK_COMPUTED;
    thunk->val_tag = NN_VALUE_FLOAT;
    memcpy(&thunk->payload, &value, sizeof(double));
    return 1;
}

int
nn_thunk_set_computed_bool(nn_thunk_t *thunk, uint8_t value)
{
    if (thunk->state == NN_THUNK_COMPUTED) return 0;
    thunk->state = NN_THUNK_COMPUTED;
    thunk->val_tag = NN_VALUE_BOOL;
    thunk->payload = (void *)(intptr_t)value;
    return 1;
}

int
nn_thunk_set_computed_null(nn_thunk_t *thunk)
{
    if (thunk->state == NN_THUNK_COMPUTED) return 0;
    thunk->state = NN_THUNK_COMPUTED;
    thunk->val_tag = NN_VALUE_NULL;
    thunk->payload = NULL;
    return 1;
}

int
nn_thunk_set_computed_str(nn_thunk_t *thunk, void *str)
{
    if (thunk->state == NN_THUNK_COMPUTED) return 0;
    thunk->state = NN_THUNK_COMPUTED;
    thunk->val_tag = NN_VALUE_STR;
    thunk->payload = str;
    return 1;
}

int
nn_thunk_set_computed_path(nn_thunk_t *thunk, void *str)
{
    if (thunk->state == NN_THUNK_COMPUTED) return 0;
    thunk->state = NN_THUNK_COMPUTED;
    thunk->val_tag = NN_VALUE_PATH;
    thunk->payload = str;
    return 1;
}

int
nn_thunk_set_computed_list(nn_thunk_t *thunk, void *list)
{
    if (thunk->state == NN_THUNK_COMPUTED) return 0;
    thunk->state = NN_THUNK_COMPUTED;
    thunk->val_tag = NN_VALUE_LIST;
    thunk->payload = list;
    return 1;
}

int
nn_thunk_set_computed_attrs(nn_thunk_t *thunk, void *attrset)
{
    if (thunk->state == NN_THUNK_COMPUTED) return 0;
    thunk->state = NN_THUNK_COMPUTED;
    thunk->val_tag = NN_VALUE_ATTRS;
    thunk->payload = attrset;
    return 1;
}

int
nn_thunk_set_computed_ctxstr(nn_thunk_t *thunk, void *ctxstr)
{
    if (thunk->state == NN_THUNK_COMPUTED) return 0;
    thunk->state = NN_THUNK_COMPUTED;
    thunk->val_tag = NN_VALUE_CTXSTR;
    thunk->payload = ctxstr;
    return 1;
}

int
nn_thunk_set_computed_lambda(nn_thunk_t *thunk, void *lambda)
{
    if (thunk->state == NN_THUNK_COMPUTED) return 0;
    thunk->state = NN_THUNK_COMPUTED;
    thunk->val_tag = NN_VALUE_LAMBDA;
    thunk->payload = lambda;
    return 1;
}

int
nn_thunk_set_computed_primop(nn_thunk_t *thunk, uint32_t primop)
{
    if (thunk->state == NN_THUNK_COMPUTED) return 0;
    thunk->state = NN_THUNK_COMPUTED;
    thunk->val_tag = NN_VALUE_PRIMOP;
    thunk->payload = (void *)(uintptr_t)primop;
    return 1;
}

int
nn_thunk_set_computed_primop_app(nn_thunk_t *thunk, void *app)
{
    if (thunk->state == NN_THUNK_COMPUTED) return 0;
    thunk->state = NN_THUNK_COMPUTED;
    thunk->val_tag = NN_VALUE_PRIMOP_APP;
    thunk->payload = app;
    return 1;
}

/* --- Census --- */

uint32_t
nn_thunk_blocks(nn_thunk_t **slots, uint32_t *counts, uint32_t max)
{
    uint32_t n = 0;
    struct nn_thunk_block *block;
    if (!g_arena) return 0;
    for (block = g_arena->head; block; block = block->next) {
        if (n < max) {
            slots[n] = block->slots;
            counts[n] = block->count;
        }
        n++;
    }
    return n;
}

uint64_t
nn_thunk_reserved_bytes(void)
{
    uint64_t bytes = 0;
    struct nn_thunk_block *block;
    if (!g_arena) return 0;
    bytes += sizeof(struct nn_thunk_arena);
    for (block = g_arena->head; block; block = block->next) {
        bytes += sizeof(struct nn_thunk_block)
               + (uint64_t)block->capacity * sizeof(nn_thunk_t);
    }
    return bytes;
}
