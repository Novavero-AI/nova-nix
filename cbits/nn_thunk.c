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
    uint32_t               total;          /* total thunks across all blocks */
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
    /* Current block full - allocate a new one */
    if (arena->current->count >= arena->current->capacity) {
        struct nn_thunk_block *block = alloc_block(arena->block_capacity);
        if (!block) return NULL;
        arena->current->next = block;
        arena->current = block;
    }
    nn_thunk_t *t = &arena->current->slots[arena->current->count];
    arena->current->count++;
    arena->total++;
    return t;
}

/* --- Lifecycle --- */

void
nn_thunk_init(uint32_t initial_capacity)
{
    /* Destroy existing arena if re-initializing */
    if (g_arena) {
        nn_thunk_destroy();
    }

    uint32_t cap = initial_capacity > 0 ? initial_capacity
                                        : NN_THUNK_DEFAULT_BLOCK_CAPACITY;

    /* An allocation failure here would leave g_arena NULL and the first
     * thunk allocation would dereference it - fail loudly at startup
     * instead (same policy as nn_env_init). */
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
    g_arena->total = 0;
    g_arena->block_capacity = cap;
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
nn_thunk_new(void *pending_data)
{
    nn_thunk_t *t = arena_alloc(g_arena);
    if (!t) return NULL;
    t->state = NN_THUNK_PENDING;
    t->val_tag = 0;
    t->_pad = 0;
    t->bc_idx = 0;  /* legacy StablePtr thunk marker */
    t->payload = pending_data;
    return t;
}

nn_thunk_t *
nn_thunk_new_bc(uint32_t bc_idx, void *env_ptr)
{
    nn_thunk_t *t = arena_alloc(g_arena);
    if (!t) return NULL;
    t->state = NN_THUNK_PENDING;
    t->val_tag = 0;
    t->_pad = 0;
    t->bc_idx = bc_idx;
    t->payload = env_ptr;
    return t;
}

uint32_t
nn_thunk_get_bc_idx(const nn_thunk_t *thunk)
{
    return thunk->bc_idx;
}

void
nn_thunk_set_payload(nn_thunk_t *thunk, void *payload)
{
    thunk->payload = payload;
}

nn_thunk_t *
nn_thunk_new_computed(void *value)
{
    nn_thunk_t *t = arena_alloc(g_arena);
    if (!t) return NULL;
    t->state = NN_THUNK_COMPUTED;
    t->val_tag = NN_VALUE_PTR;
    t->_pad = 0;
    t->bc_idx = 0;
    t->payload = value;
    return t;
}

nn_thunk_t *
nn_thunk_new_computed_int(int64_t value)
{
    nn_thunk_t *t = arena_alloc(g_arena);
    if (!t) return NULL;
    t->state = NN_THUNK_COMPUTED;
    t->val_tag = NN_VALUE_INT;
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
    t->_pad = 0;
    t->bc_idx = 0;
    memcpy(&t->payload, &value, sizeof(double));
    return t;
}

nn_thunk_t *
nn_thunk_new_computed_bool(uint8_t value)
{
    nn_thunk_t *t = arena_alloc(g_arena);
    if (!t) return NULL;
    t->state = NN_THUNK_COMPUTED;
    t->val_tag = NN_VALUE_BOOL;
    t->_pad = 0;
    t->bc_idx = 0;
    t->payload = (void *)(intptr_t)value;
    return t;
}

nn_thunk_t *
nn_thunk_new_computed_null(void)
{
    nn_thunk_t *t = arena_alloc(g_arena);
    if (!t) return NULL;
    t->state = NN_THUNK_COMPUTED;
    t->val_tag = NN_VALUE_NULL;
    t->_pad = 0;
    t->bc_idx = 0;
    t->payload = NULL;
    return t;
}

nn_thunk_t *
nn_thunk_new_computed_str(uint32_t symbol)
{
    nn_thunk_t *t = arena_alloc(g_arena);
    if (!t) return NULL;
    t->state = NN_THUNK_COMPUTED;
    t->val_tag = NN_VALUE_STR;
    t->_pad = 0;
    t->bc_idx = 0;
    t->payload = (void *)(intptr_t)symbol;
    return t;
}

nn_thunk_t *
nn_thunk_new_computed_path(uint32_t symbol)
{
    nn_thunk_t *t = arena_alloc(g_arena);
    if (!t) return NULL;
    t->state = NN_THUNK_COMPUTED;
    t->val_tag = NN_VALUE_PATH;
    t->_pad = 0;
    t->bc_idx = 0;
    t->payload = (void *)(intptr_t)symbol;
    return t;
}

nn_thunk_t *
nn_thunk_new_computed_list(void *list)
{
    nn_thunk_t *t = arena_alloc(g_arena);
    if (!t) return NULL;
    t->state = NN_THUNK_COMPUTED;
    t->val_tag = NN_VALUE_LIST;
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
    t->_pad = 0;
    t->bc_idx = 0;
    t->payload = lambda;
    return t;
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

uint32_t
nn_thunk_get_str(const nn_thunk_t *thunk)
{
    return (uint32_t)(intptr_t)thunk->payload;
}

uint32_t
nn_thunk_get_path(const nn_thunk_t *thunk)
{
    return (uint32_t)(intptr_t)thunk->payload;
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

void *
nn_thunk_set_computed(nn_thunk_t *thunk, void *value)
{
    if (thunk->state == NN_THUNK_COMPUTED) return NULL;
    void *old = thunk->payload;
    thunk->state = NN_THUNK_COMPUTED;
    thunk->val_tag = NN_VALUE_PTR;
    thunk->payload = value;
    return old;
}

void *
nn_thunk_set_computed_int(nn_thunk_t *thunk, int64_t value)
{
    if (thunk->state == NN_THUNK_COMPUTED) return NULL;
    void *old = thunk->payload;
    thunk->state = NN_THUNK_COMPUTED;
    thunk->val_tag = NN_VALUE_INT;
    thunk->payload = (void *)(intptr_t)value;
    return old;
}

void *
nn_thunk_set_computed_float(nn_thunk_t *thunk, double value)
{
    if (thunk->state == NN_THUNK_COMPUTED) return NULL;
    void *old = thunk->payload;
    thunk->state = NN_THUNK_COMPUTED;
    thunk->val_tag = NN_VALUE_FLOAT;
    memcpy(&thunk->payload, &value, sizeof(double));
    return old;
}

void *
nn_thunk_set_computed_bool(nn_thunk_t *thunk, uint8_t value)
{
    if (thunk->state == NN_THUNK_COMPUTED) return NULL;
    void *old = thunk->payload;
    thunk->state = NN_THUNK_COMPUTED;
    thunk->val_tag = NN_VALUE_BOOL;
    thunk->payload = (void *)(intptr_t)value;
    return old;
}

void *
nn_thunk_set_computed_null(nn_thunk_t *thunk)
{
    if (thunk->state == NN_THUNK_COMPUTED) return NULL;
    void *old = thunk->payload;
    thunk->state = NN_THUNK_COMPUTED;
    thunk->val_tag = NN_VALUE_NULL;
    thunk->payload = NULL;
    return old;
}

void *
nn_thunk_set_computed_str(nn_thunk_t *thunk, uint32_t symbol)
{
    if (thunk->state == NN_THUNK_COMPUTED) return NULL;
    void *old = thunk->payload;
    thunk->state = NN_THUNK_COMPUTED;
    thunk->val_tag = NN_VALUE_STR;
    thunk->payload = (void *)(intptr_t)symbol;
    return old;
}

void *
nn_thunk_set_computed_path(nn_thunk_t *thunk, uint32_t symbol)
{
    if (thunk->state == NN_THUNK_COMPUTED) return NULL;
    void *old = thunk->payload;
    thunk->state = NN_THUNK_COMPUTED;
    thunk->val_tag = NN_VALUE_PATH;
    thunk->payload = (void *)(intptr_t)symbol;
    return old;
}

void *
nn_thunk_set_computed_list(nn_thunk_t *thunk, void *list)
{
    if (thunk->state == NN_THUNK_COMPUTED) return NULL;
    void *old = thunk->payload;
    thunk->state = NN_THUNK_COMPUTED;
    thunk->val_tag = NN_VALUE_LIST;
    thunk->payload = list;
    return old;
}

void *
nn_thunk_set_computed_attrs(nn_thunk_t *thunk, void *attrset)
{
    if (thunk->state == NN_THUNK_COMPUTED) return NULL;
    void *old = thunk->payload;
    thunk->state = NN_THUNK_COMPUTED;
    thunk->val_tag = NN_VALUE_ATTRS;
    thunk->payload = attrset;
    return old;
}

void *
nn_thunk_set_computed_ctxstr(nn_thunk_t *thunk, void *ctxstr)
{
    if (thunk->state == NN_THUNK_COMPUTED) return NULL;
    void *old = thunk->payload;
    thunk->state = NN_THUNK_COMPUTED;
    thunk->val_tag = NN_VALUE_CTXSTR;
    thunk->payload = ctxstr;
    return old;
}

void *
nn_thunk_set_computed_lambda(nn_thunk_t *thunk, void *lambda)
{
    if (thunk->state == NN_THUNK_COMPUTED) return NULL;
    void *old = thunk->payload;
    thunk->state = NN_THUNK_COMPUTED;
    thunk->val_tag = NN_VALUE_LAMBDA;
    thunk->payload = lambda;
    return old;
}

/* --- Arena diagnostics / cleanup iteration --- */

uint32_t
nn_thunk_count(void)
{
    return g_arena ? g_arena->total : 0;
}

nn_thunk_t *
nn_thunk_get(uint32_t index)
{
    if (!g_arena || index >= g_arena->total) return NULL;

    struct nn_thunk_block *block = g_arena->head;
    while (block) {
        if (index < block->count) {
            return &block->slots[index];
        }
        index -= block->count;
        block = block->next;
    }
    return NULL;  /* unreachable if index < total */
}

/* --- Bulk StablePtr collection (single block-list pass, O(total)) --- */

uint32_t
nn_thunk_count_stableptrs(void)
{
    uint32_t count = 0;
    struct nn_thunk_block *block;
    uint32_t i;
    if (!g_arena) return 0;
    for (block = g_arena->head; block; block = block->next) {
        for (i = 0; i < block->count; i++) {
            const nn_thunk_t *t = &block->slots[i];
            if (t->state == NN_THUNK_PENDING || t->state == NN_THUNK_BLACKHOLE) {
                if (t->payload) count++;
            } else if (t->state == NN_THUNK_COMPUTED && t->val_tag == NN_VALUE_PTR) {
                if (t->payload) count++;
            }
        }
    }
    return count;
}

uint32_t
nn_thunk_collect_stableptrs(void **output, uint32_t max_count)
{
    uint32_t written = 0;
    struct nn_thunk_block *block;
    uint32_t i;
    if (!g_arena) return 0;
    for (block = g_arena->head; block && written < max_count; block = block->next) {
        for (i = 0; i < block->count && written < max_count; i++) {
            const nn_thunk_t *t = &block->slots[i];
            if (t->state == NN_THUNK_PENDING || t->state == NN_THUNK_BLACKHOLE) {
                if (t->payload) output[written++] = t->payload;
            } else if (t->state == NN_THUNK_COMPUTED && t->val_tag == NN_VALUE_PTR) {
                if (t->payload) output[written++] = t->payload;
            }
        }
    }
    return written;
}
