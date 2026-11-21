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
#include "nn_assert.h"
#include "nn_env.h"
#include "nn_region.h"

#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* --- Internal types --- */

struct nn_thunk_block {
    struct nn_thunk_block *next;      /* NULL for newest block */
    uint32_t               count;     /* slots used in this block */
    nn_thunk_t             slots[];   /* C99 flexible array member */
};

/* --- Global state --- */

/* The first block, for iteration, and the newest, for allocation: NULL
 * outside the init .. destroy window. */
static struct nn_thunk_block *g_first_block = NULL;
static struct nn_thunk_block *g_current_block = NULL;
static uint64_t g_cells_allocated = 0;

/* --- Internal helpers --- */

static struct nn_thunk_block *
alloc_block(void)
{
    struct nn_thunk_block *block = (struct nn_thunk_block *)malloc(
        sizeof(struct nn_thunk_block)
        + (size_t)NN_THUNK_BLOCK_CELLS * sizeof(nn_thunk_t));
    if (!block) return NULL;
    block->next = NULL;
    block->count = 0;
    if (nn_region_mode)
        nn_region_block(NN_REGION_THUNK_BLOCK, (const char *)block->slots,
                        (uint64_t)NN_THUNK_BLOCK_CELLS * sizeof(nn_thunk_t), 1);
    return block;
}

static nn_thunk_t *
arena_alloc(void)
{
    /* No blocks before nn_thunk_init or after nn_thunk_destroy.  Report
     * it through the NULL the callers already check for rather than
     * dereferencing it; nn_arena_live is how the Haskell boundary tells
     * this apart from exhaustion. */
    if (!g_current_block) return NULL;

    if (g_current_block->count >= NN_THUNK_BLOCK_CELLS) {
        struct nn_thunk_block *block = alloc_block();
        if (!block) return NULL;
        g_current_block->next = block;
        g_current_block = block;
    }
    g_cells_allocated++;
    return &g_current_block->slots[g_current_block->count++];
}

/* --- Lifecycle --- */

void
nn_thunk_init(void)
{
    /* Fail loudly at startup rather than start with no arena, which
     * every allocation would then refuse (same policy as nn_env_init). */
    g_first_block = alloc_block();
    if (!g_first_block) {
        fprintf(stderr, "nn_thunk_init: block alloc failed\n");
        abort();
    }
    g_current_block = g_first_block;
    g_cells_allocated = 0;
}

int
nn_thunk_live(void)
{
    return g_first_block != NULL;
}

void
nn_thunk_destroy(void)
{
    struct nn_thunk_block *block = g_first_block;
    while (block) {
        struct nn_thunk_block *next = block->next;
        free(block);
        block = next;
    }
    g_first_block = NULL;
    g_current_block = NULL;
    g_cells_allocated = 0;
}

/* --- Allocation --- */

nn_thunk_t *
nn_thunk_new_bc(uint32_t bc_idx, void *env)
{
    nn_thunk_t *t;
    NN_ASSERT(bc_idx != NN_THUNK_NO_CODE, "nn_thunk_new_bc: bc_idx is NN_THUNK_NO_CODE");
    t = arena_alloc();
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
        NN_ASSERT(cells[i] != NULL, "nn_thunk_new_apply1/2: a cell is NULL");
    held = nn_env_alloc_app_cells(count);
    if (!held) return NULL;
    t = arena_alloc();
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

/* The caller reads the form first and asks only for a cell the
 * application holds, so any other request is a defect. */
nn_thunk_t *
nn_thunk_apply_cell(const nn_thunk_t *thunk, uint32_t i)
{
    NN_ASSERT(thunk->state != NN_THUNK_COMPUTED,
              "nn_thunk_apply_cell: the thunk is COMPUTED");
    NN_ASSERT(thunk->form == NN_PENDING_APPLY1 || thunk->form == NN_PENDING_APPLY2,
              "nn_thunk_apply_cell: the thunk is no application");
    NN_ASSERT(i <= thunk->form, "nn_thunk_apply_cell: i is past the application's cells");
    return ((nn_thunk_t *const *)thunk->payload)[i];
}

/* A new COMPUTED cell holding `payload` under `tag`, or NULL when the
 * arena cannot hand one out. */
static nn_thunk_t *
new_computed(uint8_t tag, void *payload)
{
    nn_thunk_t *t = arena_alloc();
    if (!t) return NULL;
    t->state = NN_THUNK_COMPUTED;
    t->val_tag = tag;
    t->form = 0;
    t->_pad = 0;
    t->bc_idx = 0;
    t->payload = payload;
    return t;
}

nn_thunk_t *
nn_thunk_new_computed_int(int64_t value)
{
    return new_computed(NN_VALUE_INT, (void *)(intptr_t)value);
}

/* The double's bytes are copied over the made cell's payload, so that no
 * pointer is formed from them. */
nn_thunk_t *
nn_thunk_new_computed_float(double value)
{
    nn_thunk_t *t = new_computed(NN_VALUE_FLOAT, NULL);
    if (t) memcpy(&t->payload, &value, sizeof(double));
    return t;
}

nn_thunk_t *
nn_thunk_new_computed_primop(uint32_t primop)
{
    return new_computed(NN_VALUE_PRIMOP, (void *)(uintptr_t)primop);
}

nn_thunk_t *
nn_thunk_new_computed_str(void *str)
{
    return new_computed(NN_VALUE_STR, str);
}

nn_thunk_t *
nn_thunk_new_computed_path(void *str)
{
    return new_computed(NN_VALUE_PATH, str);
}

nn_thunk_t *
nn_thunk_new_computed_list(void *list)
{
    return new_computed(NN_VALUE_LIST, list);
}

nn_thunk_t *
nn_thunk_new_computed_attrs(void *attrset)
{
    return new_computed(NN_VALUE_ATTRS, attrset);
}

nn_thunk_t *
nn_thunk_new_computed_ctxstr(void *ctxstr)
{
    return new_computed(NN_VALUE_CTXSTR, ctxstr);
}

nn_thunk_t *
nn_thunk_new_computed_lambda(void *lambda)
{
    return new_computed(NN_VALUE_LAMBDA, lambda);
}

nn_thunk_t *
nn_thunk_new_computed_primop_app(void *app)
{
    return new_computed(NN_VALUE_PRIMOP_APP, app);
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

uint32_t
nn_thunk_get_primop(const nn_thunk_t *thunk)
{
    return (uint32_t)(uintptr_t)thunk->payload;
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

/* Whether a value of this tag holds a pointer into the data layer. */
static int
holds_object(uint8_t tag)
{
    switch (tag) {
    case NN_VALUE_INT:
    case NN_VALUE_FLOAT:
    case NN_VALUE_BOOL:
    case NN_VALUE_NULL:
    case NN_VALUE_PRIMOP:
        return 0;
    default:
        return 1;
    }
}

/* Store `payload` under `tag` in a cell not yet COMPUTED: 1, or 0 with
 * the cell unchanged when it already is.  A store of an object is a write
 * of a pointer into a cell that may be older than the force making it,
 * which a census of regions records (nn_region.h). */
static int
set_computed(nn_thunk_t *thunk, uint8_t tag, void *payload)
{
    if (thunk->state == NN_THUNK_COMPUTED) return 0;
    thunk->state = NN_THUNK_COMPUTED;
    thunk->val_tag = tag;
    thunk->payload = payload;
    if (nn_region_mode && holds_object(tag)) nn_region_record_cell(thunk);
    return 1;
}

int
nn_thunk_set_computed_int(nn_thunk_t *thunk, int64_t value)
{
    return set_computed(thunk, NN_VALUE_INT, (void *)(intptr_t)value);
}

/* The double's bytes are copied over the payload once the cell is
 * COMPUTED, as in nn_thunk_new_computed_float. */
int
nn_thunk_set_computed_float(nn_thunk_t *thunk, double value)
{
    if (!set_computed(thunk, NN_VALUE_FLOAT, NULL)) return 0;
    memcpy(&thunk->payload, &value, sizeof(double));
    return 1;
}

int
nn_thunk_set_computed_bool(nn_thunk_t *thunk, uint8_t value)
{
    return set_computed(thunk, NN_VALUE_BOOL, (void *)(intptr_t)value);
}

int
nn_thunk_set_computed_null(nn_thunk_t *thunk)
{
    return set_computed(thunk, NN_VALUE_NULL, NULL);
}

int
nn_thunk_set_computed_str(nn_thunk_t *thunk, void *str)
{
    return set_computed(thunk, NN_VALUE_STR, str);
}

int
nn_thunk_set_computed_path(nn_thunk_t *thunk, void *str)
{
    return set_computed(thunk, NN_VALUE_PATH, str);
}

int
nn_thunk_set_computed_list(nn_thunk_t *thunk, void *list)
{
    return set_computed(thunk, NN_VALUE_LIST, list);
}

int
nn_thunk_set_computed_attrs(nn_thunk_t *thunk, void *attrset)
{
    return set_computed(thunk, NN_VALUE_ATTRS, attrset);
}

int
nn_thunk_set_computed_ctxstr(nn_thunk_t *thunk, void *ctxstr)
{
    return set_computed(thunk, NN_VALUE_CTXSTR, ctxstr);
}

int
nn_thunk_set_computed_lambda(nn_thunk_t *thunk, void *lambda)
{
    return set_computed(thunk, NN_VALUE_LAMBDA, lambda);
}

int
nn_thunk_set_computed_primop(nn_thunk_t *thunk, uint32_t primop)
{
    return set_computed(thunk, NN_VALUE_PRIMOP, (void *)(uintptr_t)primop);
}

int
nn_thunk_set_computed_primop_app(nn_thunk_t *thunk, void *app)
{
    return set_computed(thunk, NN_VALUE_PRIMOP_APP, app);
}

/* --- Census --- */

uint32_t
nn_thunk_blocks(nn_thunk_t **slots, uint32_t *counts, uint32_t max)
{
    uint32_t n = 0;
    struct nn_thunk_block *block;
    for (block = g_first_block; block; block = block->next) {
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
    for (block = g_first_block; block; block = block->next) {
        bytes += sizeof(struct nn_thunk_block)
               + (uint64_t)NN_THUNK_BLOCK_CELLS * sizeof(nn_thunk_t);
    }
    return bytes;
}

uint64_t
nn_thunk_cells_allocated(void)
{
    return g_cells_allocated;
}

uint64_t
nn_thunk_block_used(const char *slots)
{
    const struct nn_thunk_block *block = (const struct nn_thunk_block *)(const void *)(
        slots - offsetof(struct nn_thunk_block, slots));
    return (uint64_t)block->count * sizeof(nn_thunk_t);
}
