/*
 * nn_attrset.c - Sorted symbol-keyed attribute set.
 *
 * Three parallel arrays: keys (nn_symbol_t), positions (uint32_t) and
 * values (void*), sorted by symbol ID after freeze.  Binary search for
 * lookup.  Keys and positions share one allocation, the keys first and
 * the positions after `capacity` of them, so carrying a position costs
 * no allocation of its own.
 *
 * Memory layout after freeze (example with 4 entries, capacity 4):
 *   keys:      [ 3, 7, 12, 45 ]    (sorted symbol IDs)
 *   positions: [ 9, 0, 13, 17 ]    (parallel, in the same block)
 *   values:    [ p0, p1, p2, p3 ]  (parallel opaque pointers)
 *
 * A layer (nn_attrset_update) holds a right operand's entries in these
 * arrays and points at its base, the set it sits on; its size counts the
 * names of the whole chain, each once, and a walk over them merges the
 * chain's arrays, the topmost entry of a name winning (cursor_next).
 *
 * Against Haskell's Map.Bin tree:
 *   Map.Bin: ~48 bytes/node (constructor + key + value + size + left + right)
 *   nn_attrset: ~16 bytes/entry (4-byte key + 4-byte position + 8-byte pointer)
 *   For 30k entries: 480 KB against 1.4 MB, in one contiguous run
 */

#include "nn_attrset.h"
#include "nn_assert.h"
#include "nn_region.h"
#include "nn_stats.h"

#include <stdlib.h>
#include <string.h>

/* Every set allocated, for bulk cleanup. */
static nn_tracker_t g_tracked;

/* --- Internal representation --- */

struct nn_attrset {
    nn_symbol_t        *keys;      /* the shared block: capacity keys, then positions */
    void              **values;
    const nn_attrset_t *base;      /* the set this one is layered on, or NULL */
    uint32_t            count;     /* entries this set's own arrays hold */
    uint32_t            capacity;  /* entries allocated */
    uint32_t            size;      /* entries the set binds, each name once */
    uint32_t            layers;    /* 0 while built, 1 flat, base's + 1 layered */
};

/* --- Internal types --- */

typedef struct {
    nn_symbol_t key;
    uint32_t    idx;
    uint32_t    pos;
    void       *value;
} nn_tagged_entry_t;

/* --- Forward declarations --- */

static int cmp_tagged(const void *a, const void *b);

/* --- Helpers --- */

/* Bytes of the block holding `capacity` keys and as many positions. */
static size_t names_block_bytes(uint32_t capacity)
{
    return (size_t)capacity * (sizeof(nn_symbol_t) + sizeof(uint32_t));
}

/* The positions, after the `capacity` keys in the shared block.  The
 * shared empty set has no block, and no positions: an offset from its
 * NULL keys would be undefined. */
static uint32_t *positions_of(const nn_attrset_t *set)
{
    return set->keys ? set->keys + set->capacity : NULL;
}

/* Double the capacity, stopping at NN_ATTRSET_MAX_ENTRIES; the doubling
 * is computed in uint64_t so it cannot wrap.  Returns 0, or -1 with the
 * capacity unchanged when the set is at the ceiling or an allocation
 * fails.  The values grow first: a values array that grew is kept, only
 * larger, and the shared block, whose positions sit at the capacity,
 * changes only once nothing else can fail. */
static int grow(nn_attrset_t *set)
{
    if (set->capacity >= NN_ATTRSET_MAX_ENTRIES) return -1;
    uint64_t doubled = (uint64_t)set->capacity * 2;
    uint32_t new_cap = doubled > NN_ATTRSET_MAX_ENTRIES
                           ? NN_ATTRSET_MAX_ENTRIES : (uint32_t)doubled;

    void **new_values = (void **)realloc(
        set->values, (size_t)new_cap * sizeof(void *));
    if (!new_values) return -1;
    set->values = new_values;

    nn_symbol_t *new_keys = (nn_symbol_t *)realloc(
        set->keys, names_block_bytes(new_cap));
    if (!new_keys) return -1;
    memmove(new_keys + new_cap, new_keys + set->capacity,
            (size_t)set->count * sizeof(uint32_t));
    set->keys = new_keys;

    uint64_t before = nn_attrset_bytes(set);
    set->capacity = new_cap;
    if (nn_region_mode) nn_region_tracked_grew(nn_attrset_bytes(set) - before);
    return 0;
}

/* Binary search for a symbol in the sorted keys array.
 * Returns the index if found, or NN_ATTRSET_NOT_FOUND. */
static uint32_t bsearch_key(const nn_symbol_t *keys, uint32_t count, nn_symbol_t key)
{
    uint32_t lo = 0;
    uint32_t hi = count;
    while (lo < hi) {
        uint32_t mid = lo + (hi - lo) / 2;
        if (keys[mid] < key) {
            lo = mid + 1;
        } else if (keys[mid] > key) {
            hi = mid;
        } else {
            return mid;
        }
    }
    return NN_ATTRSET_NOT_FOUND;
}

/* --- Lifecycle --- */

/* A tracked set with room for `capacity` entries (0 picks a small
 * default), not yet counted under any site. */
static nn_attrset_t *attrset_alloc(uint32_t capacity)
{
    nn_attrset_t *set = (nn_attrset_t *)malloc(sizeof(nn_attrset_t));
    if (!set) return NULL;

    if (capacity == 0) capacity = 8;

    set->keys = (nn_symbol_t *)malloc(names_block_bytes(capacity));
    set->values = (void **)malloc((size_t)capacity * sizeof(void *));
    set->base = NULL;
    set->count = 0;
    set->capacity = capacity;
    set->size = 0;
    set->layers = 0;

    if (!set->keys || !set->values || nn_tracker_add(&g_tracked, set) != 0) {
        free(set->keys);
        free(set->values);
        free(set);
        return NULL;
    }
    if (nn_region_mode)
        nn_region_tracked(NN_REGION_SET, set, g_tracked.count - 1, nn_attrset_bytes(set));
    return set;
}

/* Count a set just allocated at `site` while the census counts sites
 * (nn_stats.h).  The evaluator sizes every set for the entries it puts
 * in, so its bytes now are its bytes once built. */
static void count_at(uint32_t site, const nn_attrset_t *set, int shareable)
{
    if (nn_stats_counting) nn_stats_count_set(site, nn_attrset_bytes(set), shareable);
}

nn_attrset_t *nn_attrset_new(uint32_t capacity, uint32_t site)
{
    nn_attrset_t *set = attrset_alloc(capacity);
    if (set) count_at(site, set, capacity == 0);
    return set;
}

/* Free a set's arrays and the set, and not its values: they are cells
 * the thunk arena owns. */
static void attrset_free(void *object)
{
    nn_attrset_t *set = (nn_attrset_t *)object;
    free(set->keys);
    free(set->values);
    free(set);
}

void nn_attrset_free_all(void)
{
    nn_tracker_free_all(&g_tracked, attrset_free);
}

void nn_attrset_sweep(uint32_t from, nn_tracker_keep_t keep, void *ctx)
{
    nn_tracker_sweep(&g_tracked, from, keep, ctx, attrset_free);
}

static nn_attrset_t g_empty_set = { NULL, NULL, NULL, 0, 0, 0, 1 };

nn_attrset_t *nn_attrset_empty(void)
{
    return &g_empty_set;
}

/* --- Construction --- */

int nn_attrset_insert(nn_attrset_t *set, nn_symbol_t key, void *value, uint32_t pos)
{
    NN_ASSERT(set->layers == 0, "nn_attrset_insert: the set is frozen");
    if (set->count >= set->capacity && grow(set) != 0) return -1;
    set->keys[set->count] = key;
    positions_of(set)[set->count] = pos;
    set->values[set->count] = value;
    set->count++;
    set->size = set->count;
    return 0;
}

/* Sets of at most this many entries are sorted by insertion, which needs
 * no allocation; larger ones by qsort over tagged entries. */
#define NN_ATTRSET_INSERTION_SORT_MAX 64u

/* Insertion-sort the entries by key.  The sort is stable: entries with
 * one key keep the order they were inserted in. */
static void insertion_sort(nn_attrset_t *set)
{
    uint32_t *positions = positions_of(set);
    uint32_t i;
    for (i = 1; i < set->count; i++) {
        nn_symbol_t key = set->keys[i];
        uint32_t pos = positions[i];
        void *value = set->values[i];
        uint32_t j = i;
        while (j > 0 && set->keys[j - 1] > key) {
            set->keys[j] = set->keys[j - 1];
            positions[j] = positions[j - 1];
            set->values[j] = set->values[j - 1];
            j--;
        }
        set->keys[j] = key;
        positions[j] = pos;
        set->values[j] = value;
    }
}

/* Sort the entries by key with qsort.  qsort is not stable, so each
 * entry is tagged with its insertion index, which orders the entries of
 * one key and makes the sort stable.  Returns 0, or -1 with the set
 * unchanged when the tags cannot be allocated. */
static int tagged_sort(nn_attrset_t *set)
{
    uint32_t *positions = positions_of(set);
    uint32_t i;
    nn_tagged_entry_t *tagged = (nn_tagged_entry_t *)malloc(
        (size_t)set->count * sizeof(nn_tagged_entry_t));
    if (!tagged) return -1;
    for (i = 0; i < set->count; i++) {
        tagged[i].key = set->keys[i];
        tagged[i].idx = i;
        tagged[i].pos = positions[i];
        tagged[i].value = set->values[i];
    }
    qsort(tagged, set->count, sizeof(nn_tagged_entry_t), cmp_tagged);
    for (i = 0; i < set->count; i++) {
        set->keys[i] = tagged[i].key;
        positions[i] = tagged[i].pos;
        set->values[i] = tagged[i].value;
    }
    free(tagged);
    return 0;
}

void nn_attrset_freeze(nn_attrset_t *set)
{
    uint32_t write = 0;
    uint32_t read;
    uint32_t *positions;

    if (set->layers) return;

    /* Stable, so the last insert of a repeated key sorts last of its run.
     * A large set whose tags cannot be allocated is sorted by insertion,
     * slower and in place. */
    if (set->count <= NN_ATTRSET_INSERTION_SORT_MAX || tagged_sort(set) != 0) {
        insertion_sort(set);
    }
    positions = positions_of(set);

    /* Keep the last entry of each run of one key. */
    for (read = 0; read < set->count; read++) {
        if (read + 1 < set->count && set->keys[read] == set->keys[read + 1]) {
            continue;
        }
        if (write != read) {
            set->keys[write] = set->keys[read];
            positions[write] = positions[read];
            set->values[write] = set->values[read];
        }
        write++;
    }
    set->count = write;
    set->size = write;
    set->layers = 1;
}

/* --- Query --- */

void *nn_attrset_lookup(const nn_attrset_t *set, nn_symbol_t key)
{
    const nn_attrset_t *layer;
    for (layer = set; layer; layer = layer->base) {
        uint32_t idx = bsearch_key(layer->keys, layer->count, key);
        if (idx != NN_ATTRSET_NOT_FOUND) return layer->values[idx];
    }
    return NULL;
}

int nn_attrset_find(const nn_attrset_t *set, nn_symbol_t key,
                    uint32_t *pos, void **value)
{
    const nn_attrset_t *layer;
    for (layer = set; layer; layer = layer->base) {
        uint32_t idx = bsearch_key(layer->keys, layer->count, key);
        if (idx == NN_ATTRSET_NOT_FOUND) continue;
        if (pos) *pos = positions_of(layer)[idx];
        if (value) *value = layer->values[idx];
        return 1;
    }
    return 0;
}

uint32_t nn_attrset_index(const nn_attrset_t *set, nn_symbol_t key)
{
    NN_ASSERT(set->base == NULL, "nn_attrset_index: the set is layered");
    return bsearch_key(set->keys, set->count, key);
}

void nn_attrset_set_value(nn_attrset_t *set, uint32_t idx, void *value)
{
    NN_ASSERT(set->base == NULL, "nn_attrset_set_value: the set is layered");
    NN_ASSERT(idx < set->count, "nn_attrset_set_value: idx out of bounds");
    set->values[idx] = value;
}

void *nn_attrset_get_value(const nn_attrset_t *set, uint32_t idx)
{
    NN_ASSERT(set->base == NULL, "nn_attrset_get_value: the set is layered");
    NN_ASSERT(idx < set->count, "nn_attrset_get_value: idx out of bounds");
    return set->values[idx];
}

nn_symbol_t nn_attrset_get_key(const nn_attrset_t *set, uint32_t idx)
{
    NN_ASSERT(set->base == NULL, "nn_attrset_get_key: the set is layered");
    NN_ASSERT(idx < set->count, "nn_attrset_get_key: idx out of bounds");
    return set->keys[idx];
}

uint32_t nn_attrset_get_pos(const nn_attrset_t *set, uint32_t idx)
{
    NN_ASSERT(set->base == NULL, "nn_attrset_get_pos: the set is layered");
    NN_ASSERT(idx < set->count, "nn_attrset_get_pos: idx out of bounds");
    return positions_of(set)[idx];
}

uint32_t nn_attrset_size(const nn_attrset_t *set)
{
    return set->size;
}

const nn_attrset_t *nn_attrset_base(const nn_attrset_t *set)
{
    return set->base;
}

/* --- The entries a set binds, in symbol order --- */

/* A walk over the entries a set binds: one head for each set in its
 * chain, top first, at the next entry of that set's own arrays.  Each
 * step takes the least name at any head, from the topmost head that has
 * it, and moves every head at that name past it, as upstream's iterator
 * merges a set's layers (Bindings::iterator in attr-set.hh at 2.34.9). */
typedef struct {
    const nn_attrset_t *layer[NN_ATTRSET_MAX_LAYERS];
    uint32_t            next[NN_ATTRSET_MAX_LAYERS];
    uint32_t            heads;
} cursor_t;

static void cursor_start(cursor_t *cursor, const nn_attrset_t *set)
{
    const nn_attrset_t *layer;
    cursor->heads = 0;
    for (layer = set; layer; layer = layer->base) {
        NN_ASSERT(cursor->heads < NN_ATTRSET_MAX_LAYERS,
                  "cursor_start: a chain longer than NN_ATTRSET_MAX_LAYERS");
        cursor->layer[cursor->heads] = layer;
        cursor->next[cursor->heads] = 0;
        cursor->heads++;
    }
}

/* The next entry: 1 with the set that holds it and its index there, or
 * 0 past the last. */
static int cursor_next(cursor_t *cursor, const nn_attrset_t **from, uint32_t *idx)
{
    uint32_t head, best = cursor->heads;
    nn_symbol_t least = 0;
    for (head = 0; head < cursor->heads; head++) {
        const nn_attrset_t *layer = cursor->layer[head];
        if (cursor->next[head] >= layer->count) continue;
        if (best == cursor->heads || layer->keys[cursor->next[head]] < least) {
            best = head;
            least = layer->keys[cursor->next[head]];
        }
    }
    if (best == cursor->heads) return 0;
    *from = cursor->layer[best];
    *idx = cursor->next[best];
    for (head = 0; head < cursor->heads; head++) {
        const nn_attrset_t *layer = cursor->layer[head];
        if (cursor->next[head] < layer->count && layer->keys[cursor->next[head]] == least)
            cursor->next[head]++;
    }
    return 1;
}

/* Insert the entry at `idx` of `from` into `result`. */
static int insert_from(nn_attrset_t *result, const nn_attrset_t *from, uint32_t idx)
{
    return nn_attrset_insert(result, from->keys[idx], from->values[idx], positions_of(from)[idx]);
}

/* Freeze a set filled in symbol order, one entry for each name. */
static void finish_sorted(nn_attrset_t *set)
{
    set->size = set->count;
    set->layers = 1;
}

void nn_attrset_entries(const nn_attrset_t *set, nn_symbol_t *keys,
                        uint32_t *positions, void **values)
{
    cursor_t cursor;
    const nn_attrset_t *from;
    uint32_t idx, i = 0;
    cursor_start(&cursor, set);
    while (cursor_next(&cursor, &from, &idx)) {
        NN_ASSERT(i < set->size, "nn_attrset_entries: more entries than the set's size");
        keys[i] = from->keys[idx];
        positions[i] = positions_of(from)[idx];
        values[i] = from->values[idx];
        i++;
    }
}

/* --- Set operations --- */

nn_attrset_t *nn_attrset_union(const nn_attrset_t *a, const nn_attrset_t *b,
                               uint32_t site)
{
    /* The merge-join below needs both inputs sorted, as a frozen set is. */
    NN_ASSERT(a->layers && b->layers, "nn_attrset_union: inputs must be frozen");
    /* Merge-join: all keys from both, b wins on conflict.  The sum of two
     * uint32_t sizes needs 33 bits; sized at the ceiling instead, the
     * result still holds any union with enough shared keys to fit, and
     * an insert refuses the one that does not. */
    uint64_t total = (uint64_t)a->size + b->size;
    uint32_t cap = total > NN_ATTRSET_MAX_ENTRIES
                       ? NN_ATTRSET_MAX_ENTRIES : (uint32_t)total;
    nn_attrset_t *result = attrset_alloc(cap);
    if (!result) return NULL;
    count_at(site, result, a->size == 0 || b->size == 0);
    cursor_t ca, cb;
    const nn_attrset_t *fa, *fb;
    uint32_t ia, ib;
    int more_a, more_b;
    cursor_start(&ca, a);
    cursor_start(&cb, b);
    more_a = cursor_next(&ca, &fa, &ia);
    more_b = cursor_next(&cb, &fb, &ib);

    /* A refused result stays tracked, so teardown frees it. */
    while (more_a || more_b) {
        nn_symbol_t ka = more_a ? fa->keys[ia] : 0;
        nn_symbol_t kb = more_b ? fb->keys[ib] : 0;
        if (more_a && (!more_b || ka < kb)) {
            if (insert_from(result, fa, ia) != 0) return NULL;
            more_a = cursor_next(&ca, &fa, &ia);
        } else {
            /* b's entry, and over a's of the same key (right-biased //),
             * its position too. */
            if (insert_from(result, fb, ib) != 0) return NULL;
            if (more_a && ka == kb) more_a = cursor_next(&ca, &fa, &ia);
            more_b = cursor_next(&cb, &fb, &ib);
        }
    }

    finish_sorted(result);
    return result;
}

/* `b` layered on `a`: a set at `site` holding b's entries alone, with a
 * as its base, as upstream's ExprOpUpdate::eval builds one
 * (eval.cc at 2.34.9).  Its size counts each name of b that a's chain
 * already binds once, as upstream's finishSizeIfNecessary does
 * (attr-set.hh), in 64 bits, and a layer past NN_ATTRSET_MAX_ENTRIES is
 * refused. */
static nn_attrset_t *layer_on(const nn_attrset_t *a, const nn_attrset_t *b, uint32_t site)
{
    cursor_t cursor;
    const nn_attrset_t *from;
    uint32_t idx;
    uint64_t size = a->size;
    nn_attrset_t *layer = attrset_alloc(b->size);
    if (!layer) return NULL;
    count_at(site, layer, 0);
    cursor_start(&cursor, b);
    while (cursor_next(&cursor, &from, &idx)) {
        if (insert_from(layer, from, idx) != 0) return NULL;
        if (!nn_attrset_find(a, from->keys[idx], NULL, NULL)) size++;
    }
    if (size > NN_ATTRSET_MAX_ENTRIES) return NULL;
    layer->base = a;
    layer->size = (uint32_t)size;
    layer->layers = a->layers + 1;
    return layer;
}

nn_attrset_t *nn_attrset_update(const nn_attrset_t *a, const nn_attrset_t *b,
                                uint32_t site)
{
    nn_attrset_t *result;
    uint64_t copied;
    NN_ASSERT(a->layers && b->layers, "nn_attrset_update: inputs must be frozen");
    if (a->size != 0 && b->size != 0 && a->layers < NN_ATTRSET_MAX_LAYERS
        && b->size <= NN_ATTRSET_LAYER_RHS_MAX) {
        result = layer_on(a, b, site);
        copied = b->size;
    } else {
        result = nn_attrset_union(a, b, site);
        copied = result ? result->size : 0;
    }
    if (result && nn_stats_counting) nn_stats_count_update(copied);
    return result;
}

nn_attrset_t *nn_attrset_remove_keys(
    const nn_attrset_t *set,
    const nn_symbol_t *keys,
    uint32_t key_count,
    uint32_t site)
{
    NN_ASSERT(set->layers, "nn_attrset_remove_keys: input must be frozen");
    nn_attrset_t *result = attrset_alloc(set->size);
    if (!result) return NULL;
    count_at(site, result, set->size == 0);
    cursor_t cursor;
    const nn_attrset_t *from;
    uint32_t idx;

    cursor_start(&cursor, set);
    while (cursor_next(&cursor, &from, &idx)) {
        /* Each entry scans the removal keys: entries * keys comparisons. */
        uint32_t j;
        int skip = 0;
        for (j = 0; j < key_count; j++) {
            if (from->keys[idx] == keys[j]) {
                skip = 1;
                break;
            }
        }
        if (!skip && insert_from(result, from, idx) != 0) return NULL;
    }

    finish_sorted(result);  /* The walk is in order, and removals keep it. */
    return result;
}

/* --- Census --- */

const nn_tracker_t *nn_attrset_tracked(void)
{
    return &g_tracked;
}

uint64_t nn_attrset_bytes(const nn_attrset_t *set)
{
    return sizeof(*set) + (uint64_t)names_block_bytes(set->capacity)
         + (uint64_t)set->capacity * sizeof(void *);
}

uint32_t nn_attrset_layer_count(const nn_attrset_t *set)
{
    return set->count;
}

const nn_symbol_t *nn_attrset_layer_keys(const nn_attrset_t *set)
{
    return set->keys;
}

void *const *nn_attrset_layer_values(const nn_attrset_t *set)
{
    return set->values;
}

/* --- Internal: tagged sort comparator --- */

static int cmp_tagged(const void *a, const void *b)
{
    const nn_tagged_entry_t *ta = (const nn_tagged_entry_t *)a;
    const nn_tagged_entry_t *tb = (const nn_tagged_entry_t *)b;
    if (ta->key < tb->key) return -1;
    if (ta->key > tb->key) return  1;
    /* Same key: sort by insertion index (ascending) so last writer is last. */
    if (ta->idx < tb->idx) return -1;
    if (ta->idx > tb->idx) return  1;
    return 0;
}
