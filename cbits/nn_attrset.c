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
    nn_symbol_t *keys;       /* the shared block: capacity keys, then positions */
    uint32_t    *positions;  /* keys + capacity */
    void       **values;
    uint32_t     count;     /* entries used */
    uint32_t     capacity;  /* entries allocated */
    uint32_t     frozen;    /* 1 after freeze, 0 during construction */
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
    set->positions = new_keys + new_cap;

    uint64_t before = nn_attrset_bytes(set);
    set->capacity = new_cap;
    if (nn_region_census) nn_region_tracked_grew(nn_attrset_bytes(set) - before);
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
    set->positions = set->keys ? set->keys + capacity : NULL;
    set->values = (void **)malloc((size_t)capacity * sizeof(void *));
    set->count = 0;
    set->capacity = capacity;
    set->frozen = 0;

    if (!set->keys || !set->values || nn_tracker_add(&g_tracked, set) != 0) {
        free(set->keys);
        free(set->values);
        free(set);
        return NULL;
    }
    if (nn_region_census)
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

static nn_attrset_t g_empty_set = { NULL, NULL, NULL, 0, 0, 1 };

nn_attrset_t *nn_attrset_empty(void)
{
    return &g_empty_set;
}

/* --- Construction --- */

int nn_attrset_insert(nn_attrset_t *set, nn_symbol_t key, void *value, uint32_t pos)
{
    NN_ASSERT(!set->frozen, "nn_attrset_insert: the set is frozen");
    if (set->count >= set->capacity && grow(set) != 0) return -1;
    set->keys[set->count] = key;
    set->positions[set->count] = pos;
    set->values[set->count] = value;
    set->count++;
    return 0;
}

/* Sets of at most this many entries are sorted by insertion, which needs
 * no allocation; larger ones by qsort over tagged entries. */
#define NN_ATTRSET_INSERTION_SORT_MAX 64u

/* Insertion-sort the entries by key.  The sort is stable: entries with
 * one key keep the order they were inserted in. */
static void insertion_sort(nn_attrset_t *set)
{
    uint32_t i;
    for (i = 1; i < set->count; i++) {
        nn_symbol_t key = set->keys[i];
        uint32_t pos = set->positions[i];
        void *value = set->values[i];
        uint32_t j = i;
        while (j > 0 && set->keys[j - 1] > key) {
            set->keys[j] = set->keys[j - 1];
            set->positions[j] = set->positions[j - 1];
            set->values[j] = set->values[j - 1];
            j--;
        }
        set->keys[j] = key;
        set->positions[j] = pos;
        set->values[j] = value;
    }
}

/* Sort the entries by key with qsort.  qsort is not stable, so each
 * entry is tagged with its insertion index, which orders the entries of
 * one key and makes the sort stable.  Returns 0, or -1 with the set
 * unchanged when the tags cannot be allocated. */
static int tagged_sort(nn_attrset_t *set)
{
    uint32_t i;
    nn_tagged_entry_t *tagged = (nn_tagged_entry_t *)malloc(
        (size_t)set->count * sizeof(nn_tagged_entry_t));
    if (!tagged) return -1;
    for (i = 0; i < set->count; i++) {
        tagged[i].key = set->keys[i];
        tagged[i].idx = i;
        tagged[i].pos = set->positions[i];
        tagged[i].value = set->values[i];
    }
    qsort(tagged, set->count, sizeof(nn_tagged_entry_t), cmp_tagged);
    for (i = 0; i < set->count; i++) {
        set->keys[i] = tagged[i].key;
        set->positions[i] = tagged[i].pos;
        set->values[i] = tagged[i].value;
    }
    free(tagged);
    return 0;
}

void nn_attrset_freeze(nn_attrset_t *set)
{
    uint32_t write = 0;
    uint32_t read;

    if (set->frozen) return;

    /* Stable, so the last insert of a repeated key sorts last of its run.
     * A large set whose tags cannot be allocated is sorted by insertion,
     * slower and in place. */
    if (set->count <= NN_ATTRSET_INSERTION_SORT_MAX || tagged_sort(set) != 0) {
        insertion_sort(set);
    }

    /* Keep the last entry of each run of one key. */
    for (read = 0; read < set->count; read++) {
        if (read + 1 < set->count && set->keys[read] == set->keys[read + 1]) {
            continue;
        }
        if (write != read) {
            set->keys[write] = set->keys[read];
            set->positions[write] = set->positions[read];
            set->values[write] = set->values[read];
        }
        write++;
    }
    set->count = write;
    set->frozen = 1;
}

/* --- Query --- */

void *nn_attrset_lookup(const nn_attrset_t *set, nn_symbol_t key)
{
    uint32_t idx = bsearch_key(set->keys, set->count, key);
    if (idx == NN_ATTRSET_NOT_FOUND) return NULL;
    return set->values[idx];
}

uint32_t nn_attrset_index(const nn_attrset_t *set, nn_symbol_t key)
{
    return bsearch_key(set->keys, set->count, key);
}

void nn_attrset_set_value(nn_attrset_t *set, uint32_t idx, void *value)
{
    NN_ASSERT(idx < set->count, "nn_attrset_set_value: idx out of bounds");
    set->values[idx] = value;
}

void *nn_attrset_get_value(const nn_attrset_t *set, uint32_t idx)
{
    NN_ASSERT(idx < set->count, "nn_attrset_get_value: idx out of bounds");
    return set->values[idx];
}

nn_symbol_t nn_attrset_get_key(const nn_attrset_t *set, uint32_t idx)
{
    NN_ASSERT(idx < set->count, "nn_attrset_get_key: idx out of bounds");
    return set->keys[idx];
}

uint32_t nn_attrset_get_pos(const nn_attrset_t *set, uint32_t idx)
{
    NN_ASSERT(idx < set->count, "nn_attrset_get_pos: idx out of bounds");
    return set->positions[idx];
}

uint32_t nn_attrset_size(const nn_attrset_t *set)
{
    return set->count;
}

const nn_symbol_t *nn_attrset_keys_ptr(const nn_attrset_t *set)
{
    return set->keys;
}

/* --- Set operations --- */

nn_attrset_t *nn_attrset_union(const nn_attrset_t *a, const nn_attrset_t *b,
                               uint32_t site)
{
    /* The merge-join below needs both inputs sorted, as a frozen set is. */
    NN_ASSERT(a->frozen && b->frozen, "nn_attrset_union: inputs must be frozen");
    /* Merge-join: all keys from both, b wins on conflict.  The sum of two
     * uint32_t counts needs 33 bits; sized at the ceiling instead, the
     * result still holds any union with enough shared keys to fit, and
     * an insert refuses the one that does not. */
    uint64_t total = (uint64_t)a->count + b->count;
    uint32_t cap = total > NN_ATTRSET_MAX_ENTRIES
                       ? NN_ATTRSET_MAX_ENTRIES : (uint32_t)total;
    nn_attrset_t *result = attrset_alloc(cap);
    if (!result) return NULL;
    count_at(site, result, a->count == 0 || b->count == 0);
    uint32_t ia = 0, ib = 0;

    /* A refused result stays tracked, so teardown frees it. */
    while (ia < a->count && ib < b->count) {
        int status;
        if (a->keys[ia] < b->keys[ib]) {
            status = nn_attrset_insert(result, a->keys[ia], a->values[ia], a->positions[ia]);
            ia++;
        } else if (a->keys[ia] > b->keys[ib]) {
            status = nn_attrset_insert(result, b->keys[ib], b->values[ib], b->positions[ib]);
            ib++;
        } else {
            /* Same key - b wins (right-biased //), its position too. */
            status = nn_attrset_insert(result, b->keys[ib], b->values[ib], b->positions[ib]);
            ia++;
            ib++;
        }
        if (status != 0) return NULL;
    }
    while (ia < a->count) {
        if (nn_attrset_insert(result, a->keys[ia], a->values[ia], a->positions[ia]) != 0) return NULL;
        ia++;
    }
    while (ib < b->count) {
        if (nn_attrset_insert(result, b->keys[ib], b->values[ib], b->positions[ib]) != 0) return NULL;
        ib++;
    }

    result->frozen = 1;  /* Merge preserves sorted order. */
    return result;
}

nn_attrset_t *nn_attrset_remove_keys(
    const nn_attrset_t *set,
    const nn_symbol_t *keys,
    uint32_t key_count,
    uint32_t site)
{
    NN_ASSERT(set->frozen, "nn_attrset_remove_keys: input must be frozen");
    nn_attrset_t *result = attrset_alloc(set->count);
    if (!result) return NULL;
    count_at(site, result, set->count == 0);
    uint32_t i;

    for (i = 0; i < set->count; i++) {
        /* Each entry scans the removal keys: entries * keys comparisons. */
        uint32_t j;
        int skip = 0;
        for (j = 0; j < key_count; j++) {
            if (set->keys[i] == keys[j]) {
                skip = 1;
                break;
            }
        }
        if (!skip && nn_attrset_insert(result, set->keys[i], set->values[i], set->positions[i]) != 0) {
            return NULL;
        }
    }

    result->frozen = 1;  /* Source was sorted, no removals change order. */
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
