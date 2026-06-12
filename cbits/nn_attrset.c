/*
 * nn_attrset.c - Sorted symbol-keyed attribute set.
 *
 * Two contiguous arrays: keys (nn_symbol_t) and values (void*),
 * sorted by symbol ID after freeze.  Binary search for lookup.
 *
 * Memory layout after freeze (example with 4 entries):
 *   keys:   [ 3, 7, 12, 45 ]    (sorted symbol IDs)
 *   values: [ p0, p1, p2, p3 ]  (parallel opaque pointers)
 *
 * This replaces Haskell's Map.Bin tree:
 *   Map.Bin: ~48 bytes/node (constructor + key + value + size + left + right)
 *   nn_attrset: ~12 bytes/entry (4-byte key + 8-byte pointer)
 *   For 30k entries: 1.4 MB down to 360 KB (4x reduction, plus cache-friendly)
 */

#include "nn_attrset.h"
#include "nn_assert.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* --- Global tracking for bulk cleanup --- */

static nn_attrset_t **g_tracked = NULL;
static uint32_t g_tracked_count = 0;
static uint32_t g_tracked_cap   = 0;

static void nn_attrset_track(nn_attrset_t *set)
{
    if (g_tracked_count >= g_tracked_cap) {
        uint32_t new_cap = g_tracked_cap ? g_tracked_cap * 2 : 256;
        nn_attrset_t **new_arr = (nn_attrset_t **)realloc(
            g_tracked, (size_t)new_cap * sizeof(nn_attrset_t *));
        if (!new_arr) {
            fprintf(stderr, "nn_attrset_track: realloc failed\n");
            abort();
        }
        g_tracked = new_arr;
        g_tracked_cap = new_cap;
    }
    g_tracked[g_tracked_count++] = set;
}

void nn_attrset_free_all(void)
{
    uint32_t i;
    for (i = 0; i < g_tracked_count; i++) {
        nn_attrset_free(g_tracked[i]);
    }
    free(g_tracked);
    g_tracked = NULL;
    g_tracked_count = 0;
    g_tracked_cap = 0;
}

/* --- Internal representation --- */

struct nn_attrset {
    nn_symbol_t *keys;
    void       **values;
    uint32_t     count;     /* entries used */
    uint32_t     capacity;  /* entries allocated */
    uint32_t     frozen;    /* 1 after freeze, 0 during construction */
};

/* --- Internal types --- */

typedef struct {
    nn_symbol_t key;
    uint32_t    idx;
    void       *value;
} nn_tagged_entry_t;

/* --- Forward declarations --- */

static int cmp_tagged(const void *a, const void *b);

/* --- Helpers --- */

/* Grow arrays to at least new_cap.  Returns 0 on success, -1 on failure. */
static int grow(nn_attrset_t *set, uint32_t new_cap)
{
    nn_symbol_t *new_keys = (nn_symbol_t *)realloc(
        set->keys, (size_t)new_cap * sizeof(nn_symbol_t));
    if (!new_keys) return -1;
    set->keys = new_keys;

    void **new_values = (void **)realloc(
        set->values, (size_t)new_cap * sizeof(void *));
    if (!new_values) return -1;
    set->values = new_values;

    set->capacity = new_cap;
    return 0;
}

/* Binary search for a symbol in the sorted keys array.
 * Returns the index if found, or -1. */
static int32_t bsearch_key(const nn_symbol_t *keys, uint32_t count, nn_symbol_t key)
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
            return (int32_t)mid;
        }
    }
    return -1;
}

/* --- Lifecycle --- */

nn_attrset_t *nn_attrset_new(uint32_t capacity)
{
    nn_attrset_t *set = (nn_attrset_t *)malloc(sizeof(nn_attrset_t));
    if (!set) return NULL;

    if (capacity == 0) capacity = 8;

    set->keys = (nn_symbol_t *)malloc((size_t)capacity * sizeof(nn_symbol_t));
    set->values = (void **)malloc((size_t)capacity * sizeof(void *));
    set->count = 0;
    set->capacity = capacity;
    set->frozen = 0;

    if (!set->keys || !set->values) {
        free(set->keys);
        free(set->values);
        free(set);
        return NULL;
    }

    nn_attrset_track(set);
    return set;
}

void nn_attrset_free(nn_attrset_t *set)
{
    if (!set) return;
    free(set->keys);
    free(set->values);
    free(set);
}

/* --- Construction --- */

void nn_attrset_insert(nn_attrset_t *set, nn_symbol_t key, void *value)
{
    if (set->count >= set->capacity) {
        if (grow(set, set->capacity * 2) != 0) {
            fprintf(stderr, "nn_attrset_insert: grow failed (capacity %u)\n",
                    (unsigned)set->capacity);
            abort();
        }
    }
    set->keys[set->count] = key;
    set->values[set->count] = value;
    set->count++;
}

void nn_attrset_freeze(nn_attrset_t *set)
{
    if (set->frozen) return;

    if (set->count <= 1) {
        set->frozen = 1;
        return;
    }

    /*
     * Sort by key.  We need last-writer-wins for duplicates, but qsort
     * isn't stable.  Strategy: tag each entry with its insertion index,
     * sort by (key, index), then dedup keeping the highest index per key.
     *
     * For small sets (<= 64), use a simple insertion sort that IS stable
     * (avoids the malloc overhead of tagging).
     */
    if (set->count <= 64) {
        /* Stable insertion sort. */
        uint32_t i, j;
        for (i = 1; i < set->count; i++) {
            nn_symbol_t tmpk = set->keys[i];
            void *tmpv = set->values[i];
            j = i;
            while (j > 0 && set->keys[j - 1] > tmpk) {
                set->keys[j] = set->keys[j - 1];
                set->values[j] = set->values[j - 1];
                j--;
            }
            set->keys[j] = tmpk;
            set->values[j] = tmpv;
        }
    } else {
        /*
         * Large set: tag entries with insertion index, qsort by (key, index),
         * then strip tags.  This gives stable sort semantics.
         */
        nn_tagged_entry_t *tagged = (nn_tagged_entry_t *)malloc(
            (size_t)set->count * sizeof(nn_tagged_entry_t));
        if (!tagged) {
            /* Fallback: in-place insertion sort for large sets when malloc
             * fails.  Falls through to the dedup block below. */
            for (uint32_t i = 1; i < set->count; i++) {
                nn_symbol_t key = set->keys[i];
                void *val = set->values[i];
                uint32_t j = i;
                while (j > 0 && set->keys[j-1] > key) {
                    set->keys[j] = set->keys[j-1];
                    set->values[j] = set->values[j-1];
                    j--;
                }
                set->keys[j] = key;
                set->values[j] = val;
            }
        } else {
            uint32_t i;
            for (i = 0; i < set->count; i++) {
                tagged[i].key = set->keys[i];
                tagged[i].idx = i;
                tagged[i].value = set->values[i];
            }

            /* Sort by key, break ties by insertion index (ascending). */
            qsort(tagged, set->count, sizeof(nn_tagged_entry_t), cmp_tagged);

            for (i = 0; i < set->count; i++) {
                set->keys[i] = tagged[i].key;
                set->values[i] = tagged[i].value;
            }
            free(tagged);
        }
    }

    /* Dedup: keep last occurrence of each key (last-writer-wins).
     * After stable sort, duplicates are adjacent with the last insert
     * at the highest index position. */
    {
        uint32_t write = 0;
        uint32_t read;
        for (read = 0; read < set->count; read++) {
            /* If next entry has the same key, skip this one (keep the later one). */
            if (read + 1 < set->count && set->keys[read] == set->keys[read + 1]) {
                continue;
            }
            if (write != read) {
                set->keys[write] = set->keys[read];
                set->values[write] = set->values[read];
            }
            write++;
        }
        set->count = write;
    }

    set->frozen = 1;
}

/* --- Query --- */

void *nn_attrset_lookup(const nn_attrset_t *set, nn_symbol_t key)
{
    int32_t idx = bsearch_key(set->keys, set->count, key);
    if (idx < 0) return NULL;
    return set->values[idx];
}

int32_t nn_attrset_index(const nn_attrset_t *set, nn_symbol_t key)
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

uint32_t nn_attrset_size(const nn_attrset_t *set)
{
    return set->count;
}

const nn_symbol_t *nn_attrset_keys_ptr(const nn_attrset_t *set)
{
    return set->keys;
}

/* --- Set operations --- */

nn_attrset_t *nn_attrset_union(const nn_attrset_t *a, const nn_attrset_t *b)
{
    /* The merge-join below assumes both inputs are sorted; every set this
     * module produces is frozen (hence sorted), so guard it in debug. */
    NN_ASSERT(a->frozen && b->frozen, "nn_attrset_union: inputs must be frozen");
    /* Merge-join: all keys from both, b wins on conflict. */
    uint32_t cap = a->count + b->count;
    nn_attrset_t *result = nn_attrset_new(cap);
    if (!result) { fprintf(stderr, "nn_attrset_union: alloc failed\n"); abort(); }
    uint32_t ia = 0, ib = 0;

    while (ia < a->count && ib < b->count) {
        if (a->keys[ia] < b->keys[ib]) {
            nn_attrset_insert(result, a->keys[ia], a->values[ia]);
            ia++;
        } else if (a->keys[ia] > b->keys[ib]) {
            nn_attrset_insert(result, b->keys[ib], b->values[ib]);
            ib++;
        } else {
            /* Same key - b wins (right-biased //). */
            nn_attrset_insert(result, b->keys[ib], b->values[ib]);
            ia++;
            ib++;
        }
    }
    while (ia < a->count) {
        nn_attrset_insert(result, a->keys[ia], a->values[ia]);
        ia++;
    }
    while (ib < b->count) {
        nn_attrset_insert(result, b->keys[ib], b->values[ib]);
        ib++;
    }

    result->frozen = 1;  /* Merge preserves sorted order. */
    return result;
}

nn_attrset_t *nn_attrset_remove_keys(
    const nn_attrset_t *set,
    const nn_symbol_t *keys,
    uint32_t key_count)
{
    NN_ASSERT(set->frozen, "nn_attrset_remove_keys: input must be frozen");
    nn_attrset_t *result = nn_attrset_new(set->count);
    if (!result) { fprintf(stderr, "nn_attrset_remove_keys: alloc failed\n"); abort(); }
    uint32_t i;

    for (i = 0; i < set->count; i++) {
        /* Linear scan of removal keys - fine for small removal lists.
         * For large removal lists, sort them and merge-scan instead. */
        uint32_t j;
        int skip = 0;
        for (j = 0; j < key_count; j++) {
            if (set->keys[i] == keys[j]) {
                skip = 1;
                break;
            }
        }
        if (!skip) {
            nn_attrset_insert(result, set->keys[i], set->values[i]);
        }
    }

    result->frozen = 1;  /* Source was sorted, no removals change order. */
    return result;
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
