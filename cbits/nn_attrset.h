/*
 * nn_attrset.h — Sorted symbol-keyed attribute set for nova-nix.
 *
 * Replaces Haskell's Data.Map.Strict (48 bytes per Bin node, pointer-
 * chasing balanced tree) with a contiguous sorted array of (symbol, slot)
 * pairs.  Binary search for O(log n) lookup; linear scan for iteration.
 *
 * Construction is two-phase:
 *   1. nn_attrset_new(cap) + nn_attrset_insert() — append unsorted
 *   2. nn_attrset_freeze() — sort by symbol ID, enable binary search
 *
 * After freeze, the set is immutable (values can be updated in-place
 * but keys cannot change).  This matches Nix semantics: attribute sets
 * are constructed once and never gain/lose keys.
 *
 * Values are opaque void* — Haskell passes StablePtr Thunk.  The C
 * side never dereferences them.
 *
 * Lifecycle: caller owns the nn_attrset_t* and must call nn_attrset_free.
 * Values (StablePtrs) are NOT freed by nn_attrset_free — Haskell owns them.
 */

#ifndef NN_ATTRSET_H
#define NN_ATTRSET_H

#include "nn_symbol.h"
#include <stddef.h>
#include <stdint.h>

/* --- Types --- */

/* Opaque attribute set handle. */
typedef struct nn_attrset nn_attrset_t;

/* --- Lifecycle --- */

/* Allocate a new empty attribute set with space for `capacity` entries.
 * Returns NULL on allocation failure. */
nn_attrset_t *nn_attrset_new(uint32_t capacity);

/* Free the attribute set structure.  Does NOT free the value pointers
 * (Haskell owns those via StablePtr). */
void nn_attrset_free(nn_attrset_t *set);

/* --- Construction (before freeze) --- */

/* Append a key-value pair.  Keys need not be sorted or unique at this
 * stage — duplicates are resolved at freeze time (last insert wins,
 * matching Nix // merge semantics).
 * Grows the internal arrays if capacity is exceeded. */
void nn_attrset_insert(nn_attrset_t *set, nn_symbol_t key, void *value);

/* Sort entries by symbol ID and deduplicate (last writer wins).
 * Must be called exactly once before any lookup/keys/values access.
 * After freeze, no more inserts are allowed. */
void nn_attrset_freeze(nn_attrset_t *set);

/* --- Query (after freeze) --- */

/* Look up a key by symbol.  Returns the value pointer, or NULL if
 * the key is not present.  O(log n) binary search. */
void *nn_attrset_lookup(const nn_attrset_t *set, nn_symbol_t key);

/* Return the index of a key, or -1 if not found.
 * Useful for updating value slots in place. */
int32_t nn_attrset_index(const nn_attrset_t *set, nn_symbol_t key);

/* Update the value at a known index.  Index must be valid (0 <= idx < count).
 * Used for lazy materialization: slot starts NULL, gets filled on first force. */
void nn_attrset_set_value(nn_attrset_t *set, uint32_t idx, void *value);

/* Return the value at a known index. */
void *nn_attrset_get_value(const nn_attrset_t *set, uint32_t idx);

/* Return the key at a known index. */
nn_symbol_t nn_attrset_get_key(const nn_attrset_t *set, uint32_t idx);

/* --- Bulk access --- */

/* Number of entries (after freeze: unique keys). */
uint32_t nn_attrset_size(const nn_attrset_t *set);

/* Direct pointer to the sorted keys array (count elements).
 * Valid until the set is freed. */
const nn_symbol_t *nn_attrset_keys_ptr(const nn_attrset_t *set);

/* Direct pointer to the values array (count elements).
 * Valid until the set is freed. */
void *const *nn_attrset_values_ptr(const nn_attrset_t *set);

/* --- Lifecycle: bulk cleanup --- */

/* Free all tracked attribute sets at once (arena-style cleanup).
 * Every set created via nn_attrset_new is automatically tracked.
 * Call this once at evaluation end, before destroying sub-arenas. */
void nn_attrset_free_all(void);

/* --- Set operations --- */

/* Create a new set containing only keys present in both `a` and `b`,
 * taking values from `b`.  Both inputs must be frozen.
 * The result is returned frozen.  Caller owns the result. */
nn_attrset_t *nn_attrset_intersect(const nn_attrset_t *a, const nn_attrset_t *b);

/* Create a new set that is the right-biased union of `a` and `b`
 * (keys in `b` override `a`).  Both inputs must be frozen.
 * The result is returned frozen.  Caller owns the result. */
nn_attrset_t *nn_attrset_union(const nn_attrset_t *a, const nn_attrset_t *b);

/* Create a new set with the given keys removed.
 * Input must be frozen.  Result is returned frozen. */
nn_attrset_t *nn_attrset_remove_keys(
    const nn_attrset_t *set,
    const nn_symbol_t *keys,
    uint32_t key_count);

#endif /* NN_ATTRSET_H */
