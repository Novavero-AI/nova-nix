/*
 * nn_list.h — Contiguous array of thunk pointers for nova-nix lists.
 *
 * Replaces Haskell's [Thunk] (linked-list cons cells, ~48 bytes per
 * element on the GHC heap) with a flat C array of nn_thunk_t* pointers
 * (8 bytes per element, cache-friendly layout).
 *
 * The items array is allocated via nn_env_alloc_slots (page-based bump
 * allocator).  The nn_list_t header is malloc'd and tracked for bulk
 * cleanup via nn_list_free_all().  Both are freed at evaluation end.
 *
 * Lists are immutable after construction: allocate with nn_list_new,
 * fill via nn_list_set, then read with nn_list_get/nn_list_count.
 *
 * Lifecycle: constructed during evaluation, freed via nn_list_free_all()
 * at arena teardown.  Not thread-safe — single-threaded evaluation only.
 */

#ifndef NN_LIST_H
#define NN_LIST_H

#include <stdint.h>

/* Forward declaration — nn_thunk_t is defined in nn_thunk.h */
struct nn_thunk;

/* --- Types --- */

/* A list: contiguous array of thunk pointers with a count. */
typedef struct nn_list {
    struct nn_thunk **items;   /* contiguous array (page-allocated) */
    uint32_t          count;   /* number of elements */
} nn_list_t;

/* --- Lifecycle --- */

/* Allocate a new list with space for `count` thunk pointers.
 * The items array is allocated via the env page allocator (O(1)).
 * The header is malloc'd and tracked for bulk cleanup.
 * Returns NULL on allocation failure or if count is 0. */
nn_list_t *nn_list_new(uint32_t count);

/* Free all tracked list headers at once (arena-style cleanup).
 * Items arrays are freed by nn_env_destroy (page allocator).
 * Call this once at evaluation end. */
void nn_list_free_all(void);

/* --- Access --- */

/* Number of elements in the list. */
uint32_t nn_list_count(const nn_list_t *list);

/* Get the thunk pointer at the given index.
 * Index must be < nn_list_count(list). */
struct nn_thunk *nn_list_get(const nn_list_t *list, uint32_t index);

/* Set the thunk pointer at the given index (for construction).
 * Index must be < nn_list_count(list). */
void nn_list_set(nn_list_t *list, uint32_t index, struct nn_thunk *thunk);

#endif /* NN_LIST_H */
