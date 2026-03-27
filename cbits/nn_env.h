/*
 * nn_env.h — Page-based bump allocator for environment slot arrays.
 *
 * Nix evaluation creates millions of environments, each with a
 * variable-size array of slot pointers (lambda formals, let bindings,
 * deferred applications).  This allocator provides fast O(1) allocation
 * for these arrays and bulk O(pages) deallocation at evaluation end.
 *
 * Slot arrays are never individually freed — the entire allocator is
 * torn down via nn_env_destroy().  This matches Nix evaluation semantics
 * where all environments die together at eval completion.
 *
 * Lifecycle: nn_env_init() before evaluation, nn_env_destroy() after.
 * Not thread-safe — single-threaded evaluation only.
 */

#ifndef NN_ENV_H
#define NN_ENV_H

#include <stdint.h>

/* --- Lifecycle --- */

/* Initialize the global slot allocator.  Must be called once before
 * any nn_env_alloc_slots() calls. */
void nn_env_init(void);

/* Destroy the global slot allocator, freeing all page memory.
 * All pointers returned by nn_env_alloc_slots() become invalid. */
void nn_env_destroy(void);

/* --- Allocation --- */

/* Allocate an array of `count` void* pointers.  O(1) amortized.
 * Returns a pointer to a contiguous array of `count` void* slots,
 * all initialized to NULL.  The pointer is valid until nn_env_destroy().
 * Returns NULL if count is 0. */
void **nn_env_alloc_slots(uint32_t count);

/* --- Diagnostics --- */

/* Total bytes allocated across all pages (for memory tracking). */
uint64_t nn_env_bytes_allocated(void);

#endif /* NN_ENV_H */
