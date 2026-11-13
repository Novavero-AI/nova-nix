/*
 * nn_arena.h - Unified arena lifecycle and StablePtr cleanup.
 *
 * Provides batch collection of StablePtr payloads from the thunk arena
 * for efficient cleanup.  COMPUTED/NN_VALUE_PTR thunks hold a StablePtr
 * NixValue; no other payload is a StablePtr.  PENDING and BLACKHOLE
 * thunks hold an nn_env_t * the env arena owns, and the inline
 * scalar/C-pointer payloads (INT .. LAMBDA) are skipped.
 *
 * Used by Haskell-side Arena.hs to free all StablePtrs before
 * destroying the sub-arenas (thunk, env, symbol, bytecode).
 *
 * nn_arena_live answers whether every sub-arena is inside its init ..
 * destroy window.  Outside it, each sub-arena's allocators report
 * failure through their ordinary sentinel (NULL, UINT32_MAX,
 * NN_SYMBOL_INVALID) instead of touching freed or never-allocated
 * state; the Haskell boundary asks this on that failure path to tell a
 * missing arenaInit from exhaustion.
 */

#ifndef NN_ARENA_H
#define NN_ARENA_H

#include <stdint.h>

/* Bulk-free all tracked attribute sets (called during arena teardown). */
void nn_attrset_free_all(void);

/* Nonzero when the symbol table, thunk arena, env allocator and bytecode
 * store are all initialized. */
int nn_arena_live(void);

/* Count how many thunk payloads are StablePtrs (need freeing).
 * Iterates all thunks, checks state + val_tag. */
uint32_t nn_arena_stableptr_count(void);

/* Collect StablePtr payloads into `output`.  Writes at most `max_count`
 * pointers.  Returns the number actually written.
 * Caller must allocate the output buffer. */
uint32_t nn_arena_collect_stableptrs(void **output, uint32_t max_count);

#endif /* NN_ARENA_H */
