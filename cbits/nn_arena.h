/*
 * nn_arena.h — Unified arena lifecycle and StablePtr cleanup.
 *
 * Provides batch collection of StablePtr payloads from the thunk arena
 * for efficient cleanup.  After M6 bytecode integration, PENDING thunks
 * hold (bc_idx, StablePtr Env) — the Expr is eliminated, replaced by
 * a bytecode index.  COMPUTED/NN_VALUE_PTR thunks hold StablePtr NixValue.
 *
 * Inline scalar/C-pointer payloads (INT, FLOAT, BOOL, NULL, STR,
 * PATH, LIST, ATTRS, CTXSTR) are skipped.
 *
 * Used by Haskell-side Arena.hs to free all StablePtrs before
 * destroying the sub-arenas (thunk, env, symbol).
 */

#ifndef NN_ARENA_H
#define NN_ARENA_H

#include <stdint.h>

/* Bulk-free all tracked attribute sets (called during arena teardown). */
void nn_attrset_free_all(void);

/* Count how many thunk payloads are StablePtrs (need freeing).
 * Iterates all thunks, checks state + val_tag. */
uint32_t nn_arena_stableptr_count(void);

/* Collect StablePtr payloads into `output`.  Writes at most `max_count`
 * pointers.  Returns the number actually written.
 * Caller must allocate the output buffer. */
uint32_t nn_arena_collect_stableptrs(void **output, uint32_t max_count);

#endif /* NN_ARENA_H */
