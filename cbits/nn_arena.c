/*
 * nn_arena.c — Batch StablePtr collection from the thunk arena.
 *
 * The Haskell teardown (Nix.Eval.Arena) needs every Haskell StablePtr the C
 * arena holds so it can free them.  These two entry points delegate to the
 * single-pass block-list walk in nn_thunk.c (nn_thunk_{count,collect}_
 * stableptrs), which is O(total) — index-based iteration via nn_thunk_get
 * would be O(total * blocks), since each get re-walks the block list.
 *
 * StablePtr sources (see nn_thunk.c):
 *   1. PENDING/BLACKHOLE: payload = StablePtr Env (knot-tying laziness)
 *   2. COMPUTED with val_tag == NN_VALUE_PTR: StablePtr NixValue
 * All other COMPUTED payloads (inline scalars, C pointers) are NOT freed.
 */

#include "nn_arena.h"
#include "nn_thunk.h"

uint32_t
nn_arena_stableptr_count(void)
{
    return nn_thunk_count_stableptrs();
}

uint32_t
nn_arena_collect_stableptrs(void **output, uint32_t max_count)
{
    return nn_thunk_collect_stableptrs(output, max_count);
}
