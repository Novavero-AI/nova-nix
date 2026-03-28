/*
 * nn_arena.c — Batch StablePtr collection from the thunk arena.
 *
 * Iterates all thunks via nn_thunk_count/nn_thunk_get and identifies
 * payloads that are Haskell StablePtrs (as opposed to inline scalar
 * values or C pointers).
 *
 * After M6 bytecode integration, PENDING thunks store (bc_idx,
 * StablePtr Env) — the bc_idx replaces the Expr, but the Env stays
 * as a StablePtr for knot-tying laziness.
 *
 * StablePtr sources:
 *   1. PENDING/BLACKHOLE: payload = StablePtr Env (all pending thunks)
 *   2. COMPUTED with val_tag == NN_VALUE_PTR: StablePtr NixValue
 *
 * All other COMPUTED payloads (inline scalars, C pointers) are NOT
 * StablePtrs and must NOT be freed.
 */

#include "nn_arena.h"
#include "nn_attrset.h"
#include "nn_thunk.h"

#include <stddef.h>

uint32_t
nn_arena_stableptr_count(void)
{
    uint32_t total = nn_thunk_count();
    uint32_t count = 0;
    uint32_t i;

    for (i = 0; i < total; i++) {
        nn_thunk_t *t = nn_thunk_get(i);
        if (!t) continue;

        uint8_t state = nn_thunk_state(t);
        if (state == NN_THUNK_PENDING || state == NN_THUNK_BLACKHOLE) {
            void *p = nn_thunk_payload(t);
            if (p) count++;
        } else if (state == NN_THUNK_COMPUTED) {
            if (nn_thunk_value_tag(t) == NN_VALUE_PTR) {
                void *p = nn_thunk_payload(t);
                if (p) count++;
            }
        }
    }
    return count;
}

uint32_t
nn_arena_collect_stableptrs(void **output, uint32_t max_count)
{
    uint32_t total = nn_thunk_count();
    uint32_t written = 0;
    uint32_t i;

    for (i = 0; i < total && written < max_count; i++) {
        nn_thunk_t *t = nn_thunk_get(i);
        if (!t) continue;

        uint8_t state = nn_thunk_state(t);
        if (state == NN_THUNK_PENDING || state == NN_THUNK_BLACKHOLE) {
            void *p = nn_thunk_payload(t);
            if (p) output[written++] = p;
        } else if (state == NN_THUNK_COMPUTED) {
            if (nn_thunk_value_tag(t) == NN_VALUE_PTR) {
                void *p = nn_thunk_payload(t);
                if (p) output[written++] = p;
            }
        }
    }
    return written;
}
