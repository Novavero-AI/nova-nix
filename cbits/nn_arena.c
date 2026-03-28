/*
 * nn_arena.c — Batch StablePtr collection from the thunk arena.
 *
 * Iterates all thunks via nn_thunk_count/nn_thunk_get and identifies
 * payloads that are Haskell StablePtrs (as opposed to inline scalar
 * values).  Three cases have StablePtr payloads:
 *
 *   1. PENDING thunks: payload = StablePtr (Expr, Env)
 *   2. BLACKHOLE thunks: payload = original PENDING StablePtr
 *      (mark_blackhole only changes state, not payload)
 *   3. COMPUTED thunks with val_tag == NN_VALUE_PTR:
 *      payload = StablePtr NixValue
 *
 * All other COMPUTED thunks (INT, FLOAT, BOOL, NULL, STR, PATH,
 * LIST, ATTRS, CTXSTR) have inline scalar or C-pointer payloads —
 * not StablePtrs, must NOT be freed.
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
