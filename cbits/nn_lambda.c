/*
 * nn_lambda.c - Lambda closure structs for nova-nix.
 *
 * Each nn_lambda_t is a malloc'd struct holding the closure env,
 * body bytecode index, and formals specification.  Tracked in a
 * global array for bulk cleanup at evaluation end.
 */

#include "nn_lambda.h"
#include "nn_assert.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* --- Global tracking for bulk cleanup --- */

static nn_lambda_t **g_tracked = NULL;
static uint32_t g_tracked_count = 0;
static uint32_t g_tracked_cap   = 0;

static void nn_lambda_track(nn_lambda_t *lam)
{
    if (g_tracked_count >= g_tracked_cap) {
        uint32_t new_cap = g_tracked_cap ? g_tracked_cap * 2 : 256;
        nn_lambda_t **new_arr = (nn_lambda_t **)realloc(
            g_tracked, (size_t)new_cap * sizeof(nn_lambda_t *));
        if (!new_arr) {
            fprintf(stderr, "nn_lambda_track: realloc failed\n");
            abort();
        }
        g_tracked = new_arr;
        g_tracked_cap = new_cap;
    }
    g_tracked[g_tracked_count++] = lam;
}

/* --- Lifecycle --- */

nn_lambda_t *
nn_lambda_new(struct nn_env *env, uint32_t body_bc_idx,
              uint8_t formals_type, uint32_t name_sym,
              uint8_t allow_extra, uint32_t formal_count)
{
    /* The count is input-reachable (it scales with the source formal
     * list); computing the entries size in uint64_t and capping it at
     * the UINT32_MAX byte limit used across the C layer keeps the
     * malloc argument from wrapping size_t on any platform. */
    uint64_t entry_bytes64 = (uint64_t)formal_count * sizeof(nn_formal_entry_t);
    if (entry_bytes64 > UINT32_MAX) return NULL;

    nn_lambda_t *lam = (nn_lambda_t *)malloc(sizeof(nn_lambda_t));
    if (!lam) return NULL;

    lam->env          = env;
    lam->body_bc_idx  = body_bc_idx;
    lam->formals_type = formals_type;
    lam->name_sym     = name_sym;
    lam->allow_extra  = allow_extra;
    lam->formal_count = formal_count;

    if (formal_count > 0) {
        lam->entries = (nn_formal_entry_t *)malloc((size_t)entry_bytes64);
        if (!lam->entries) {
            free(lam);
            return NULL;
        }
        memset(lam->entries, 0, (size_t)entry_bytes64);
    } else {
        lam->entries = NULL;
    }

    nn_lambda_track(lam);
    return lam;
}

void
nn_lambda_set_entry(nn_lambda_t *lam, uint32_t idx,
                    uint32_t name_sym, uint32_t has_default,
                    uint32_t default_bc_idx)
{
    /* Bound stays live under NDEBUG: the fill index derives from
     * input-sized data crossing the FFI, so a violated bound must drop
     * the write, not run past the sized array. */
    NN_ASSERT(lam != NULL && lam->entries != NULL && idx < lam->formal_count,
              "nn_lambda_set_entry: idx out of bounds");
    if (lam == NULL || lam->entries == NULL || idx >= lam->formal_count) return;
    lam->entries[idx].name_sym       = name_sym;
    lam->entries[idx].has_default    = has_default;
    lam->entries[idx].default_bc_idx = default_bc_idx;
}

void
nn_lambda_free_all(void)
{
    uint32_t i;
    for (i = 0; i < g_tracked_count; i++) {
        free(g_tracked[i]->entries);
        free(g_tracked[i]);
    }
    free(g_tracked);
    g_tracked = NULL;
    g_tracked_count = 0;
    g_tracked_cap = 0;
}

/* --- Accessors --- */

struct nn_env *
nn_lambda_env(const nn_lambda_t *lam)
{
    return lam->env;
}

uint32_t
nn_lambda_body(const nn_lambda_t *lam)
{
    return lam->body_bc_idx;
}

uint8_t
nn_lambda_formals_type(const nn_lambda_t *lam)
{
    return lam->formals_type;
}

uint32_t
nn_lambda_name_sym(const nn_lambda_t *lam)
{
    return lam->name_sym;
}

uint8_t
nn_lambda_allow_extra(const nn_lambda_t *lam)
{
    return lam->allow_extra;
}

uint32_t
nn_lambda_formal_count(const nn_lambda_t *lam)
{
    return lam->formal_count;
}

/* Entry reads keep their bound live under NDEBUG (see set_entry);
 * a violated bound returns 0 rather than reading out of bounds. */

uint32_t
nn_lambda_entry_name(const nn_lambda_t *lam, uint32_t idx)
{
    NN_ASSERT(lam != NULL && lam->entries != NULL && idx < lam->formal_count,
              "nn_lambda_entry_name: idx out of bounds or no formals");
    if (lam == NULL || lam->entries == NULL || idx >= lam->formal_count) return 0;
    return lam->entries[idx].name_sym;
}

uint32_t
nn_lambda_entry_has_default(const nn_lambda_t *lam, uint32_t idx)
{
    NN_ASSERT(lam != NULL && lam->entries != NULL && idx < lam->formal_count,
              "nn_lambda_entry_has_default: idx out of bounds or no formals");
    if (lam == NULL || lam->entries == NULL || idx >= lam->formal_count) return 0;
    return lam->entries[idx].has_default;
}

uint32_t
nn_lambda_entry_default(const nn_lambda_t *lam, uint32_t idx)
{
    NN_ASSERT(lam != NULL && lam->entries != NULL && idx < lam->formal_count,
              "nn_lambda_entry_default: idx out of bounds or no formals");
    if (lam == NULL || lam->entries == NULL || idx >= lam->formal_count) return 0;
    return lam->entries[idx].default_bc_idx;
}
