/*
 * nn_lambda.c - Lambda closure structs for nova-nix.
 *
 * Each nn_lambda_t is a malloc'd struct holding the closure env,
 * body bytecode index, and formals specification.  Tracked in a
 * global array for bulk cleanup at evaluation end.
 */

#include "nn_lambda.h"
#include "nn_assert.h"

#include <stdlib.h>
#include <string.h>

/* --- Global tracking for bulk cleanup --- */

static nn_lambda_t **g_tracked = NULL;
static uint32_t g_tracked_count = 0;
static uint32_t g_tracked_cap   = 0;

/* Returns 0, or -1 when the tracking array cannot grow. */
static int nn_lambda_track(nn_lambda_t *lam)
{
    if (g_tracked_count >= g_tracked_cap) {
        if (g_tracked_cap > UINT32_MAX / 2) return -1;
        uint32_t new_cap = g_tracked_cap ? g_tracked_cap * 2 : 256;
        nn_lambda_t **new_arr = (nn_lambda_t **)realloc(
            g_tracked, (size_t)new_cap * sizeof(nn_lambda_t *));
        if (!new_arr) return -1;
        g_tracked = new_arr;
        g_tracked_cap = new_cap;
    }
    g_tracked[g_tracked_count++] = lam;
    return 0;
}

/* --- Shape --- */

/* Where the shape's fields sit: the formals type in the top two bits,
 * the ellipsis flag below it, the count in the rest. */
#define NN_LAMBDA_EXTRA_SHIFT NN_LAMBDA_COUNT_BITS
#define NN_LAMBDA_TYPE_SHIFT  (NN_LAMBDA_COUNT_BITS + 1)
#define NN_LAMBDA_TYPE_MASK   UINT32_C(3)

/* C99-compatible compile-time checks: the type's two bits end the word,
 * and every formals type fits them. */
typedef char nn_lambda_type_bits_check_[(NN_LAMBDA_TYPE_SHIFT + 2 == 32) ? 1 : -1];
typedef char nn_lambda_type_range_check_[(NN_FORMALS_NAMED_SET <= NN_LAMBDA_TYPE_MASK) ? 1 : -1];

uint32_t
nn_lambda_shape(uint8_t formals_type, uint8_t allow_extra, uint32_t formal_count)
{
    return (((uint32_t)formals_type & NN_LAMBDA_TYPE_MASK) << NN_LAMBDA_TYPE_SHIFT)
         | ((uint32_t)(allow_extra != 0) << NN_LAMBDA_EXTRA_SHIFT)
         | (formal_count & NN_LAMBDA_MAX_FORMALS);
}

uint8_t
nn_lambda_shape_formals_type(uint32_t shape)
{
    return (uint8_t)((shape >> NN_LAMBDA_TYPE_SHIFT) & NN_LAMBDA_TYPE_MASK);
}

uint8_t
nn_lambda_shape_allow_extra(uint32_t shape)
{
    return (uint8_t)((shape >> NN_LAMBDA_EXTRA_SHIFT) & UINT32_C(1));
}

uint32_t
nn_lambda_shape_formal_count(uint32_t shape)
{
    return shape & NN_LAMBDA_MAX_FORMALS;
}

/* --- Lifecycle --- */

nn_lambda_t *
nn_lambda_new(struct nn_env *env, uint32_t body_bc_idx,
              uint8_t formals_type, uint32_t name_sym,
              uint32_t lambda_name_sym,
              uint8_t allow_extra, uint32_t formal_count)
{
    /* What the shape cannot hold is refused before it is packed. */
    if (formals_type > NN_FORMALS_NAMED_SET || formal_count > NN_LAMBDA_MAX_FORMALS) return NULL;

    /* The count is input-reachable (it scales with the source formal
     * list); computing the entries size in uint64_t and capping it at
     * the UINT32_MAX byte limit used across the C layer keeps the
     * malloc argument from wrapping size_t on any platform. */
    uint64_t entry_bytes64 = (uint64_t)formal_count * sizeof(nn_formal_entry_t);
    if (entry_bytes64 > UINT32_MAX) return NULL;

    nn_lambda_t *lam = (nn_lambda_t *)malloc(sizeof(nn_lambda_t));
    if (!lam) return NULL;

    lam->env             = env;
    lam->body_bc_idx     = body_bc_idx;
    lam->name_sym        = name_sym;
    lam->lambda_name_sym = lambda_name_sym;
    lam->shape           = nn_lambda_shape(formals_type, allow_extra, formal_count);

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

    if (nn_lambda_track(lam) != 0) {
        free(lam->entries);
        free(lam);
        return NULL;
    }
    return lam;
}

void
nn_lambda_set_entry(nn_lambda_t *lam, uint32_t idx,
                    uint32_t name_sym, uint32_t has_default,
                    uint32_t default_bc_idx)
{
    NN_ASSERT(lam != NULL && lam->entries != NULL && idx < nn_lambda_formal_count(lam),
              "nn_lambda_set_entry: idx out of bounds");
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
    return nn_lambda_shape_formals_type(lam->shape);
}

uint32_t
nn_lambda_name_sym(const nn_lambda_t *lam)
{
    return lam->name_sym;
}

uint32_t
nn_lambda_lambda_name_sym(const nn_lambda_t *lam)
{
    return lam->lambda_name_sym;
}

uint8_t
nn_lambda_allow_extra(const nn_lambda_t *lam)
{
    return nn_lambda_shape_allow_extra(lam->shape);
}

uint32_t
nn_lambda_formal_count(const nn_lambda_t *lam)
{
    return nn_lambda_shape_formal_count(lam->shape);
}

uint32_t
nn_lambda_entry_name(const nn_lambda_t *lam, uint32_t idx)
{
    NN_ASSERT(lam != NULL && lam->entries != NULL && idx < nn_lambda_formal_count(lam),
              "nn_lambda_entry_name: idx out of bounds or no formals");
    return lam->entries[idx].name_sym;
}

uint32_t
nn_lambda_entry_has_default(const nn_lambda_t *lam, uint32_t idx)
{
    NN_ASSERT(lam != NULL && lam->entries != NULL && idx < nn_lambda_formal_count(lam),
              "nn_lambda_entry_has_default: idx out of bounds or no formals");
    return lam->entries[idx].has_default;
}

uint32_t
nn_lambda_entry_default(const nn_lambda_t *lam, uint32_t idx)
{
    NN_ASSERT(lam != NULL && lam->entries != NULL && idx < nn_lambda_formal_count(lam),
              "nn_lambda_entry_default: idx out of bounds or no formals");
    return lam->entries[idx].default_bc_idx;
}

/* --- Census --- */

nn_lambda_t *const *
nn_lambda_tracked(uint32_t *count, uint32_t *capacity)
{
    *count = g_tracked_count;
    *capacity = g_tracked_cap;
    return g_tracked;
}

uint64_t
nn_lambda_bytes(const nn_lambda_t *lam)
{
    return sizeof(*lam) + (uint64_t)nn_lambda_formal_count(lam) * sizeof(nn_formal_entry_t);
}
