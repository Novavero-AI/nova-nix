/*
 * nn_lambda.c - Lambda closure structs for nova-nix.
 *
 * Each nn_lambda_t is a malloc'd struct holding the closure env,
 * body bytecode index, and formals specification, tracked
 * (nn_tracker.h) for bulk cleanup at evaluation end.
 */

#include "nn_lambda.h"
#include "nn_assert.h"

#include <stdlib.h>
#include <string.h>

/* Every lambda allocated, for bulk cleanup. */
static nn_tracker_t g_tracked;

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

/* The most formals whose entries fit the UINT32_MAX byte limit used
 * across the C layer, which nn_lambda_new keeps a count within.  A C99
 * compile-time check that every such count fits the shape: */
#define NN_LAMBDA_ENTRIES_MAX (UINT32_MAX / sizeof(nn_formal_entry_t))
typedef char nn_lambda_count_range_check_[(NN_LAMBDA_ENTRIES_MAX <= NN_LAMBDA_MAX_FORMALS) ? 1 : -1];

/* Whether `formals_type` is one of the NN_FORMALS_* tags. */
static int is_formals_type(uint8_t formals_type)
{
    switch (formals_type) {
    case NN_FORMALS_NAME:
    case NN_FORMALS_SET:
    case NN_FORMALS_NAMED_SET:
        return 1;
    default:
        return 0;
    }
}

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
    NN_ASSERT(is_formals_type(formals_type), "nn_lambda_new: unknown formals type");

    /* The count is input-reachable (it scales with the source formal
     * list); refusing one past NN_LAMBDA_ENTRIES_MAX keeps the entries'
     * bytes within the limit, the malloc argument from wrapping size_t on
     * any platform, and the count within the shape. */
    if (formal_count > NN_LAMBDA_ENTRIES_MAX) return NULL;
    size_t entry_bytes = (size_t)formal_count * sizeof(nn_formal_entry_t);

    nn_lambda_t *lam = (nn_lambda_t *)malloc(sizeof(nn_lambda_t));
    if (!lam) return NULL;

    lam->env             = env;
    lam->body_bc_idx     = body_bc_idx;
    lam->name_sym        = name_sym;
    lam->lambda_name_sym = lambda_name_sym;
    lam->shape           = nn_lambda_shape(formals_type, allow_extra, formal_count);

    if (formal_count > 0) {
        lam->entries = (nn_formal_entry_t *)malloc(entry_bytes);
        if (!lam->entries) {
            free(lam);
            return NULL;
        }
        memset(lam->entries, 0, entry_bytes);
    } else {
        lam->entries = NULL;
    }

    if (nn_tracker_add(&g_tracked, lam) != 0) {
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
    NN_ASSERT(idx < nn_lambda_formal_count(lam), "nn_lambda_set_entry: idx out of bounds");
    lam->entries[idx].name_sym       = name_sym;
    lam->entries[idx].has_default    = has_default;
    lam->entries[idx].default_bc_idx = default_bc_idx;
}

/* Free a lambda and its formal entries. */
static void
lambda_free(void *object)
{
    nn_lambda_t *lam = (nn_lambda_t *)object;
    free(lam->entries);
    free(lam);
}

void
nn_lambda_free_all(void)
{
    nn_tracker_free_all(&g_tracked, lambda_free);
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
    NN_ASSERT(idx < nn_lambda_formal_count(lam), "nn_lambda_entry_name: idx out of bounds");
    return lam->entries[idx].name_sym;
}

uint32_t
nn_lambda_entry_has_default(const nn_lambda_t *lam, uint32_t idx)
{
    NN_ASSERT(idx < nn_lambda_formal_count(lam), "nn_lambda_entry_has_default: idx out of bounds");
    return lam->entries[idx].has_default;
}

uint32_t
nn_lambda_entry_default(const nn_lambda_t *lam, uint32_t idx)
{
    NN_ASSERT(idx < nn_lambda_formal_count(lam), "nn_lambda_entry_default: idx out of bounds");
    return lam->entries[idx].default_bc_idx;
}

/* --- Census --- */

const nn_tracker_t *
nn_lambda_tracked(void)
{
    return &g_tracked;
}

uint64_t
nn_lambda_bytes(const nn_lambda_t *lam)
{
    return sizeof(*lam) + (uint64_t)nn_lambda_formal_count(lam) * sizeof(nn_formal_entry_t);
}
