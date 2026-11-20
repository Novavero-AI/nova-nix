/*
 * nn_ctxstr.c - Context-bearing string allocation and tracking.
 *
 * Each nn_ctxstr_t is a single contiguous malloc (header + flexible
 * array of nn_sce_t elements), tracked (nn_tracker.h) for bulk cleanup
 * via nn_ctxstr_free_all().
 */

#include "nn_ctxstr.h"
#include "nn_assert.h"
#include "nn_region.h"

#include <stdlib.h>
#include <string.h>

/* Every string allocated, for bulk cleanup. */
static nn_tracker_t g_tracked;

/* --- Lifecycle --- */

nn_ctxstr_t *
nn_ctxstr_new(nn_str_t *text, uint32_t ctx_count)
{
    /* The count is input-reachable (it scales with the evaluated
     * expression); computing the size in uint64_t and capping it at the
     * UINT32_MAX byte limit used across the C layer keeps the malloc
     * argument from wrapping size_t on any platform. */
    uint64_t bytes64 = sizeof(nn_ctxstr_t)
                     + (uint64_t)ctx_count * sizeof(nn_sce_t);
    if (bytes64 > UINT32_MAX) return NULL;
    nn_ctxstr_t *s = (nn_ctxstr_t *)malloc((size_t)bytes64);
    if (!s) return NULL;
    s->text = text;
    s->ctx_count = ctx_count;
    memset(s->ctx, 0, (size_t)ctx_count * sizeof(nn_sce_t));
    if (nn_tracker_add(&g_tracked, s) != 0) {
        free(s);
        return NULL;
    }
    if (nn_region_census) nn_region_tracked(NN_REGION_CTXSTR, s, g_tracked.count - 1, nn_ctxstr_bytes(s));
    return s;
}

void
nn_ctxstr_free_all(void)
{
    nn_tracker_free_all(&g_tracked, free);
}

/* --- Element setters --- */

void
nn_ctxstr_set_plain(nn_ctxstr_t *s, uint32_t idx,
                    nn_str_t *sp_hash, nn_str_t *sp_name)
{
    NN_ASSERT(idx < s->ctx_count, "nn_ctxstr_set_plain: idx out of bounds");
    s->ctx[idx].tag = NN_SCE_PLAIN;
    s->ctx[idx].sp_hash = sp_hash;
    s->ctx[idx].sp_name = sp_name;
    s->ctx[idx].output = NULL;
}

void
nn_ctxstr_set_drv_output(nn_ctxstr_t *s, uint32_t idx,
                         nn_str_t *sp_hash, nn_str_t *sp_name,
                         nn_str_t *output)
{
    NN_ASSERT(idx < s->ctx_count, "nn_ctxstr_set_drv_output: idx out of bounds");
    s->ctx[idx].tag = NN_SCE_DRV_OUTPUT;
    s->ctx[idx].sp_hash = sp_hash;
    s->ctx[idx].sp_name = sp_name;
    s->ctx[idx].output = output;
}

void
nn_ctxstr_set_all_outputs(nn_ctxstr_t *s, uint32_t idx,
                          nn_str_t *sp_hash, nn_str_t *sp_name)
{
    NN_ASSERT(idx < s->ctx_count, "nn_ctxstr_set_all_outputs: idx out of bounds");
    s->ctx[idx].tag = NN_SCE_ALL_OUTPUTS;
    s->ctx[idx].sp_hash = sp_hash;
    s->ctx[idx].sp_name = sp_name;
    s->ctx[idx].output = NULL;
}

/* --- Accessors --- */

nn_str_t *
nn_ctxstr_text(const nn_ctxstr_t *s)
{
    return s->text;
}

uint32_t
nn_ctxstr_ctx_count(const nn_ctxstr_t *s)
{
    return s->ctx_count;
}

uint8_t
nn_ctxstr_elem_tag(const nn_ctxstr_t *s, uint32_t idx)
{
    NN_ASSERT(idx < s->ctx_count, "nn_ctxstr_elem_tag: idx out of bounds");
    return s->ctx[idx].tag;
}

nn_str_t *
nn_ctxstr_elem_hash(const nn_ctxstr_t *s, uint32_t idx)
{
    NN_ASSERT(idx < s->ctx_count, "nn_ctxstr_elem_hash: idx out of bounds");
    return s->ctx[idx].sp_hash;
}

nn_str_t *
nn_ctxstr_elem_name(const nn_ctxstr_t *s, uint32_t idx)
{
    NN_ASSERT(idx < s->ctx_count, "nn_ctxstr_elem_name: idx out of bounds");
    return s->ctx[idx].sp_name;
}

nn_str_t *
nn_ctxstr_elem_output(const nn_ctxstr_t *s, uint32_t idx)
{
    NN_ASSERT(idx < s->ctx_count, "nn_ctxstr_elem_output: idx out of bounds");
    return s->ctx[idx].output;
}

/* --- Census --- */

const nn_tracker_t *
nn_ctxstr_tracked(void)
{
    return &g_tracked;
}

uint64_t
nn_ctxstr_bytes(const nn_ctxstr_t *s)
{
    return sizeof(*s) + (uint64_t)s->ctx_count * sizeof(nn_sce_t);
}
