/*
 * nn_ctxstr.c — Context-bearing string allocation and tracking.
 *
 * Each nn_ctxstr_t is a single contiguous malloc (header + flexible
 * array of nn_sce_t elements).  Pointers are tracked in a global
 * dynamic array for bulk cleanup via nn_ctxstr_free_all().
 */

#include "nn_ctxstr.h"

#include <stdlib.h>
#include <string.h>

/* --- Tracking array --- */

#define NN_CTXSTR_INITIAL_CAPACITY 256u

static nn_ctxstr_t **g_tracked = NULL;
static uint32_t      g_tracked_count = 0;
static uint32_t      g_tracked_capacity = 0;

static void
track(nn_ctxstr_t *s)
{
    if (g_tracked_count >= g_tracked_capacity) {
        uint32_t new_cap = g_tracked_capacity == 0
                               ? NN_CTXSTR_INITIAL_CAPACITY
                               : g_tracked_capacity * 2;
        nn_ctxstr_t **new_arr = (nn_ctxstr_t **)realloc(
            g_tracked, (size_t)new_cap * sizeof(nn_ctxstr_t *));
        if (!new_arr) return;
        g_tracked = new_arr;
        g_tracked_capacity = new_cap;
    }
    g_tracked[g_tracked_count++] = s;
}

/* --- Lifecycle --- */

nn_ctxstr_t *
nn_ctxstr_new(uint32_t text, uint16_t ctx_count)
{
    size_t size = sizeof(nn_ctxstr_t) + (size_t)ctx_count * sizeof(nn_sce_t);
    nn_ctxstr_t *s = (nn_ctxstr_t *)malloc(size);
    if (!s) return NULL;
    s->text = text;
    s->ctx_count = ctx_count;
    memset(s->ctx, 0, (size_t)ctx_count * sizeof(nn_sce_t));
    track(s);
    return s;
}

void
nn_ctxstr_free_all(void)
{
    uint32_t i;
    for (i = 0; i < g_tracked_count; i++) {
        free(g_tracked[i]);
    }
    free(g_tracked);
    g_tracked = NULL;
    g_tracked_count = 0;
    g_tracked_capacity = 0;
}

/* --- Element setters --- */

void
nn_ctxstr_set_plain(nn_ctxstr_t *s, uint16_t idx,
                    uint32_t sp_hash, uint32_t sp_name)
{
    s->ctx[idx].tag = NN_SCE_PLAIN;
    s->ctx[idx].sp_hash = sp_hash;
    s->ctx[idx].sp_name = sp_name;
    s->ctx[idx].output = 0;
}

void
nn_ctxstr_set_drv_output(nn_ctxstr_t *s, uint16_t idx,
                         uint32_t sp_hash, uint32_t sp_name,
                         uint32_t output)
{
    s->ctx[idx].tag = NN_SCE_DRV_OUTPUT;
    s->ctx[idx].sp_hash = sp_hash;
    s->ctx[idx].sp_name = sp_name;
    s->ctx[idx].output = output;
}

void
nn_ctxstr_set_all_outputs(nn_ctxstr_t *s, uint16_t idx,
                          uint32_t sp_hash, uint32_t sp_name)
{
    s->ctx[idx].tag = NN_SCE_ALL_OUTPUTS;
    s->ctx[idx].sp_hash = sp_hash;
    s->ctx[idx].sp_name = sp_name;
    s->ctx[idx].output = 0;
}

/* --- Accessors --- */

uint32_t
nn_ctxstr_text(const nn_ctxstr_t *s)
{
    return s->text;
}

uint16_t
nn_ctxstr_ctx_count(const nn_ctxstr_t *s)
{
    return s->ctx_count;
}

uint8_t
nn_ctxstr_elem_tag(const nn_ctxstr_t *s, uint16_t idx)
{
    return s->ctx[idx].tag;
}

uint32_t
nn_ctxstr_elem_hash(const nn_ctxstr_t *s, uint16_t idx)
{
    return s->ctx[idx].sp_hash;
}

uint32_t
nn_ctxstr_elem_name(const nn_ctxstr_t *s, uint16_t idx)
{
    return s->ctx[idx].sp_name;
}

uint32_t
nn_ctxstr_elem_output(const nn_ctxstr_t *s, uint16_t idx)
{
    return s->ctx[idx].output;
}
