/*
 * nn_list.c — Contiguous array of thunk pointers for lists.
 *
 * Each nn_list_t is a small header (malloc'd, tracked) pointing to
 * a contiguous items array (page-allocated via nn_env_alloc_slots).
 * Bulk cleanup frees all headers; the page allocator frees items.
 */

#include "nn_list.h"
#include "nn_env.h"
#include "nn_assert.h"

#include <stdlib.h>

/* --- Global tracking for bulk cleanup --- */

static nn_list_t **g_tracked = NULL;
static uint32_t g_tracked_count = 0;
static uint32_t g_tracked_cap   = 0;

static void nn_list_track(nn_list_t *list)
{
    if (g_tracked_count >= g_tracked_cap) {
        uint32_t new_cap = g_tracked_cap ? g_tracked_cap * 2 : 256;
        nn_list_t **new_arr = (nn_list_t **)realloc(
            g_tracked, (size_t)new_cap * sizeof(nn_list_t *));
        if (!new_arr) return;
        g_tracked = new_arr;
        g_tracked_cap = new_cap;
    }
    g_tracked[g_tracked_count++] = list;
}

/* --- Lifecycle --- */

nn_list_t *
nn_list_new(uint32_t count)
{
    if (count == 0) return NULL;

    nn_list_t *list = (nn_list_t *)malloc(sizeof(nn_list_t));
    if (!list) return NULL;

    /* Items array via the env page allocator (O(1) bump allocation).
     * nn_env_alloc_slots returns void**, which we cast to nn_thunk**.
     * All slots are zero-initialized (NULL pointers). */
    list->items = (struct nn_thunk **)nn_env_alloc_slots(count);
    list->count = count;

    if (!list->items) {
        free(list);
        return NULL;
    }

    nn_list_track(list);
    return list;
}

void
nn_list_free_all(void)
{
    uint32_t i;
    for (i = 0; i < g_tracked_count; i++) {
        free(g_tracked[i]);
    }
    free(g_tracked);
    g_tracked = NULL;
    g_tracked_count = 0;
    g_tracked_cap = 0;
}

/* --- Access --- */

uint32_t
nn_list_count(const nn_list_t *list)
{
    return list->count;
}

struct nn_thunk *
nn_list_get(const nn_list_t *list, uint32_t index)
{
    NN_ASSERT(index < list->count, "nn_list_get: index out of bounds");
    return list->items[index];
}

void
nn_list_set(nn_list_t *list, uint32_t index, struct nn_thunk *thunk)
{
    NN_ASSERT(index < list->count, "nn_list_set: index out of bounds");
    list->items[index] = thunk;
}
