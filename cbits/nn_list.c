/*
 * nn_list.c - Contiguous array of thunk pointers for lists.
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

/* Returns 0, or -1 when the tracking array cannot grow. */
static int nn_list_track(nn_list_t *list)
{
    if (g_tracked_count >= g_tracked_cap) {
        if (g_tracked_cap > UINT32_MAX / 2) return -1;
        uint32_t new_cap = g_tracked_cap ? g_tracked_cap * 2 : 256;
        nn_list_t **new_arr = (nn_list_t **)realloc(
            g_tracked, (size_t)new_cap * sizeof(nn_list_t *));
        if (!new_arr) return -1;
        g_tracked = new_arr;
        g_tracked_cap = new_cap;
    }
    g_tracked[g_tracked_count++] = list;
    return 0;
}

/* --- Lifecycle --- */

int
nn_list_new(nn_list_t **out, uint32_t count)
{
    *out = NULL;
    if (count == 0) return 0;

    nn_list_t *list = (nn_list_t *)malloc(sizeof(nn_list_t));
    if (!list) return -1;

    /* Items array via the env page allocator (O(1) bump allocation).
     * nn_env_alloc_slots returns void**, which we cast to nn_thunk**.
     * All slots are zero-initialized (NULL pointers).  When tracking
     * fails the items array stays in its page, which only nn_env_destroy
     * releases. */
    list->items = (struct nn_thunk **)nn_env_alloc_slots(count);
    list->count = count;

    if (!list->items || nn_list_track(list) != 0) {
        free(list);
        return -1;
    }

    *out = list;
    return 0;
}

int
nn_list_drop(nn_list_t **out, const nn_list_t *list, uint32_t n)
{
    *out = NULL;
    if (!list || n >= list->count) return 0;

    nn_list_t *rest = (nn_list_t *)malloc(sizeof(nn_list_t));
    if (!rest) return -1;

    rest->items = list->items + n;
    rest->count = list->count - n;
    if (nn_list_track(rest) != 0) {
        free(rest);
        return -1;
    }

    *out = rest;
    return 0;
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
