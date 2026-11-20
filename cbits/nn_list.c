/*
 * nn_list.c - Contiguous array of thunk pointers for lists.
 *
 * Each nn_list_t is a small header (malloc'd and tracked, nn_tracker.h)
 * pointing to a contiguous items array (page-allocated via
 * nn_env_alloc_items).  Bulk cleanup frees all headers; the page
 * allocator frees items.
 */

#include "nn_list.h"
#include "nn_env.h"
#include "nn_assert.h"
#include "nn_region.h"

#include <stdlib.h>

/* Every header allocated, for bulk cleanup. */
static nn_tracker_t g_tracked;

/* --- Lifecycle --- */

int
nn_list_new(nn_list_t **out, uint32_t count)
{
    *out = NULL;
    if (count == 0) return 0;

    nn_list_t *list = (nn_list_t *)malloc(sizeof(nn_list_t));
    if (!list) return -1;

    /* The items array is zeroed in an env page.  When tracking fails it
     * stays there, since only nn_env_destroy releases a page. */
    list->items = (struct nn_thunk **)nn_env_alloc_items(count);
    list->count = count;

    if (!list->items || nn_tracker_add(&g_tracked, list) != 0) {
        free(list);
        return -1;
    }
    if (nn_region_census) nn_region_list(list, g_tracked.count - 1, NULL);

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
    if (nn_tracker_add(&g_tracked, rest) != 0) {
        free(rest);
        return -1;
    }
    if (nn_region_census) nn_region_list(rest, g_tracked.count - 1, list);

    *out = rest;
    return 0;
}

void
nn_list_free_all(void)
{
    nn_tracker_free_all(&g_tracked, free);
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

/* --- Census --- */

const nn_tracker_t *
nn_list_tracked(void)
{
    return &g_tracked;
}
