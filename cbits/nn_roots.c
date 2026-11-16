/*
 * nn_roots.c - The roots of an evaluation session (nn_roots.h).
 *
 * Two growable arrays, one per kind of root, doubling as they fill.
 */

#include "nn_roots.h"

#include <stdlib.h>

/* The first capacity of each array, which doubles from there. */
#define NN_ROOTS_INITIAL 64u

typedef struct {
    void   **items;
    uint32_t count;
    uint32_t cap;
} nn_root_list_t;

static nn_root_list_t g_thunks;
static nn_root_list_t g_envs;

/* Returns 0, or -1 when the list cannot grow. */
static int push(nn_root_list_t *list, void *root)
{
    if (!root) return 0;
    if (list->count == list->cap) {
        uint32_t cap;
        void **grown;
        if (list->cap > UINT32_MAX / 2) return -1;
        cap = list->cap ? list->cap * 2 : NN_ROOTS_INITIAL;
        grown = (void **)realloc(list->items, (size_t)cap * sizeof(void *));
        if (!grown) return -1;
        list->items = grown;
        list->cap = cap;
    }
    list->items[list->count++] = root;
    return 0;
}

static void release(nn_root_list_t *list)
{
    free(list->items);
    list->items = NULL;
    list->count = 0;
    list->cap = 0;
}

int nn_roots_add_thunk(struct nn_thunk *thunk) { return push(&g_thunks, thunk); }
int nn_roots_add_env(struct nn_env *env)       { return push(&g_envs, env); }

uint32_t nn_roots_thunk_count(void) { return g_thunks.count; }
uint32_t nn_roots_env_count(void)   { return g_envs.count; }

struct nn_thunk *
nn_roots_thunk(uint32_t index)
{
    return index < g_thunks.count ? (struct nn_thunk *)g_thunks.items[index] : NULL;
}

struct nn_env *
nn_roots_env(uint32_t index)
{
    return index < g_envs.count ? (struct nn_env *)g_envs.items[index] : NULL;
}

void
nn_roots_mark(uint32_t *thunks, uint32_t *envs)
{
    *thunks = g_thunks.count;
    *envs = g_envs.count;
}

void
nn_roots_release(uint32_t thunks, uint32_t envs)
{
    if (thunks < g_thunks.count) g_thunks.count = thunks;
    if (envs < g_envs.count) g_envs.count = envs;
}

void
nn_roots_clear(void)
{
    release(&g_thunks);
    release(&g_envs);
}
