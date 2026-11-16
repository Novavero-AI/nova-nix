/*
 * nn_primop.c - A builtin's partial application for nova-nix.
 *
 * Each nn_primop_app_t is one malloc'd block, its argument cells inline
 * after the header, tracked in a global array for bulk cleanup at
 * evaluation end.
 */

#include "nn_primop.h"
#include "nn_assert.h"

#include <stdlib.h>
#include <string.h>

/* --- Global tracking for bulk cleanup --- */

static nn_primop_app_t **g_tracked = NULL;
static uint32_t g_tracked_count = 0;
static uint32_t g_tracked_cap   = 0;

/* The first capacity of the tracking array, which doubles from there. */
#define NN_PRIMOP_APP_TRACK_INITIAL 256u

/* Returns 0, or -1 when the tracking array cannot grow. */
static int track(nn_primop_app_t *app)
{
    if (g_tracked_count >= g_tracked_cap) {
        uint32_t cap;
        nn_primop_app_t **grown;
        if (g_tracked_cap > UINT32_MAX / 2) return -1;
        cap = g_tracked_cap ? g_tracked_cap * 2 : NN_PRIMOP_APP_TRACK_INITIAL;
        grown = (nn_primop_app_t **)realloc(g_tracked, (size_t)cap * sizeof(nn_primop_app_t *));
        if (!grown) return -1;
        g_tracked = grown;
        g_tracked_cap = cap;
    }
    g_tracked[g_tracked_count++] = app;
    return 0;
}

/* --- Lifecycle --- */

nn_primop_app_t *
nn_primop_app_new(uint32_t primop, uint16_t forced,
                  struct nn_thunk *const *args, uint16_t taken)
{
    nn_primop_app_t *app;
    size_t arg_bytes;
    if (taken == 0 || taken > NN_PRIMOP_APP_MAX_ARGS || !args) return NULL;
    arg_bytes = (size_t)taken * sizeof(struct nn_thunk *);
    app = (nn_primop_app_t *)malloc(sizeof(nn_primop_app_t) + arg_bytes);
    if (!app) return NULL;
    app->primop = primop;
    app->taken = taken;
    app->forced = forced;
    memcpy(app->args, args, arg_bytes);
    if (track(app) != 0) {
        free(app);
        return NULL;
    }
    return app;
}

void
nn_primop_app_free_all(void)
{
    uint32_t i;
    for (i = 0; i < g_tracked_count; i++) free(g_tracked[i]);
    free(g_tracked);
    g_tracked = NULL;
    g_tracked_count = 0;
    g_tracked_cap = 0;
}

/* --- Accessors --- */

uint32_t
nn_primop_app_primop(const nn_primop_app_t *app)
{
    return app->primop;
}

uint16_t
nn_primop_app_taken(const nn_primop_app_t *app)
{
    return app->taken;
}

uint16_t
nn_primop_app_forced(const nn_primop_app_t *app)
{
    return app->forced;
}

/* The bound stays live under NDEBUG: the index crosses the FFI, so a
 * violated bound returns NULL rather than reading past the arguments. */
struct nn_thunk *
nn_primop_app_arg(const nn_primop_app_t *app, uint16_t i)
{
    NN_ASSERT(i < app->taken, "nn_primop_app_arg: index past the arguments taken");
    if (i >= app->taken) return NULL;
    return app->args[i];
}

/* --- Census --- */

nn_primop_app_t *const *
nn_primop_app_tracked(uint32_t *count, uint32_t *capacity)
{
    *count = g_tracked_count;
    *capacity = g_tracked_cap;
    return g_tracked;
}

uint64_t
nn_primop_app_bytes(const nn_primop_app_t *app)
{
    return sizeof(*app) + (uint64_t)app->taken * sizeof(struct nn_thunk *);
}
