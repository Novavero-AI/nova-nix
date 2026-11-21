/*
 * nn_primop.c - A builtin's partial application for nova-nix.
 *
 * Each nn_primop_app_t is one malloc'd block, its argument cells inline
 * after the header, tracked (nn_tracker.h) for bulk cleanup at
 * evaluation end.
 */

#include "nn_primop.h"
#include "nn_assert.h"
#include "nn_region.h"

#include <stdlib.h>
#include <string.h>

/* Every application allocated, for bulk cleanup. */
static nn_tracker_t g_tracked;

/* --- Lifecycle --- */

nn_primop_app_t *
nn_primop_app_new(uint32_t primop, uint16_t forced,
                  struct nn_thunk *const *args, uint16_t taken)
{
    nn_primop_app_t *app;
    size_t arg_bytes;
    NN_ASSERT(taken > 0 && taken <= NN_PRIMOP_APP_MAX_ARGS,
              "nn_primop_app_new: an application takes 1 to NN_PRIMOP_APP_MAX_ARGS arguments");
    NN_ASSERT(args != NULL, "nn_primop_app_new: no argument cells");
    arg_bytes = (size_t)taken * sizeof(struct nn_thunk *);
    app = (nn_primop_app_t *)malloc(sizeof(nn_primop_app_t) + arg_bytes);
    if (!app) return NULL;
    app->primop = primop;
    app->taken = taken;
    app->forced = forced;
    memcpy(app->args, args, arg_bytes);
    if (nn_tracker_add(&g_tracked, app) != 0) {
        free(app);
        return NULL;
    }
    if (nn_region_mode)
        nn_region_tracked(NN_REGION_PRIMOP_APP, app, g_tracked.count - 1, nn_primop_app_bytes(app));
    return app;
}

void
nn_primop_app_free_all(void)
{
    nn_tracker_free_all(&g_tracked, free);
}

void
nn_primop_app_sweep(uint32_t from, nn_tracker_keep_t keep, void *ctx)
{
    nn_tracker_sweep(&g_tracked, from, keep, ctx, free);
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

struct nn_thunk *
nn_primop_app_arg(const nn_primop_app_t *app, uint16_t i)
{
    NN_ASSERT(i < app->taken, "nn_primop_app_arg: index past the arguments taken");
    return app->args[i];
}

/* --- Census --- */

const nn_tracker_t *
nn_primop_app_tracked(void)
{
    return &g_tracked;
}

uint64_t
nn_primop_app_bytes(const nn_primop_app_t *app)
{
    return sizeof(*app) + (uint64_t)app->taken * sizeof(struct nn_thunk *);
}
