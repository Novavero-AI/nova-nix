/*
 * nn_primop.h - A builtin's partial application for nova-nix.
 *
 * A builtin applied to fewer arguments than it takes is a value: the
 * builtin and the cells of the arguments it has taken, as upstream's
 * tPrimOpApp is a primop and the values applied to it (value.hh at
 * 2.24.9).  A thunk holds one under NN_VALUE_PRIMOP_APP; a builtin with
 * no argument taken is its index, inline under NN_VALUE_PRIMOP
 * (nn_thunk.h).  The builtin is its index in the evaluator's table
 * (Nix.Eval.Builtin), which C does not interpret.
 *
 * Each argument is a cell, so the value graph stays in C: a mark follows
 * the cells like a list's items.  Which arguments were taken forced is
 * recorded beside them, so the evaluator reads back the value it stored.
 *
 * Applications are malloc'd and tracked for bulk cleanup via
 * nn_primop_app_free_all() at arena teardown.  Not thread-safe.
 */

#ifndef NN_PRIMOP_H
#define NN_PRIMOP_H

#include "nn_tracker.h"

#include <stdint.h>

struct nn_thunk;

/* The most arguments one application holds: one bit each in `forced`. */
#define NN_PRIMOP_APP_MAX_ARGS 16u

/* A builtin and the arguments it has taken, first to last.  Bit i of
 * `forced` is set when argument i was taken forced, so its cell holds
 * the value it was taken as; a clear bit is an argument held as the
 * thunk it arrived as. */
typedef struct nn_primop_app {
    uint32_t         primop;
    uint16_t         taken;
    uint16_t         forced;
    struct nn_thunk *args[];
} nn_primop_app_t;

/* --- Lifecycle --- */

/* A partial application of `primop` to the `taken` cells at `args`.
 * Returns NULL on allocation failure.  `taken` is 1 to
 * NN_PRIMOP_APP_MAX_ARGS, and `args` holds that many cells: a count
 * outside that range, or NULL `args`, aborts (NN_ASSERT).  A builtin
 * with none taken is a NN_VALUE_PRIMOP cell, not an application. */
nn_primop_app_t *nn_primop_app_new(uint32_t primop, uint16_t forced,
                                   struct nn_thunk *const *args, uint16_t taken);

/* Free every tracked application.  Call once at evaluation end. */
void nn_primop_app_free_all(void);

/* --- Accessors --- */

uint32_t nn_primop_app_primop(const nn_primop_app_t *app);
uint16_t nn_primop_app_taken(const nn_primop_app_t *app);
uint16_t nn_primop_app_forced(const nn_primop_app_t *app);

/* Argument `i`'s cell; `i` must be below the arguments taken, and an
 * index past them aborts (NN_ASSERT). */
struct nn_thunk *nn_primop_app_arg(const nn_primop_app_t *app, uint16_t i);

/* --- Census (nn_stats.h) --- */

/* Every application allocated and not yet freed: the module's own
 * tracker, whose array moves when the next application is made and is
 * freed by nn_primop_app_free_all. */
const nn_tracker_t *nn_primop_app_tracked(void);

/* Free the applications from place `from` of the tracker on that `keep`
 * refuses, and close up the rest (nn_tracker_sweep). */
void nn_primop_app_sweep(uint32_t from, nn_tracker_keep_t keep, void *ctx);

/* Bytes an application holds from the allocator. */
uint64_t nn_primop_app_bytes(const nn_primop_app_t *app);

#endif /* NN_PRIMOP_H */
