/*
 * nn_tracker.h - The record of every object of one kind the C data layer
 * allocates on its own.
 *
 * Attribute sets, list headers, lambdas, builtins' partial applications
 * and strings with context are malloc'd one object at a time, a set or a
 * lambda in more than one block.  Each kind keeps a tracker, adds every
 * object to it as the object is made, and frees them all at once at
 * arena teardown through its own free function.  A census (nn_stats.h)
 * reads the objects back from it, and the region collector (nn_region.h)
 * frees the objects a close finds unreached through a sweep, which keeps
 * the rest in the order they were added.
 *
 * A tracker takes objects only while an evaluation session holds the
 * arena (nn_arena.h).  An object made outside every session would sit
 * below every region and be freed by the next session's teardown under
 * whoever held it, so each kind's allocator reports failure there, as
 * the sub-arenas' allocators do.
 *
 * Not thread-safe - single-threaded evaluation only.
 */

#ifndef NN_TRACKER_H
#define NN_TRACKER_H

#include <stdint.h>

/* The objects of one kind, in the order they were added.  An all-zero
 * tracker is empty and ready to use.  `objects` moves when an add grows
 * it, and is NULL while the tracker is empty. */
typedef struct {
    void   **objects;
    uint32_t count;
    uint32_t capacity;   /* objects the array has room for */
} nn_tracker_t;

/* Add an object.  Returns 0, or -1 with the tracker unchanged outside an
 * evaluation session (nn_arena_live is 0) or when its array cannot grow. */
int nn_tracker_add(nn_tracker_t *tracker, void *object);

/* Free every object with `free_object`, and then the array: the tracker
 * is empty after. */
void nn_tracker_free_all(nn_tracker_t *tracker, void (*free_object)(void *));

/* Whether to keep an object a sweep meets at `place`, told the place it
 * takes if kept. */
typedef int (*nn_tracker_keep_t)(void *ctx, void *object, uint32_t place, uint32_t kept_at);

/* Sweep the objects from `from` on, in order: each `keep` refuses is
 * freed with `free_object`, and the kept ones close up behind the ones
 * before `from`, which stay where they are. */
void nn_tracker_sweep(nn_tracker_t *tracker, uint32_t from, nn_tracker_keep_t keep, void *ctx,
                      void (*free_object)(void *));

#endif /* NN_TRACKER_H */
