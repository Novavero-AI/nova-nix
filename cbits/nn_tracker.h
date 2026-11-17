/*
 * nn_tracker.h - The record of every object of one kind the C data layer
 * allocates on its own.
 *
 * Attribute sets, list headers, lambdas, builtins' partial applications
 * and strings with context are malloc'd one object at a time, a set or a
 * lambda in more than one block.  Each kind keeps a tracker, adds every
 * object to it as the object is made, and frees them all at once at
 * arena teardown through its own free function.  A census (nn_stats.h)
 * reads the objects back from it.
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

/* Add an object.  Returns 0, or -1 with the tracker unchanged when its
 * array cannot grow. */
int nn_tracker_add(nn_tracker_t *tracker, void *object);

/* Free every object with `free_object`, and then the array: the tracker
 * is empty after. */
void nn_tracker_free_all(nn_tracker_t *tracker, void (*free_object)(void *));

#endif /* NN_TRACKER_H */
