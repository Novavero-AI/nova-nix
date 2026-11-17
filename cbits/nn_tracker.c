/*
 * nn_tracker.c - The record of every object of one kind (nn_tracker.h).
 *
 * A pointer array that doubles as it fills.
 */

#include "nn_tracker.h"

#include <stdlib.h>

/* The first capacity of the array, which doubles from there. */
#define NN_TRACKER_INITIAL_CAPACITY 256u

int nn_tracker_add(nn_tracker_t *tracker, void *object)
{
    if (tracker->count == tracker->capacity) {
        uint32_t cap;
        void **grown;
        if (tracker->capacity > UINT32_MAX / 2) return -1;
        cap = tracker->capacity ? tracker->capacity * 2 : NN_TRACKER_INITIAL_CAPACITY;
        grown = (void **)realloc(tracker->objects, (size_t)cap * sizeof(void *));
        if (!grown) return -1;
        tracker->objects = grown;
        tracker->capacity = cap;
    }
    tracker->objects[tracker->count++] = object;
    return 0;
}

void nn_tracker_free_all(nn_tracker_t *tracker, void (*free_object)(void *))
{
    uint32_t i;
    for (i = 0; i < tracker->count; i++) free_object(tracker->objects[i]);
    free(tracker->objects);
    tracker->objects = NULL;
    tracker->count = 0;
    tracker->capacity = 0;
}
