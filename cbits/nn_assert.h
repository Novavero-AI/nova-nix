#ifndef NN_ASSERT_H
#define NN_ASSERT_H

#include <stdio.h>
#include <stdlib.h>

/*
 * Debug-mode bounds checking.  Compiles to nothing under NDEBUG.
 * Use for array accesses, index validation, and invariant checks.
 */
#ifdef NDEBUG
#define NN_ASSERT(cond, msg) ((void)0)
#else
#define NN_ASSERT(cond, msg) \
    do { \
        if (!(cond)) { \
            fprintf(stderr, "NN_ASSERT failed: %s\n  at %s:%d\n", \
                    (msg), __FILE__, __LINE__); \
            abort(); \
        } \
    } while (0)
#endif

#endif /* NN_ASSERT_H */
