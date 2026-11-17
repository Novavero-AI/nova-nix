#ifndef NN_ASSERT_H
#define NN_ASSERT_H

#include <stdio.h>
#include <stdlib.h>

/*
 * An invariant check that holds in every build, NDEBUG or not: a breach
 * prints its message and site and aborts.  It guards an index or pointer
 * this library produced itself, where a breach means corrupt state.  An
 * answer the Haskell boundary checks (a NULL it raises as
 * CInvariantViolated) is returned instead, not asserted, so that the
 * defect is reported with its site.
 */
#define NN_ASSERT(cond, msg) \
    do { \
        if (!(cond)) { \
            fprintf(stderr, "NN_ASSERT failed: %s\n  at %s:%d\n", \
                    (msg), __FILE__, __LINE__); \
            abort(); \
        } \
    } while (0)

#endif /* NN_ASSERT_H */
