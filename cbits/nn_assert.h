#ifndef NN_ASSERT_H
#define NN_ASSERT_H

#include <stdio.h>
#include <stdlib.h>

/*
 * An invariant check that holds in every build, NDEBUG or not: a breach
 * prints its message and site and aborts.
 *
 * Each invariant is guarded once, where its bound is known:
 *   - An allocator asserts.  Its NULL means only that it could not
 *     allocate (no live arena, exhaustion, or a size past its limit),
 *     which the Haskell boundary reports as a resource failure, so a
 *     breach must not share it.
 *   - A reader whose caller knows the bound asserts.  The caller checks
 *     a list's length, a set's size or an application's form before it
 *     reads, so a breach is a defect in the caller.
 *   - A reader whose bound only the C side knows answers NULL for a
 *     breach: an index into the bytecode string table, an env
 *     coordinate past its chain or slots, a builtin past the static
 *     cells.  Its Haskell binding raises that NULL as CInvariantViolated
 *     naming the C function, so the defect is reported with its site and
 *     a test can provoke it.
 *   - A function with no NULL to spare, a setter or a reader of a scalar
 *     such as an opcode or a data word, asserts.
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
