/*
 * nn_stats.h - A census of the C data layer: what it holds by kind, and
 * what a set of roots reaches.
 *
 * The census reads what the allocators keep anyway (the thunk blocks, the
 * env pages, the tracked sets, list headers, lambdas and strings, the
 * symbol table) and adds no work to any allocation, so a process that
 * never takes one pays nothing for it.
 *
 * A census is a session.  nn_stats_begin counts what is allocated and
 * indexes every object; the caller names roots and calls nn_stats_mark,
 * which follows every pointer from them; nn_stats_end releases the index.
 * The mark frees and writes nothing in the data layer: its marks are bits
 * of its own.  It dereferences only a pointer it places at the start of
 * an object the index holds, and counts any other pointer it meets as
 * unplaced, which a graph the mark models in full never holds.
 *
 * A StablePtr payload (a COMPUTED thunk tagged NN_VALUE_PTR) is a Haskell
 * value, which can hold C pointers C cannot see.  The mark keeps each one
 * it reaches for the caller (nn_stats_take_stableptrs), which can name
 * what the value holds as further roots and mark again.
 *
 * One session at a time, between the arena's init and destroy, with no
 * allocation in the data layer while it is open.  Not thread-safe.
 */

#ifndef NN_STATS_H
#define NN_STATS_H

#include <stdint.h>

/* --- Kinds: the index of each kind's figures --- */

#define NN_STATS_THUNKS     0  /* thunk cells in the arena's blocks */
#define NN_STATS_ENVS       1  /* env structs, their slot and with-scope arrays */
#define NN_STATS_ATTRSETS   2  /* attribute sets: header, keys, positions, values */
#define NN_STATS_LISTS      3  /* list headers and the items arrays they span */
#define NN_STATS_LAMBDAS    4  /* lambda closures and their formal entries */
#define NN_STATS_CTXSTRS    5  /* strings with context */
#define NN_STATS_SYMBOLS    6  /* interned text: every name and context-free string */
#define NN_STATS_KIND_COUNT 7

/* A count no allocator keeps: the env pages hold env structs and arrays
 * side by side with no record of which is which, so the bytes are known
 * and the number of envs is not. */
#define NN_STATS_UNCOUNTED UINT64_MAX

/* --- Session --- */

/* Count what is allocated and index every object for the mark.  Returns
 * 0, or -1 with no session open when the index cannot be allocated.  A
 * session already open is ended first. */
int nn_stats_begin(void);

/* Release the session's index.  Nothing in the data layer changes. */
void nn_stats_end(void);

/* --- What is allocated --- */

/* Objects and the bytes they hold, by kind (NN_STATS_KIND_COUNT entries
 * each).  The env count is NN_STATS_UNCOUNTED. */
void nn_stats_allocated(uint64_t *counts, uint64_t *bytes);

/* Bytes the allocators hold that no object does: block and page headers
 * and unused tails, the tracking arrays, and the symbol table beyond its
 * text. */
uint64_t nn_stats_overhead_bytes(void);

/* Bytes the bytecode store holds.  Code is never freed, so the mark
 * does not look at it. */
uint64_t nn_stats_code_bytes(void);

/* The StablePtrs the arena holds: one per COMPUTED NN_VALUE_PTR thunk. */
uint64_t nn_stats_stableptrs(void);

/* --- The mark --- */

/* Name a root.  NULL names nothing.  A root the index cannot place is
 * counted as unplaced and not followed. */
void nn_stats_root_thunk(const void *thunk);
void nn_stats_root_env(const void *env);
void nn_stats_root_attrset(const void *set);
void nn_stats_root_list(const void *list);

/* Follow every pointer from the roots named since the last call.  Marks
 * accumulate across calls.  Returns 0, or -1 when the work stack or the
 * StablePtr list could not grow, after which the session's figures are
 * incomplete. */
int nn_stats_mark(void);

/* The StablePtr payloads the mark has reached and not yet handed over:
 * how many there are, and up to `max` of them written to `out`, which
 * hands them over.  Returns the number written. */
uint32_t nn_stats_pending_stableptrs(void);
uint32_t nn_stats_take_stableptrs(void **out, uint32_t max);

/* Objects the mark has reached and the bytes they hold, by kind.  An
 * env's arrays count in its bytes and not in its count; a list's items
 * array counts in the bytes of the first header over it reached. */
void nn_stats_reached(uint64_t *counts, uint64_t *bytes);

/* The mark's own work: the objects it has entered (thunks, env structs,
 * slot and with-scope arrays, sets, list headers, items arrays, lambdas,
 * strings with context; symbols are marked and not entered) and their
 * bytes, the pointers it could not place, and the StablePtr payloads it
 * has reached. */
void nn_stats_visited(uint64_t *objects, uint64_t *bytes,
                      uint64_t *unplaced, uint64_t *stableptrs);

/* The reached attribute sets by their number of entries, in ascending
 * order of that number: up to `max` distinct sizes go to `sizes`, with
 * how many reached sets have each in `counts`, and the number of distinct
 * sizes, which may exceed `max`, to *distinct.  Returns 0, or -1 when the
 * sizes cannot be sorted for want of memory.  Read after the last mark. */
int nn_stats_reached_set_sizes(uint32_t *sizes, uint64_t *counts, uint32_t max,
                               uint32_t *distinct);

#endif /* NN_STATS_H */
