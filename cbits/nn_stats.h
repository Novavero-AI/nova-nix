/*
 * nn_stats.h - A census of the C data layer: what it holds by kind, and
 * what a set of roots reaches.
 *
 * The census reads what the allocators keep anyway (the thunk blocks, the
 * env pages, the tracked sets, list headers, lambdas, builtins' partial
 * applications and strings with context, the string arena's blocks, the
 * symbol table), so a process
 * that never takes one pays nothing for it beyond the test of
 * nn_stats_counting below.
 *
 * A census is a session.  nn_stats_begin counts what is allocated and
 * indexes every object; the caller names roots and calls nn_stats_mark,
 * which follows every pointer from them; nn_stats_end releases the index.
 * The mark frees and writes nothing in the data layer: its marks are bits
 * of its own.  It dereferences only a pointer it places at the start of
 * an object the index holds, and counts any other pointer it meets as
 * unplaced, which a graph the mark models in full never holds.  Every
 * value lives in C (nn_thunk.h), so the mark reaches all a root holds
 * without the caller.
 *
 * One session at a time, between the arena's init and destroy, with no
 * allocation in the data layer while it is open.  Not thread-safe.
 *
 * Every env and attribute set allocation also names its site: the
 * construct or builtin that makes it, which the evaluator numbers
 * (Nix.Eval.AllocSite).  Once nn_stats_count_sites turns counting on, the
 * allocators count each allocation under its site; until then an
 * allocation reads nn_stats_counting and does nothing more.
 */

#ifndef NN_STATS_H
#define NN_STATS_H

#include <stdint.h>

/* --- Kinds: the index of each kind's figures --- */

#define NN_STATS_THUNKS     0  /* thunk cells in the arena's blocks */
#define NN_STATS_ENVS       1  /* envs with their slots, and with-scope arrays */
#define NN_STATS_ATTRSETS   2  /* attribute sets: header, keys, positions, values */
#define NN_STATS_LISTS      3  /* list headers and the items arrays they span */
#define NN_STATS_LAMBDAS    4  /* lambda closures and their formal entries */
#define NN_STATS_PRIMOP_APPS 5 /* builtins' partial applications (nn_primop.h) */
#define NN_STATS_CTXSTRS    6  /* strings with context */
#define NN_STATS_STRINGS    7  /* string and path bytes in the value arena (nn_str.h) */
#define NN_STATS_SYMBOLS    8  /* interned names */
#define NN_STATS_KIND_COUNT 9

/* A count no allocator keeps: the env pages hold envs and with-scope
 * arrays side by side with no record of which is which, so without
 * counting by site from the arena's first allocation the bytes are known
 * and the number of envs is not. */
#define NN_STATS_UNCOUNTED UINT64_MAX

/* --- Sites --- */

/* How many sites the evaluator numbers for envs and for attribute sets:
 * a site is below its bound.  Nix.Eval.AllocSite enumerates exactly
 * these many. */
#define NN_STATS_ENV_SITES 16u
#define NN_STATS_SET_SITES 22u

/* Envs by the slots they hold: one bucket for each count below
 * NN_STATS_ENV_SIZES - 1, and the last for every larger count. */
#define NN_STATS_ENV_SIZES 33u

/* Nonzero while the allocators count by site. */
extern int nn_stats_counting;

/* Turn counting by site on, every figure from zero.  The counts cover
 * every env and set allocated from then on; nn_stats_allocated reports
 * the number of envs only when counting began before the env pages
 * handed out their first byte. */
void nn_stats_count_sites(void);

/* The allocators' half, called only while nn_stats_counting is nonzero.
 * An env counts once, with the slots it holds and its bytes, and every
 * other byte the env pages hand out for envs (with-scope arrays) counts
 * under the site it is for; a set counts once
 * with the bytes it holds once built, and as shareable when it equals a
 * set that already exists: one with no entries, which equals every empty
 * set, or a union one of whose operands has none, which equals the
 * other. */
void nn_stats_count_env(uint32_t site, uint32_t slots, uint64_t bytes);
void nn_stats_count_env_bytes(uint32_t site, uint64_t bytes);
void nn_stats_count_set(uint32_t site, uint64_t bytes, int shareable);

/* What counting has found since nn_stats_count_sites, zero where it has
 * never run: per env site the envs, their slots and their bytes; per
 * slot count the envs; per set site the sets, the shareable ones among
 * them and their bytes.  Each writes its figures for the first `max`
 * sites or slot counts and returns how many there are
 * (NN_STATS_ENV_SITES, NN_STATS_ENV_SIZES, NN_STATS_SET_SITES). */
uint32_t nn_stats_env_sites(uint64_t *envs, uint64_t *slots, uint64_t *bytes, uint32_t max);
uint32_t nn_stats_env_sizes(uint64_t *envs, uint32_t max);
uint32_t nn_stats_set_sites(uint64_t *sets, uint64_t *shareable, uint64_t *bytes, uint32_t max);

/* --- Session --- */

/* Count what is allocated and index every object for the mark.  Returns
 * 0, or -1 with no session open when the index cannot be allocated.  A
 * session already open is ended first. */
int nn_stats_begin(void);

/* Release the session's index.  Nothing in the data layer changes. */
void nn_stats_end(void);

/* --- What is allocated --- */

/* Objects and the bytes they hold, by kind (NN_STATS_KIND_COUNT entries
 * each).  The env count is NN_STATS_UNCOUNTED unless counting by site
 * has covered every env (nn_stats_count_sites). */
void nn_stats_allocated(uint64_t *counts, uint64_t *bytes);

/* Bytes the allocators hold that no object does: block and page headers
 * and unused tails, the tracking arrays, and the symbol table beyond its
 * text. */
uint64_t nn_stats_overhead_bytes(void);

/* Bytes the bytecode store holds, its strings included.  Code is never
 * freed, so the mark does not look at it, and a string of the code's
 * that a value points at is placed and not counted. */
uint64_t nn_stats_code_bytes(void);

/* --- The mark --- */

/* Name a root.  NULL names nothing.  A root the index cannot place is
 * counted as unplaced and not followed. */
void nn_stats_root_thunk(const void *thunk);
void nn_stats_root_env(const void *env);
void nn_stats_root_attrset(const void *set);
void nn_stats_root_list(const void *list);
void nn_stats_root_string(const void *str);
void nn_stats_root_ctxstr(const void *ctxstr);

/* Name every root the session has registered (nn_roots.h). */
void nn_stats_root_registered(void);

/* Follow every pointer from the roots named since the last call.  Marks
 * accumulate across calls.  Returns 0, or -1 when the work stack could
 * not grow, after which the session's figures are incomplete. */
int nn_stats_mark(void);

/* Objects the mark has reached and the bytes they hold, by kind.  An
 * env's with-scope array counts in the bytes of the first env over it
 * reached and not in its count; a list's items array counts in the bytes
 * of the first header over it reached. */
void nn_stats_reached(uint64_t *counts, uint64_t *bytes);

/* The mark's own work: the objects it has entered (thunks, envs,
 * with-scope arrays, sets, list headers, items arrays, lambdas, partial
 * applications, strings with context; strings and symbols hold no
 * pointer, and are marked and not entered) and their bytes, and the
 * pointers it could not place. */
void nn_stats_visited(uint64_t *objects, uint64_t *bytes, uint64_t *unplaced);

/* The reached attribute sets by their number of entries, in ascending
 * order of that number: up to `max` distinct sizes go to `sizes`, with
 * how many reached sets have each in `counts`, and the number of distinct
 * sizes, which may exceed `max`, to *distinct.  Returns 0, or -1 when the
 * sizes cannot be sorted for want of memory.  Read after the last mark. */
int nn_stats_reached_set_sizes(uint32_t *sizes, uint64_t *counts, uint32_t max,
                               uint32_t *distinct);

#endif /* NN_STATS_H */
