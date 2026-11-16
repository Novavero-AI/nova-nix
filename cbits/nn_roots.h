/*
 * nn_roots.h - The roots of an evaluation session in the C data layer.
 *
 * A value Haskell keeps beyond one evaluation step points into the C
 * data layer from outside it, where no mark over the C graph can find
 * it.  Each such pointer is registered here when it is made: a session's
 * base environment and search path as the session starts, and an
 * imported file's value as the import cache takes it.  A mark starts
 * from every registered root as well as from the roots its caller names
 * (nn_stats.h), as upstream's collector scans the import cache it keeps
 * in traceable memory (fileEvalCache, eval.hh at 2.24.9).
 *
 * Sessions nest, so roots do too: a session marks the registry as it
 * starts and releases back to the mark as it ends, forgetting every root
 * registered within it; the arena's teardown clears what is left.  Not
 * thread-safe.
 */

#ifndef NN_ROOTS_H
#define NN_ROOTS_H

#include <stdint.h>

struct nn_thunk;
struct nn_env;

/* Register a thunk cell or an environment as a root.  Returns 0, or -1
 * when the registry cannot grow; NULL registers nothing and returns 0. */
int nn_roots_add_thunk(struct nn_thunk *thunk);
int nn_roots_add_env(struct nn_env *env);

/* How many roots of each kind are registered, and the one at an index
 * below that count, in the order they were registered; NULL past it. */
uint32_t nn_roots_thunk_count(void);
uint32_t nn_roots_env_count(void);
struct nn_thunk *nn_roots_thunk(uint32_t index);
struct nn_env *nn_roots_env(uint32_t index);

/* The registry's size now, to release back to. */
void nn_roots_mark(uint32_t *thunks, uint32_t *envs);

/* Forget every root registered since the mark, which an earlier
 * nn_roots_mark returned with no release past it since. */
void nn_roots_release(uint32_t thunks, uint32_t envs);

/* Forget every root and release the registry's memory. */
void nn_roots_clear(void);

#endif /* NN_ROOTS_H */
