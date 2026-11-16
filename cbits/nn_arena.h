/*
 * nn_arena.h - Whether the C data layer's arenas are live.
 *
 * nn_arena_live answers whether every sub-arena is inside its init ..
 * destroy window.  Outside it, each sub-arena's allocators report
 * failure through their ordinary sentinel (NULL, UINT32_MAX,
 * NN_SYMBOL_INVALID) instead of touching freed or never-allocated
 * state; the Haskell boundary asks this on that failure path to tell a
 * missing arenaInit from exhaustion.
 */

#ifndef NN_ARENA_H
#define NN_ARENA_H


/* Bulk-free all tracked attribute sets (called during arena teardown). */
void nn_attrset_free_all(void);

/* Nonzero when the symbol table, string arena, thunk arena, env
 * allocator and bytecode store are all initialized. */
int nn_arena_live(void);

#endif /* NN_ARENA_H */
