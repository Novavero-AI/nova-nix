/*
 * nn_arena.h - Whether the C data layer's arenas are live.
 *
 * nn_arena_live answers whether every sub-arena is inside its init ..
 * destroy window, which an evaluation session opens.  Outside it, each
 * sub-arena's allocators report failure through their ordinary sentinel
 * (NULL, UINT32_MAX, NN_SYMBOL_INVALID) instead of touching freed or
 * never-allocated state.
 *
 * The Haskell boundary asks it in three places: after an allocator's
 * failure, to tell a call outside an evaluation session from
 * exhaustion (Nix.Eval.CStatus); as a session starts, to share the
 * arenas of a session already running rather than set up its own
 * (Nix.Eval.IO.withEvalSession); and for a symbol id the table does not
 * hold, to tell a call outside a session from a defect
 * (Nix.Eval.Symbol).
 */

#ifndef NN_ARENA_H
#define NN_ARENA_H

/* Nonzero when the symbol table, string arena, thunk arena, env
 * allocator and bytecode store are all initialized. */
int nn_arena_live(void);

#endif /* NN_ARENA_H */
