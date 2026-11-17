/*
 * nn_env.h - C-native evaluation environments.
 *
 * Nix evaluation creates millions of environments.  An env is a word and
 * its slots, followed by the fields its word says it has:
 *
 *   word                 the parent and slot count, or a header
 *   slots[slot_count]    one nn_thunk_t * each
 *   parent               an nn_env_t *, when a header has NN_ENV_PARENT
 *   scopes               an nn_env_scopes_t, when a header has NN_ENV_SCOPES
 *
 * An env of 1 to NN_ENV_SHORT_MAX slots and no scopes is short: its word
 * is its parent's address, or 0 for none, with the slot count in the low
 * bits every env's alignment leaves clear, so it takes a word and a
 * pointer a slot, as upstream's Env is a parent and its values
 * (eval.hh:135-139 at 2.24.9).  A call's env, the most common, is short,
 * and so is nearly every frame.  Any other env's word is a header: those
 * bits clear, which fields follow the slots, and the slot count.  Only a
 * root env (the builtins, or scopedImport's names over them), a frame
 * whose names are not all static, a with and a trimmed closure under a
 * with have scopes.  Everything an env points to is found from the env
 * alone: its word gives the slot count and which fields follow them.
 *
 * The slots follow the word as a C99 flexible array member, so the
 * compiler places them at a pointer-aligned offset, which nn_env_t's size
 * check below pins at NN_ENV_WORD_BYTES.  Every env starts at an
 * NN_ENV_ALIGNMENT boundary of its page, and every field after the slots
 * starts a whole number of pointers past them, so every slot, parent and
 * scopes field is pointer-aligned.
 *
 * Envs, the with-scope arrays, the list items arrays (nn_list.h) and the
 * cells of deferred applications (nn_thunk.h) are allocated from a
 * page-based bump allocator.  Nothing is individually freed -
 * nn_env_destroy() tears down everything at eval end.
 *
 * Every allocation for an env names its site, the construct or builtin
 * that makes the env (nn_stats.h), under which the census counts it.
 *
 * Lifecycle: nn_env_init() before evaluation, nn_env_destroy() after.
 * Not thread-safe - single-threaded evaluation only.
 */

#ifndef NN_ENV_H
#define NN_ENV_H

#include <stddef.h>
#include <stdint.h>

/* --- Env struct --- */

typedef struct nn_env {
    uintptr_t word;        /* the parent and slot count, or a header */
    void     *slots[];     /* nn_thunk_t *, NULL until the frame is filled */
} nn_env_t;

/* The low bits of a word: a short env's slot count, and 0 in a header.
 * An env's address is a multiple of NN_ENV_ALIGNMENT, which nn_env.c
 * checks exceeds them, so a parent's address has them clear. */
#define NN_ENV_SHORT_MASK ((uintptr_t)7)
#define NN_ENV_SHORT_MAX  7u

/* A header: the fields that follow the slots, in this order, and the
 * slot count from NN_ENV_COUNT_SHIFT up. */
#define NN_ENV_PARENT ((uintptr_t)8)    /* the enclosing env */
#define NN_ENV_SCOPES ((uintptr_t)16)   /* an nn_env_scopes_t */
#define NN_ENV_COUNT_SHIFT 8

/* What an env with NN_ENV_SCOPES has: a lazy scope, an attribute set
 * whose names it binds (the root env's builtins, a frame whose names
 * are not all static), and its own with-scopes, innermost first, which
 * every env under it that has none of its own sees (nn_env_with_owner).
 * Either may be absent: NULL, and NULL with a count of 0. */
typedef struct nn_env_scopes {
    void     *lazy_scope;   /* nn_attrset_t * or NULL */
    void    **with_scopes;  /* with-scope entries or NULL */
    uint32_t  with_count;
} nn_env_scopes_t;

#define NN_ENV_WORD_BYTES   8
#define NN_ENV_SCOPES_BYTES 24

/* C99-compatible compile-time checks of the sizes above, which the
 * bytes an env takes (nn_env_bytes) and the census's figures rest on,
 * and that a header holds any uint32_t slot count. */
typedef char nn_env_word_size_check_[(sizeof(nn_env_t) == NN_ENV_WORD_BYTES) ? 1 : -1];
typedef char nn_env_slots_offset_check_[(offsetof(nn_env_t, slots) == NN_ENV_WORD_BYTES) ? 1 : -1];
typedef char nn_env_scopes_size_check_[(sizeof(nn_env_scopes_t) == NN_ENV_SCOPES_BYTES) ? 1 : -1];
typedef char nn_env_count_width_check_[(UINTPTR_MAX >> NN_ENV_COUNT_SHIFT >= UINT32_MAX) ? 1 : -1];

/* A with-scope entry is an nn_attrset_t *, or a with-scope not yet forced:
 * the nn_thunk_t * of its expression with this bit set, which the
 * evaluator replaces by the set once it forces the thunk. */
#define NN_ENV_LAZY_WITH ((uintptr_t)1)

/* --- Lifecycle --- */

/* Initialize the global env allocator.  Must be called once before
 * any nn_env_* calls (except nn_env_destroy, nn_env_live and
 * nn_env_empty); outside the init .. destroy window every allocating
 * call returns NULL. */
void nn_env_init(void);

/* Nonzero between nn_env_init and nn_env_destroy. */
int nn_env_live(void);

/* Destroy the global env allocator, freeing all page memory.
 * All pointers returned by any nn_env_* function become invalid. */
void nn_env_destroy(void);

/* --- List items --- */

/* Allocate an array of `count` void* pointers for a list's items
 * (nn_list.h), which no env site counts.  O(1) amortized.  All entries
 * initialized to NULL.  Returns NULL if count is 0, past the allocator's
 * uint32 byte limit, or on OOM. */
void **nn_env_alloc_items(uint32_t count);

/* --- Deferred applications' cells --- */

/* Allocate an array of `count` void* pointers for the cells of a deferred
 * application (nn_thunk.h), which no env site counts.  All entries
 * initialized to NULL.  Returns NULL if count is 0, past the allocator's
 * uint32 byte limit, or on OOM. */
void **nn_env_alloc_app_cells(uint32_t count);

/* The cell arrays nn_env_alloc_app_cells has handed out since
 * nn_env_init, and their bytes. */
uint64_t nn_env_app_count(void);
uint64_t nn_env_app_bytes(void);

/* --- Env constructors (all arena-allocated) --- */

/* Return a pointer to the global empty env: no slots, no parent, no
 * scopes.  A static, valid in and outside every init .. destroy window.
 * Do not modify it. */
nn_env_t *nn_env_empty(void);

/* An env of `slot_count` NULL slots under `parent`, or under none for a
 * NULL parent, made at `site`.  Its with-scopes are its parent's.
 * Returns NULL past the allocator's uint32 byte limit, on OOM, or for a
 * parent with any of NN_ENV_SHORT_MASK set in its address, which no env
 * has. */
nn_env_t *nn_env_new(uint32_t slot_count, nn_env_t *parent, uint32_t site);

/* nn_env_new for an env with scopes: a lazy scope (or NULL) and its own
 * with-scopes (or NULL and 0, to see its parent's).  Returns NULL for a
 * count above 0 without an array. */
nn_env_t *nn_env_new_scoped(uint32_t slot_count, nn_env_t *parent,
                            void *lazy_scope,
                            void **with_scopes, uint32_t with_count,
                            uint32_t site);

/* The env a with's body runs in, made at `site`: base's slots, parent
 * and lazy scope, with `scope` prepended (innermost) to the with-scopes
 * base sees.  The slots are copied, so base's must all be filled, as they
 * are: nothing is evaluated in a frame before tieFrame (Nix.Eval.Types)
 * fills it.  Allocates a with-scopes array one longer than the one base
 * sees. */
nn_env_t *nn_env_push_with(nn_env_t *base, void *scope, uint32_t site);

/* --- Accessors --- */

/* The env's slots, which the caller fills after construction. */
void       **nn_env_slots(nn_env_t *env);
uint32_t     nn_env_slot_count(const nn_env_t *env);
/* The parent, or NULL for an env without one. */
nn_env_t    *nn_env_parent(const nn_env_t *env);
/* The scopes, or NULL for an env without them. */
const nn_env_scopes_t *nn_env_scopes(const nn_env_t *env);
/* The lazy scope, or NULL. */
void        *nn_env_lazy_scope(const nn_env_t *env);
/* The env's own with-scopes and their count, NULL and 0 when it has
 * none of its own; nn_env_with_owner finds the ones it sees. */
void       **nn_env_with_scopes(const nn_env_t *env);
uint32_t     nn_env_with_count(const nn_env_t *env);

/* The bytes the env takes from its page. */
uint32_t     nn_env_bytes(const nn_env_t *env);

/* --- Lookup --- */

/* Resolved variable lookup: walk `level` parent hops, then read
 * slots[idx].  Returns the nn_thunk_t* there, or NULL for a NULL env, a
 * negative level or index, a level past the parent chain, an index
 * outside the slots, or a slot not yet filled, which the Haskell boundary
 * raises as CInvariantViolated. */
void *nn_env_lookup_resolved(const nn_env_t *env, int level, int idx);

/* Walk parent chain to the root env (one without a parent), return its
 * lazy scope.  Returns NULL if the root has no lazy scope. */
void *nn_env_root_scope(const nn_env_t *env);

/* The env whose with-scopes `env` sees: the first on its parent chain,
 * itself included, with with-scopes of its own, or NULL for none.  An env
 * made under a parent sees what its parent sees until a with or a trimmed
 * closure gives it its own. */
nn_env_t *nn_env_with_owner(nn_env_t *env);

/* --- With-scopes array allocation --- */

/* Allocate an arena array of `count` void* pointers for with-scopes of
 * an env at `site`.  All entries initialized to NULL.  Returns NULL if
 * count is 0, if the array size would exceed the allocator's uint32 byte
 * limit, or on OOM - the caller must check. */
void **nn_env_alloc_with_scopes(uint32_t count, uint32_t site);

/* --- Diagnostics --- */

/* Total bytes allocated across all pages (for memory tracking). */
uint64_t nn_env_bytes_allocated(void);

/* --- Census (nn_stats.h) --- */

/* The pages, oldest first: for each of the first `max`, its first byte
 * and the bytes it has handed out go to `data` and `used`.  Every
 * allocation starts at an NN_ENV_ALIGNMENT boundary from its page's first
 * byte.  Returns the number of pages, which may exceed `max`; 0 outside
 * the init .. destroy window. */
uint32_t nn_env_pages(char **data, uint32_t *used, uint32_t max);

/* The alignment nn_env_pages promises. */
#define NN_ENV_ALIGNMENT 8u

/* Bytes the pages hold from the allocator, headers and unused tails
 * included. */
uint64_t nn_env_reserved_bytes(void);

#endif /* NN_ENV_H */
