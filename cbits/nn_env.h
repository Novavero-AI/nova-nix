/*
 * nn_env.h - C-native evaluation environments.
 *
 * Nix evaluation creates millions of environments.  An env is a header
 * word and its slots, followed by the fields its kind says it has:
 *
 *   slot_count, kind     NN_ENV_HEADER_BYTES
 *   slots[slot_count]    one nn_thunk_t * each
 *   parent               an nn_env_t *, when kind has NN_ENV_PARENT
 *   scopes               an nn_env_scopes_t, when kind has NN_ENV_SCOPES
 *
 * A call's env, the most common, is the header, its parent and its one
 * slot.  A trimmed closure's env or a builtin's deferred application has
 * no parent and pays for none.  Only a root env (the builtins, or
 * scopedImport's names over them), a frame whose names are not all
 * static, a with and a trimmed closure under a with have scopes.
 * Everything an env points to is found from the env alone: the header
 * gives the slot count and which fields follow them.
 *
 * The slots follow the header as a C99 flexible array member, so the
 * compiler places them at a pointer-aligned offset, which nn_env_t's size
 * check below pins at NN_ENV_HEADER_BYTES.  Every env starts at an
 * NN_ENV_ALIGNMENT boundary of its page, and every field after the slots
 * starts a whole number of pointers past them, so every slot, parent and
 * scopes field is pointer-aligned.
 *
 * Envs, the with-scope arrays and the list items arrays (nn_list.h) are
 * allocated from a page-based bump allocator.  Nothing is individually
 * freed - nn_env_destroy() tears down everything at eval end.
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
    uint32_t slot_count;   /* entries in slots */
    uint32_t kind;         /* NN_ENV_PARENT and NN_ENV_SCOPES, or neither */
    void    *slots[];      /* nn_thunk_t *, NULL until the frame is filled */
} nn_env_t;

/* The fields an env's kind says follow its slots, in this order. */
#define NN_ENV_PARENT 1u   /* the enclosing env */
#define NN_ENV_SCOPES 2u   /* an nn_env_scopes_t */

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

#define NN_ENV_HEADER_BYTES 8
#define NN_ENV_SCOPES_BYTES 24

/* C99-compatible compile-time checks of the sizes above, which the
 * bytes an env takes (nn_env_bytes) and the census's figures rest on. */
typedef char nn_env_header_size_check_[(sizeof(nn_env_t) == NN_ENV_HEADER_BYTES) ? 1 : -1];
typedef char nn_env_slots_offset_check_[(offsetof(nn_env_t, slots) == NN_ENV_HEADER_BYTES) ? 1 : -1];
typedef char nn_env_scopes_size_check_[(sizeof(nn_env_scopes_t) == NN_ENV_SCOPES_BYTES) ? 1 : -1];

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

/* --- Env constructors (all arena-allocated) --- */

/* Return a pointer to the global empty env: no slots, no parent, no
 * scopes.  Valid until nn_env_destroy().  Do not modify it. */
nn_env_t *nn_env_empty(void);

/* An env of `slot_count` NULL slots under `parent`, or under none for a
 * NULL parent, made at `site`.  Its with-scopes are its parent's.
 * Returns NULL past the allocator's uint32 byte limit or on OOM. */
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
 * slots[idx].  Returns the nn_thunk_t* at that position.
 * Caller must ensure level/idx are in bounds (guaranteed by
 * the resolution pass). */
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
