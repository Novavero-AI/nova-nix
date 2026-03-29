/*
 * nn_env.h — C-native evaluation environments.
 *
 * Nix evaluation creates millions of environments, each with a
 * variable-size array of slot pointers, optional lazy scope (attr set),
 * optional parent pointer, and a with-scopes chain.
 *
 * All nn_env_t structs and their backing arrays are arena-allocated
 * from a page-based bump allocator.  Nothing is individually freed —
 * nn_env_destroy() tears down everything at eval end.
 *
 * Lifecycle: nn_env_init() before evaluation, nn_env_destroy() after.
 * Not thread-safe — single-threaded evaluation only.
 */

#ifndef NN_ENV_H
#define NN_ENV_H

#include <stdint.h>

/* --- Env struct --- */

/* 48 bytes on 64-bit (with padding after slot_count and with_count).
 * Could be reduced to 40 bytes by reordering fields, but the current
 * layout groups related fields logically and matches access patterns. */
typedef struct nn_env {
    void         **slots;        /* nn_thunk_t* array (or NULL for 0 slots) */
    uint32_t       slot_count;   /* 4 bytes + 4 pad to align lazy_scope */
    void          *lazy_scope;   /* nn_attrset_t* or NULL */
    struct nn_env *parent;       /* nn_env_t* or NULL */
    void         **with_scopes;  /* array of nn_attrset_t* (or NULL) */
    uint16_t       with_count;   /* 2 bytes + 6 pad to struct alignment */
} nn_env_t;

/* --- Lifecycle --- */

/* Initialize the global env allocator.  Must be called once before
 * any nn_env_* calls (except nn_env_destroy). */
void nn_env_init(void);

/* Destroy the global env allocator, freeing all page memory.
 * All pointers returned by any nn_env_* function become invalid. */
void nn_env_destroy(void);

/* --- Slot allocation (unchanged from original API) --- */

/* Allocate an array of `count` void* pointers.  O(1) amortized.
 * All slots initialized to NULL.  Returns NULL if count is 0. */
void **nn_env_alloc_slots(uint32_t count);

/* --- Env constructors (all arena-allocated) --- */

/* Return a pointer to the global empty env (all fields zero/NULL).
 * Valid until nn_env_destroy().  Do not modify the returned struct. */
nn_env_t *nn_env_empty(void);

/* Full constructor: set all fields explicitly. */
nn_env_t *nn_env_new(void **slots, uint32_t slot_count,
                     void *lazy_scope, nn_env_t *parent,
                     void **with_scopes, uint16_t with_count);

/* Child env with positional slots, inheriting parent's with-scopes.
 * lazy_scope = NULL. */
nn_env_t *nn_env_from_slots(void **slots, uint32_t slot_count,
                            nn_env_t *parent);

/* Copy base env with a new with-scope prepended (innermost position).
 * Allocates a new with-scopes array of size (base->with_count + 1). */
nn_env_t *nn_env_push_with(nn_env_t *base, void *scope);

/* Minimal env: slots only, no parent, no with-scopes, no lazy scope. */
nn_env_t *nn_env_new_minimal(void **slots, uint32_t slot_count);

/* --- Accessors --- */

void       **nn_env_slots(const nn_env_t *env);
uint32_t     nn_env_slot_count(const nn_env_t *env);
void        *nn_env_lazy_scope(const nn_env_t *env);
nn_env_t    *nn_env_parent(const nn_env_t *env);
void       **nn_env_with_scopes(const nn_env_t *env);
uint16_t     nn_env_with_count(const nn_env_t *env);

/* --- Lookup --- */

/* Resolved variable lookup: walk `level` parent hops, then read
 * slots[idx].  Returns the nn_thunk_t* at that position.
 * Caller must ensure level/idx are in bounds (guaranteed by
 * the resolution pass). */
void *nn_env_lookup_resolved(const nn_env_t *env, int level, int idx);

/* Walk parent chain to the root env (parent == NULL), return its
 * lazy_scope.  Returns NULL if the root has no lazy scope. */
void *nn_env_root_scope(const nn_env_t *env);

/* --- With-scopes array allocation --- */

/* Allocate an arena array of `count` void* pointers for with-scopes.
 * All entries initialized to NULL.  Returns NULL if count is 0. */
void **nn_env_alloc_with_scopes(uint16_t count);

/* --- Diagnostics --- */

/* Total bytes allocated across all pages (for memory tracking). */
uint64_t nn_env_bytes_allocated(void);

#endif /* NN_ENV_H */
