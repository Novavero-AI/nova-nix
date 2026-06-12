/*
 * nn_lambda.h - Lambda closure structs for nova-nix.
 *
 * Stores lambda closures entirely in C: captured environment (nn_env_t*),
 * body bytecode index, and formal parameter specification.  Replaces
 * StablePtr NixValue for VLambda on the GHC heap (~25% of remaining
 * heap after M6) with a C-native tag 9 in the thunk system.
 *
 * Formals are stored as a type tag (Name/Set/NamedSet) plus an array
 * of (symbol, has_default, default_bc_idx) entries for set patterns.
 * Interned symbols avoid any string allocation - pointer equality
 * for name comparison.
 *
 * Lambda structs are malloc'd and tracked for bulk cleanup via
 * nn_lambda_free_all().  Formal entry arrays use malloc (small, typ.
 * 1-20 entries).  All freed at evaluation end.
 *
 * Lifecycle: constructed during evaluation, freed via
 * nn_lambda_free_all() at arena teardown.  Not thread-safe.
 */

#ifndef NN_LAMBDA_H
#define NN_LAMBDA_H

#include <stdint.h>

/* Forward declarations */
struct nn_env;

/* --- Formals type tags --- */

#define NN_FORMALS_NAME      0   /* x: body */
#define NN_FORMALS_SET       1   /* { a, b, ... }: body */
#define NN_FORMALS_NAMED_SET 2   /* x@{ a, b, ... }: body */

/* --- Types --- */

/* A single formal entry: name + optional default bytecode index. */
typedef struct nn_formal_entry {
    uint32_t name_sym;       /* interned symbol ID */
    uint32_t has_default;    /* 0 = required, 1 = has default */
    uint32_t default_bc_idx; /* bytecode index of default (valid if has_default) */
} nn_formal_entry_t;

/* Lambda closure: env + body + formals specification.
 * Packed for minimal size (pointers first, then uint32s, then small fields).
 * 28 bytes on 64-bit (no padding). */
typedef struct nn_lambda {
    struct nn_env         *env;          /* captured closure environment */
    nn_formal_entry_t     *entries;      /* formal entries (malloc'd, or NULL) */
    uint32_t               body_bc_idx;  /* bytecode index of lambda body */
    uint32_t               name_sym;     /* symbol for Name/NamedSet binding */
    uint16_t               formal_count; /* number of formal entries */
    uint8_t                formals_type; /* NN_FORMALS_NAME/SET/NAMED_SET */
    uint8_t                allow_extra;  /* 1 if ellipsis (...) present */
} nn_lambda_t;

/* --- Lifecycle --- */

/* Allocate a new lambda closure.  The entries array is allocated
 * internally (formal_count entries).  Fill entries via
 * nn_lambda_set_entry() after construction.
 * Returns NULL on allocation failure. */
nn_lambda_t *nn_lambda_new(struct nn_env *env, uint32_t body_bc_idx,
                           uint8_t formals_type, uint32_t name_sym,
                           uint8_t allow_extra, uint16_t formal_count);

/* Set a formal entry at the given index (for construction).
 * Index must be < formal_count. */
void nn_lambda_set_entry(nn_lambda_t *lam, uint16_t idx,
                         uint32_t name_sym, uint32_t has_default,
                         uint32_t default_bc_idx);

/* Free all tracked lambda structs (arena-style cleanup).
 * Also frees all entries arrays.
 * Call once at evaluation end. */
void nn_lambda_free_all(void);

/* --- Accessors --- */

struct nn_env *nn_lambda_env(const nn_lambda_t *lam);
uint32_t       nn_lambda_body(const nn_lambda_t *lam);
uint8_t        nn_lambda_formals_type(const nn_lambda_t *lam);
uint32_t       nn_lambda_name_sym(const nn_lambda_t *lam);
uint8_t        nn_lambda_allow_extra(const nn_lambda_t *lam);
uint16_t       nn_lambda_formal_count(const nn_lambda_t *lam);

/* Read formal entry fields by index (must be < formal_count). */
uint32_t nn_lambda_entry_name(const nn_lambda_t *lam, uint16_t idx);
uint32_t nn_lambda_entry_has_default(const nn_lambda_t *lam, uint16_t idx);
uint32_t nn_lambda_entry_default(const nn_lambda_t *lam, uint16_t idx);

#endif /* NN_LAMBDA_H */
