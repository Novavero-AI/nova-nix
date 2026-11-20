/*
 * nn_lambda.h - Lambda closure structs for nova-nix.
 *
 * Stores lambda closures entirely in C: captured environment (nn_env_t*),
 * body bytecode index, and formal parameter specification.  A thunk
 * holds one under NN_VALUE_LAMBDA, so a closure takes nothing on the GHC
 * heap.
 *
 * Formals are stored as a type tag (Name/Set/NamedSet) plus an array
 * of (symbol, has_default, default_bc_idx) entries for set patterns.
 * Names are interned symbols, compared as integers.
 *
 * Lambda structs and their formal entry arrays are malloc'd, and each
 * lambda is tracked for bulk cleanup via nn_lambda_free_all().
 *
 * Lifecycle: constructed during evaluation, freed via
 * nn_lambda_free_all() at arena teardown.  Not thread-safe.
 */

#ifndef NN_LAMBDA_H
#define NN_LAMBDA_H

#include "nn_tracker.h"

#include <stdint.h>

/* Forward declarations */
struct nn_env;

/* --- Formals type tags --- */

#define NN_FORMALS_NAME      0   /* x: body */
#define NN_FORMALS_SET       1   /* { a, b, ... }: body */
#define NN_FORMALS_NAMED_SET 2   /* x@{ a, b, ... }: body */

/* --- Types --- */

/* A single formal entry: name + the kind of its default + the default's
 * bytecode index.  The kinds are the Haskell side's
 * (Nix.Eval.CBytecode.formalNoDefault and its siblings): 0 = required,
 * 1 = has a default, 2 = has a default that is a variable naming a slot
 * of the call's frame, whose cell the formal's slot takes. */
typedef struct nn_formal_entry {
    uint32_t name_sym;       /* interned symbol ID */
    uint32_t has_default;    /* the default's kind */
    uint32_t default_bc_idx; /* bytecode index of default (valid if has_default) */
} nn_formal_entry_t;

/* A lambda's shape: its formals type, its ellipsis flag and its formal
 * count in one word, the type in the top two bits, the flag in the bit
 * below them and the count in the rest.  The count is input-reachable
 * (it scales with the source formal list); nn_lambda_new refuses every
 * count whose entries would pass the UINT32_MAX byte limit, which is
 * every count above UINT32_MAX / 12, well below NN_LAMBDA_MAX_FORMALS,
 * so no count reaches the flag bits (nn_lambda.c checks this at compile
 * time). */
#define NN_LAMBDA_COUNT_BITS  29
#define NN_LAMBDA_MAX_FORMALS ((UINT32_C(1) << NN_LAMBDA_COUNT_BITS) - 1)

/* Lambda closure: env + body + formals specification.  Two pointers and
 * four words with no padding: a closure is allocated per lambda value,
 * so its size is held at NN_LAMBDA_BYTES, the size on the 64-bit
 * targets nn_thunk.h requires.  lambda_name_sym is the name a set
 * pattern's call errors give the function, the attribute it is bound to,
 * or NN_SYMBOL_INVALID for an anonymous one; a Name lambda holds
 * NN_SYMBOL_INVALID there. */
typedef struct nn_lambda {
    struct nn_env         *env;          /* captured closure environment */
    nn_formal_entry_t     *entries;      /* formal entries (malloc'd, or NULL) */
    uint32_t               body_bc_idx;  /* bytecode index of lambda body */
    uint32_t               name_sym;     /* symbol for Name/NamedSet binding */
    uint32_t               lambda_name_sym; /* the function's own name */
    uint32_t               shape;        /* nn_lambda_shape() */
} nn_lambda_t;

#define NN_LAMBDA_BYTES 32

/* C99-compatible compile-time check of the size above. */
typedef char nn_lambda_size_check_[(sizeof(nn_lambda_t) == NN_LAMBDA_BYTES) ? 1 : -1];

/* --- Shape --- */

/* Pack a shape.  The type is taken mod 4, any nonzero flag as 1 and the
 * count mod 2^NN_LAMBDA_COUNT_BITS; nn_lambda_new takes only a type and
 * a count that fit. */
uint32_t nn_lambda_shape(uint8_t formals_type, uint8_t allow_extra,
                         uint32_t formal_count);

/* Unpack a shape's fields. */
uint8_t  nn_lambda_shape_formals_type(uint32_t shape);
uint8_t  nn_lambda_shape_allow_extra(uint32_t shape);
uint32_t nn_lambda_shape_formal_count(uint32_t shape);

/* --- Lifecycle --- */

/* Allocate a new lambda closure.  The entries array is allocated
 * internally (formal_count entries).  Fill entries via
 * nn_lambda_set_entry() after construction.  formals_type is one of the
 * NN_FORMALS_* tags, and any other aborts (NN_ASSERT).
 * Returns NULL on allocation failure, or when the entries would pass
 * the UINT32_MAX byte limit - the caller must check. */
nn_lambda_t *nn_lambda_new(struct nn_env *env, uint32_t body_bc_idx,
                           uint8_t formals_type, uint32_t name_sym,
                           uint32_t lambda_name_sym,
                           uint8_t allow_extra, uint32_t formal_count);

/* Set a formal entry at the given index (for construction).
 * idx must be < formal_count: an index past it aborts (NN_ASSERT). */
void nn_lambda_set_entry(nn_lambda_t *lam, uint32_t idx,
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
uint32_t       nn_lambda_lambda_name_sym(const nn_lambda_t *lam);
uint8_t        nn_lambda_allow_extra(const nn_lambda_t *lam);
uint32_t       nn_lambda_formal_count(const nn_lambda_t *lam);

/* Read formal entry fields by index, which must be < formal_count: an
 * index past it aborts (NN_ASSERT). */
uint32_t nn_lambda_entry_name(const nn_lambda_t *lam, uint32_t idx);
uint32_t nn_lambda_entry_has_default(const nn_lambda_t *lam, uint32_t idx);
uint32_t nn_lambda_entry_default(const nn_lambda_t *lam, uint32_t idx);

/* --- Census (nn_stats.h) --- */

/* Every lambda allocated and not yet freed: the module's own tracker,
 * whose array moves when the next lambda is created and is freed by
 * nn_lambda_free_all. */
const nn_tracker_t *nn_lambda_tracked(void);

/* Bytes a lambda holds from the allocator: its struct and its formal
 * entries. */
uint64_t nn_lambda_bytes(const nn_lambda_t *lam);

#endif /* NN_LAMBDA_H */
