/*
 * nn_thunk.h — Arena-allocated thunk memoization cells for nova-nix.
 *
 * Thunks are the core memoization mechanism in Nix evaluation.  Each
 * thunk starts as PENDING (holding a reference to an unevaluated
 * expression + environment) and transitions to COMPUTED (holding the
 * result value) on first force.  A BLACKHOLE state detects infinite
 * recursion during evaluation.
 *
 * Replaces Haskell's IORef ThunkCell (~56 bytes on GHC heap, all GC-
 * traced) with a 16-byte C struct in an arena (invisible to GHC GC).
 * For 8M thunks: ~450 MB less GHC heap pressure.
 *
 * Arena: chunked bump allocator.  Thunks are never individually freed —
 * the entire arena is destroyed at evaluation end.  Pointers into the
 * arena remain valid until nn_thunk_destroy().
 *
 * Payloads are opaque void* — Haskell passes StablePtr values.  The C
 * side never dereferences them.  Haskell is responsible for freeing
 * StablePtrs before arena destruction (via nn_thunk_count/nn_thunk_get
 * iteration).
 *
 * Lifecycle: nn_thunk_init() before evaluation, nn_thunk_destroy()
 * after.  Not thread-safe — single-threaded evaluation only.
 */

#ifndef NN_THUNK_H
#define NN_THUNK_H

#include <stdint.h>

/* --- Thunk states --- */

#define NN_THUNK_PENDING   0
#define NN_THUNK_COMPUTED  1
#define NN_THUNK_BLACKHOLE 2

/* --- Value tags (valid when state == COMPUTED) --- */

#define NN_VALUE_INT     0   /* payload = (void*)(intptr_t)int64_value */
#define NN_VALUE_FLOAT   1   /* payload = memcpy'd double */
#define NN_VALUE_BOOL    2   /* payload = (void*)(intptr_t)(0 or 1) */
#define NN_VALUE_NULL    3   /* payload = NULL */
#define NN_VALUE_STR     4   /* payload = (void*)(intptr_t)nn_symbol_t (no context) */
#define NN_VALUE_PATH    5   /* payload = (void*)(intptr_t)nn_symbol_t */
#define NN_VALUE_LIST    6   /* payload = nn_list_t* */
#define NN_VALUE_ATTRS   7   /* payload = nn_attrset_t* */
#define NN_VALUE_PTR     255 /* payload = StablePtr NixValue (complex types) */

/* --- Types --- */

/* A thunk: mutable cell holding either a pending expression or a
 * computed value.  16 bytes: state(1) + val_tag(1) + padding(6) +
 * payload(8).  val_tag is valid only when state == COMPUTED.
 *
 * PENDING:   payload = StablePtr (Expr, Env)
 * COMPUTED:  val_tag selects interpretation of payload:
 *            INT/BOOL: (intptr_t) cast of scalar
 *            FLOAT: memcpy'd double
 *            NULL: ignored
 *            PTR: StablePtr to Haskell NixValue (complex types)
 * BLACKHOLE: payload = stale (do not dereference) */
typedef struct nn_thunk {
    uint8_t  state;
    uint8_t  val_tag;
    void    *payload;
} nn_thunk_t;

/* --- Lifecycle --- */

/* Initialize the global thunk arena.  initial_capacity is the number
 * of thunks to pre-allocate per block (0 uses default: 65536 = 1 MB).
 * Must be called once before any nn_thunk_new() calls. */
void nn_thunk_init(uint32_t initial_capacity);

/* Destroy the global thunk arena, freeing all block memory.
 * Does NOT free payload StablePtrs — caller must iterate and free
 * them first (via nn_thunk_count/nn_thunk_get).
 * All nn_thunk_t pointers become invalid after this call. */
void nn_thunk_destroy(void);

/* --- Allocation --- */

/* Allocate a new PENDING thunk from the arena.  O(1) amortized.
 * pending_data is an opaque pointer (StablePtr to Haskell (Expr, Env)).
 * Returns a pointer into arena memory, valid until nn_thunk_destroy(). */
nn_thunk_t *nn_thunk_new(void *pending_data);

/* Allocate a new pre-COMPUTED thunk from the arena (StablePtr payload).
 * value is an opaque pointer (StablePtr to Haskell NixValue).
 * val_tag is set to NN_VALUE_PTR. */
nn_thunk_t *nn_thunk_new_computed(void *value);

/* Allocate pre-COMPUTED thunks with inline scalar values (no StablePtr). */
nn_thunk_t *nn_thunk_new_computed_int(int64_t value);
nn_thunk_t *nn_thunk_new_computed_float(double value);
nn_thunk_t *nn_thunk_new_computed_bool(uint8_t value);
nn_thunk_t *nn_thunk_new_computed_null(void);

/* Allocate pre-COMPUTED thunks with C-native complex values (no StablePtr). */
nn_thunk_t *nn_thunk_new_computed_str(uint32_t symbol);
nn_thunk_t *nn_thunk_new_computed_path(uint32_t symbol);
nn_thunk_t *nn_thunk_new_computed_list(void *list);
nn_thunk_t *nn_thunk_new_computed_attrs(void *attrset);

/* --- State queries --- */

/* Read the current state (NN_THUNK_PENDING/COMPUTED/BLACKHOLE). */
uint8_t nn_thunk_state(const nn_thunk_t *thunk);

/* Read the payload pointer.  Meaning depends on state + val_tag. */
void *nn_thunk_payload(const nn_thunk_t *thunk);

/* Read the value tag (valid when state == COMPUTED). */
uint8_t nn_thunk_value_tag(const nn_thunk_t *thunk);

/* Read inline scalar values from a COMPUTED thunk. */
int64_t nn_thunk_get_int(const nn_thunk_t *thunk);
double  nn_thunk_get_float(const nn_thunk_t *thunk);
uint8_t nn_thunk_get_bool(const nn_thunk_t *thunk);

/* Read C-native complex values from a COMPUTED thunk. */
uint32_t nn_thunk_get_str(const nn_thunk_t *thunk);
uint32_t nn_thunk_get_path(const nn_thunk_t *thunk);
void    *nn_thunk_get_list(const nn_thunk_t *thunk);
void    *nn_thunk_get_attrs(const nn_thunk_t *thunk);

/* --- State transitions --- */

/* Transition PENDING -> BLACKHOLE (thunk is being evaluated).
 * Payload remains unchanged (caller reads it before this call).
 * Returns 1 on success, 0 if thunk is not PENDING. */
int nn_thunk_mark_blackhole(nn_thunk_t *thunk);

/* Set a non-COMPUTED thunk to COMPUTED with a StablePtr value (NN_VALUE_PTR).
 * Accepts PENDING or BLACKHOLE state (skips blackhole for direct memoization).
 * Returns the old payload (pending StablePtr for caller to free).
 * Returns NULL if thunk is already COMPUTED. */
void *nn_thunk_set_computed(nn_thunk_t *thunk, void *value);

/* Set a non-COMPUTED thunk to COMPUTED with inline scalar values.
 * Returns the old payload (pending StablePtr for caller to free).
 * Returns NULL if thunk is already COMPUTED. */
void *nn_thunk_set_computed_int(nn_thunk_t *thunk, int64_t value);
void *nn_thunk_set_computed_float(nn_thunk_t *thunk, double value);
void *nn_thunk_set_computed_bool(nn_thunk_t *thunk, uint8_t value);
void *nn_thunk_set_computed_null(nn_thunk_t *thunk);

/* Set a non-COMPUTED thunk to COMPUTED with C-native complex values.
 * Returns the old payload (pending StablePtr for caller to free).
 * Returns NULL if thunk is already COMPUTED. */
void *nn_thunk_set_computed_str(nn_thunk_t *thunk, uint32_t symbol);
void *nn_thunk_set_computed_path(nn_thunk_t *thunk, uint32_t symbol);
void *nn_thunk_set_computed_list(nn_thunk_t *thunk, void *list);
void *nn_thunk_set_computed_attrs(nn_thunk_t *thunk, void *attrset);


/* --- Arena diagnostics / cleanup iteration --- */

/* Total thunks allocated across all blocks. */
uint32_t nn_thunk_count(void);

/* Get a thunk by global index (0-based, across all blocks).
 * Used for StablePtr cleanup iteration before nn_thunk_destroy().
 * Index must be < nn_thunk_count(). */
nn_thunk_t *nn_thunk_get(uint32_t index);

#endif /* NN_THUNK_H */
