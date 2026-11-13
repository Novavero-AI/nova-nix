/*
 * nn_thunk.h - Arena-allocated thunk memoization cells for nova-nix.
 *
 * Thunks are the core memoization mechanism in Nix evaluation.  Each
 * thunk starts as PENDING (holding a bytecode index + C environment
 * pointer) and transitions to COMPUTED (holding the result value) on
 * first force.  A BLACKHOLE state detects infinite recursion.
 *
 * Each thunk is a 16-byte C struct in an arena, invisible to GHC's GC.
 *
 * Arena: chunked bump allocator.  Thunks are never individually freed -
 * the entire arena is destroyed at evaluation end.  Pointers into the
 * arena remain valid until nn_thunk_destroy().
 *
 * PENDING payloads are the nn_env_t * the code runs in, owned by the env
 * arena, so a pending thunk holds no reference into the GHC heap.  A
 * recursive frame (rec attrs, let, a formals set) is allocated with its
 * slot array before the thunks that point at it, and its slots are
 * filled after, as upstream allocates env2 before filling its values.
 * COMPUTED payloads are either inline scalars (int/float/bool/null via
 * val_tag 0-3), C-native pointers (str/path/list/attrs/ctxstr/lambda
 * via val_tag 4-9), or StablePtrs (for VBuiltin/VDrv via tag 255).
 *
 * Lifecycle: nn_thunk_init() before evaluation, nn_thunk_destroy()
 * after.  Not thread-safe - single-threaded evaluation only.
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
#define NN_VALUE_CTXSTR  8   /* payload = nn_ctxstr_t* (string with context) */
#define NN_VALUE_LAMBDA  9   /* payload = nn_lambda_t* (lambda closure) */
#define NN_VALUE_PTR     255 /* payload = StablePtr NixValue (complex types) */

/* --- Types --- */

/* A thunk: mutable cell holding either a pending expression or a
 * computed value.  16 bytes: state(1) + val_tag(1) + _pad(2) +
 * bc_idx(4) + payload(8).  val_tag is valid only when state == COMPUTED.
 *
 * PENDING:   bc_idx = bytecode instruction index
 *            payload = nn_env_t * the code runs in
 * COMPUTED:  val_tag selects interpretation of payload:
 *            INT/BOOL: (intptr_t) cast of scalar
 *            FLOAT: memcpy'd double
 *            NULL: ignored
 *            PTR: StablePtr to Haskell NixValue (complex types)
 * BLACKHOLE: bc_idx/payload = stale from PENDING (do not dereference) */
typedef struct nn_thunk {
    uint8_t   state;
    uint8_t   val_tag;
    uint16_t  _pad;
    uint32_t  bc_idx;    /* PENDING: bytecode index; otherwise stale (never read) */
    void     *payload;
} nn_thunk_t;

/* Nix integers are int64_t, stored inline via (intptr_t) cast into payload.
 * This requires pointers to be at least 64 bits.  32-bit targets would
 * silently truncate large integers.  C99-compatible compile-time check: */
typedef char nn_thunk_ptr_size_check_[(sizeof(void *) >= sizeof(int64_t)) ? 1 : -1];

/* --- Lifecycle --- */

/* Initialize the global thunk arena.  initial_capacity is the number
 * of thunks to pre-allocate per block (0 uses default: 65536 = 1 MB).
 * Must be called once before any thunk allocation (nn_thunk_new_bc or
 * the computed-value constructors); outside the init .. destroy window
 * every allocator returns NULL. */
void nn_thunk_init(uint32_t initial_capacity);

/* Nonzero between nn_thunk_init and nn_thunk_destroy. */
int nn_thunk_live(void);

/* Destroy the global thunk arena, freeing all block memory.
 * Does NOT free NN_VALUE_PTR StablePtrs - caller collects and frees
 * them first (via nn_thunk_collect_stableptrs).
 * All nn_thunk_t pointers become invalid after this call. */
void nn_thunk_destroy(void);

/* --- Allocation --- */

/* Allocate a new PENDING thunk with a bytecode index + env payload.
 * Returns a pointer into arena memory, valid until nn_thunk_destroy().
 * bc_idx is the root instruction index in the bytecode store.
 * env is the nn_env_t * the code runs in.  It must already exist, but
 * its slots need not be filled yet: nothing reads them until the thunk
 * is forced. */
nn_thunk_t *nn_thunk_new_bc(uint32_t bc_idx, void *env);

/* Read the bytecode index from a PENDING thunk. */
uint32_t nn_thunk_get_bc_idx(const nn_thunk_t *thunk);

/* Allocate a new pre-COMPUTED thunk from the arena (StablePtr payload).
 * value is an opaque pointer (StablePtr to Haskell NixValue).
 * val_tag is set to NN_VALUE_PTR. */
nn_thunk_t *nn_thunk_new_computed(void *value);

/* Allocate pre-COMPUTED thunks with inline scalar values (no StablePtr). */
nn_thunk_t *nn_thunk_new_computed_int(int64_t value);
nn_thunk_t *nn_thunk_new_computed_float(double value);

/* Allocate pre-COMPUTED thunks with C-native complex values (no StablePtr). */
nn_thunk_t *nn_thunk_new_computed_str(uint32_t symbol);
nn_thunk_t *nn_thunk_new_computed_path(uint32_t symbol);
nn_thunk_t *nn_thunk_new_computed_list(void *list);
nn_thunk_t *nn_thunk_new_computed_attrs(void *attrset);
nn_thunk_t *nn_thunk_new_computed_ctxstr(void *ctxstr);
nn_thunk_t *nn_thunk_new_computed_lambda(void *lambda);

/* --- Shared constants --- */

/* The COMPUTED null, true and false cells, as upstream shares one vNull,
 * vTrue and vFalse per EvalState.  They are file-scope statics, not
 * arena memory, so each has one address in every init .. destroy window
 * and outside all of them: a constant built from one before
 * nn_thunk_init, or kept across nn_thunk_destroy, still reads correctly.
 * Every state transition refuses a COMPUTED thunk, so nothing writes
 * them.  nn_thunk_bool answers the true cell for any nonzero value. */
nn_thunk_t *nn_thunk_null(void);
nn_thunk_t *nn_thunk_bool(uint8_t value);

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
void    *nn_thunk_get_ctxstr(const nn_thunk_t *thunk);
void    *nn_thunk_get_lambda(const nn_thunk_t *thunk);

/* --- State transitions --- */

/* Transition PENDING -> BLACKHOLE (thunk is being evaluated).
 * Payload remains unchanged (caller reads it before this call).
 * Returns 1 on success, 0 if thunk is not PENDING. */
int nn_thunk_mark_blackhole(nn_thunk_t *thunk);

/* Transition BLACKHOLE -> PENDING (a force threw and was caught upstream).
 * Payload + bc_idx are preserved, so the thunk is re-forceable; matches C++
 * Nix restoring a thunk after a caught exception during force.
 * Returns 1 on success, 0 if thunk is not BLACKHOLE. */
int nn_thunk_mark_pending(nn_thunk_t *thunk);

/* Set a non-COMPUTED thunk to COMPUTED, the setter naming the value
 * kind: a StablePtr NixValue (NN_VALUE_PTR), an inline scalar, or a
 * C-native complex value.  Accepts PENDING or BLACKHOLE state (skips
 * blackhole for direct memoization).  The pending payload is overwritten:
 * it is an env pointer the env arena owns, so there is nothing to hand
 * back or free.  Returns 1, or 0 with the thunk unchanged if it is
 * already COMPUTED. */
int nn_thunk_set_computed(nn_thunk_t *thunk, void *value);
int nn_thunk_set_computed_int(nn_thunk_t *thunk, int64_t value);
int nn_thunk_set_computed_float(nn_thunk_t *thunk, double value);
int nn_thunk_set_computed_bool(nn_thunk_t *thunk, uint8_t value);
int nn_thunk_set_computed_null(nn_thunk_t *thunk);
int nn_thunk_set_computed_str(nn_thunk_t *thunk, uint32_t symbol);
int nn_thunk_set_computed_path(nn_thunk_t *thunk, uint32_t symbol);
int nn_thunk_set_computed_list(nn_thunk_t *thunk, void *list);
int nn_thunk_set_computed_attrs(nn_thunk_t *thunk, void *attrset);
int nn_thunk_set_computed_ctxstr(nn_thunk_t *thunk, void *ctxstr);
int nn_thunk_set_computed_lambda(nn_thunk_t *thunk, void *lambda);

/* --- Arena diagnostics / cleanup iteration --- */

/* Total thunks allocated across all blocks. */
uint32_t nn_thunk_count(void);

/* Get a thunk by global index (0-based, across all blocks, in allocation
 * order), or NULL when index >= nn_thunk_count().  O(blocks) per call. */
nn_thunk_t *nn_thunk_get(uint32_t index);

/* Single-pass (O(total)) collection of Haskell StablePtr payloads across the
 * whole arena.  Walks the block list directly - nn_thunk_get is O(blocks) per
 * call, so index-based iteration would be O(total * blocks).  The only
 * StablePtr payload is a COMPUTED thunk's with val_tag NN_VALUE_PTR.
 * nn_thunk_collect_stableptrs writes up to max_count pointers into `output`
 * and returns the count written.  These back the nn_arena_* teardown helpers. */
uint32_t nn_thunk_count_stableptrs(void);
uint32_t nn_thunk_collect_stableptrs(void **output, uint32_t max_count);

#endif /* NN_THUNK_H */
