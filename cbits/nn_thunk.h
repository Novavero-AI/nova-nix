/*
 * nn_thunk.h - Arena-allocated thunk memoization cells for nova-nix.
 *
 * Thunks are the core memoization mechanism in Nix evaluation.  Each
 * thunk starts as PENDING (holding a bytecode index and the C environment
 * it runs in, or a deferred application's cells) and transitions to
 * COMPUTED (holding the result value) on first force.  A BLACKHOLE state
 * detects infinite recursion.
 *
 * Each thunk is a 16-byte C struct in an arena, invisible to GHC's GC.
 *
 * Arena: chunked bump allocator.  Thunks are never individually freed -
 * the entire arena is destroyed at evaluation end.  Pointers into the
 * arena remain valid until nn_thunk_destroy().
 *
 * A PENDING payload is the nn_env_t * the code runs in, or the cells of
 * a deferred application, a builtin's per-element application of a
 * function (map, mapAttrs and the like) as upstream's app value is a
 * function and its argument (value.hh at 2.24.9); either lives in the env
 * pages, so a pending thunk holds no reference into the GHC heap.  A
 * recursive frame (rec attrs, let, a formals set) is allocated with its
 * slot array before the thunks that point at it, and its slots are
 * filled after, as upstream allocates env2 before filling its values.
 * COMPUTED payloads are inline scalars (int/float/bool/null via val_tag
 * 0-3), C objects (str/path/list/attrs/ctxstr/lambda via val_tag 4-9, a
 * builtin's partial application via 11), or a builtin's index (10), so
 * every value lives in C and holds nothing in GHC's heap.
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

/* --- Pending forms (valid when state is PENDING or BLACKHOLE) --- */

/* Code: the instruction at bc_idx, run in the env at payload. */
#define NN_PENDING_CODE   0
/* A deferred application of a function to one argument, or to two in
 * turn, as upstream's mapAttrs nests two app values: the payload is the
 * cells, the function's first, one more than the form's number of
 * arguments (nn_env_alloc_app_cells). */
#define NN_PENDING_APPLY1 1
#define NN_PENDING_APPLY2 2

/* --- Value tags (valid when state == COMPUTED) --- */

#define NN_VALUE_INT     0   /* payload = (void*)(intptr_t)int64_value */
#define NN_VALUE_FLOAT   1   /* payload = memcpy'd double */
#define NN_VALUE_BOOL    2   /* payload = (void*)(intptr_t)(0 or 1) */
#define NN_VALUE_NULL    3   /* payload = NULL */
#define NN_VALUE_STR     4   /* payload = nn_str_t* (no context) */
#define NN_VALUE_PATH    5   /* payload = nn_str_t* (the path's UTF-8 text) */
#define NN_VALUE_LIST    6   /* payload = nn_list_t* */
#define NN_VALUE_ATTRS   7   /* payload = nn_attrset_t* */
#define NN_VALUE_CTXSTR  8   /* payload = nn_ctxstr_t* (string with context) */
#define NN_VALUE_LAMBDA  9   /* payload = nn_lambda_t* (lambda closure) */
#define NN_VALUE_PRIMOP  10  /* payload = (void*)(uintptr_t)builtin index */
#define NN_VALUE_PRIMOP_APP 11 /* payload = nn_primop_app_t* (nn_primop.h) */

/* --- Types --- */

/* A thunk: mutable cell holding either pending work or a computed value.
 * 16 bytes: state(1) + val_tag(1) + form(1) + _pad(1) + bc_idx(4) +
 * payload(8).  val_tag is valid only when state == COMPUTED, and form
 * only when it is PENDING or BLACKHOLE.
 *
 * PENDING:   form selects interpretation of bc_idx and payload:
 *            CODE: bc_idx = bytecode instruction index,
 *                  payload = nn_env_t * the code runs in
 *            APPLY1, APPLY2: payload = the application's cells,
 *                  bc_idx unused
 * COMPUTED:  val_tag selects interpretation of payload:
 *            INT/BOOL: (intptr_t) cast of scalar
 *            FLOAT: memcpy'd double
 *            NULL: ignored
 *            PRIMOP: (uintptr_t) cast of the builtin's index
 *            others: a pointer to the C object
 * BLACKHOLE: form, bc_idx and payload as they were PENDING, kept for
 *            nn_thunk_mark_pending, which puts the thunk back to work */
typedef struct nn_thunk {
    uint8_t   state;
    uint8_t   val_tag;
    uint8_t   form;      /* PENDING and BLACKHOLE: an NN_PENDING_* form */
    uint8_t   _pad;
    uint32_t  bc_idx;    /* PENDING code: bytecode index; otherwise never read */
    void     *payload;
} nn_thunk_t;

/* C99-compatible compile-time check of the 16 bytes above. */
typedef char nn_thunk_size_check_[(sizeof(nn_thunk_t) == 16) ? 1 : -1];

/* Nix integers are int64_t, stored inline via (intptr_t) cast into payload.
 * This requires pointers to be at least 64 bits.  32-bit targets would
 * silently truncate large integers.  C99-compatible compile-time check: */
typedef char nn_thunk_ptr_size_check_[(sizeof(void *) >= sizeof(int64_t)) ? 1 : -1];

/* --- Blocks --- */

/* The cells each block of the arena holds. */
#define NN_THUNK_BLOCK_CELLS 65536u

/* --- Lifecycle --- */

/* Initialize the global thunk arena with its first block.  Called only
 * while the arena is not live (nn_thunk_live is 0), as a second init
 * would leak the live arena's blocks, and before any thunk allocation:
 * outside the init .. destroy window every allocator returns NULL. */
void nn_thunk_init(void);

/* Nonzero between nn_thunk_init and nn_thunk_destroy. */
int nn_thunk_live(void);

/* Destroy the global thunk arena, freeing all block memory.  The
 * objects a cell points to belong to their own allocators.  Every
 * nn_thunk_t pointer into the arena becomes invalid after this call; the
 * static cells (below) do not. */
void nn_thunk_destroy(void);

/* --- Allocation --- */

/* No instruction's index, which nn_thunk_get_bc_idx answers for a thunk
 * that runs none: the bytecode emitters answer it only for an append
 * that failed (nn_bytecode.h). */
#define NN_THUNK_NO_CODE UINT32_MAX

/* Allocate a new PENDING thunk with a bytecode index + env payload.
 * Returns a pointer into arena memory, valid until nn_thunk_destroy(),
 * or NULL when the thunk cannot be allocated.
 * bc_idx is the root instruction index in the bytecode store, never
 * NN_THUNK_NO_CODE (asserted).
 * env is the nn_env_t * the code runs in.  It must already exist, but
 * its slots need not be filled yet: nothing reads them until the thunk
 * is forced. */
nn_thunk_t *nn_thunk_new_bc(uint32_t bc_idx, void *env);

/* Allocate a new PENDING thunk deferring the application of the function
 * in `fn` to the argument in `arg`, or, for nn_thunk_new_apply2, to
 * `first` and then the result to `second`.  No cell is NULL (asserted).
 * Returns NULL when the thunk or its cells cannot be allocated. */
nn_thunk_t *nn_thunk_new_apply1(nn_thunk_t *fn, nn_thunk_t *arg);
nn_thunk_t *nn_thunk_new_apply2(nn_thunk_t *fn, nn_thunk_t *first, nn_thunk_t *second);

/* Read the bytecode index of a PENDING or BLACKHOLE code thunk, or
 * NN_THUNK_NO_CODE for an application, so that one read tells a code
 * thunk, nearly every thunk forced, from the rest. */
uint32_t nn_thunk_get_bc_idx(const nn_thunk_t *thunk);

/* Read the form of a PENDING or BLACKHOLE thunk (NN_PENDING_*). */
uint8_t nn_thunk_pending_form(const nn_thunk_t *thunk);

/* Cell `i` of a PENDING or BLACKHOLE application, the function's at 0.
 * A thunk of another state or form, or `i` past its cells, aborts
 * (NN_ASSERT). */
nn_thunk_t *nn_thunk_apply_cell(const nn_thunk_t *thunk, uint32_t i);

/* Allocate pre-COMPUTED thunks with inline scalar values. */
nn_thunk_t *nn_thunk_new_computed_int(int64_t value);
nn_thunk_t *nn_thunk_new_computed_float(double value);
nn_thunk_t *nn_thunk_new_computed_primop(uint32_t primop);

/* Allocate pre-COMPUTED thunks pointing at C objects. */
nn_thunk_t *nn_thunk_new_computed_str(void *str);
nn_thunk_t *nn_thunk_new_computed_path(void *str);
nn_thunk_t *nn_thunk_new_computed_list(void *list);
nn_thunk_t *nn_thunk_new_computed_attrs(void *attrset);
nn_thunk_t *nn_thunk_new_computed_ctxstr(void *ctxstr);
nn_thunk_t *nn_thunk_new_computed_lambda(void *lambda);
nn_thunk_t *nn_thunk_new_computed_primop_app(void *app);

/* --- Shared constants --- */

/* The COMPUTED null, true and false cells and the empty list's, as
 * upstream shares one vNull, vTrue, vFalse and vEmptyList per EvalState.  They are file-scope statics, not
 * arena memory, so each has one address in every init .. destroy window
 * and outside all of them: a constant built from one before
 * nn_thunk_init, or kept across nn_thunk_destroy, still reads correctly.
 * Every state transition refuses a COMPUTED thunk, so nothing writes
 * them.  nn_thunk_bool answers the true cell for any nonzero value. */
nn_thunk_t *nn_thunk_null(void);
nn_thunk_t *nn_thunk_bool(uint8_t value);
nn_thunk_t *nn_thunk_empty_list(void);

/* The builtins the static cells can name: one more than the highest
 * index nn_thunk_primop answers for. */
#define NN_PRIMOP_CELLS 256u

/* The COMPUTED cell of the builtin at `primop`, as upstream binds one
 * value per primop (EvalState::addPrimOp at 2.24.9): a static, like the
 * null and Boolean cells, so the builtin bound in every environment is
 * the one cell, valid in every init .. destroy window and outside all of
 * them.  NULL for an index from NN_PRIMOP_CELLS on.  A cell is filled the
 * first time it is asked for, with values that never change after, so
 * nothing reads one in another state. */
nn_thunk_t *nn_thunk_primop(uint32_t primop);

/* Nonzero for a static cell: the null, true, false and empty list cells
 * and the builtins' cells, which no arena holds. */
int nn_thunk_is_static(const nn_thunk_t *thunk);

/* --- State queries --- */

/* Read the current state (NN_THUNK_PENDING/COMPUTED/BLACKHOLE). */
uint8_t nn_thunk_state(const nn_thunk_t *thunk);

/* Read the payload pointer.  For a COMPUTED thunk of a C object (val_tag
 * 4-9 and 11) it is that object, for the reader to cast to the type its
 * tag names; for a PENDING or BLACKHOLE one, its env or its application's
 * cells. */
void *nn_thunk_payload(const nn_thunk_t *thunk);

/* Read the value tag (valid when state == COMPUTED). */
uint8_t nn_thunk_value_tag(const nn_thunk_t *thunk);

/* Read inline values from a COMPUTED thunk: a scalar or a builtin's
 * index. */
int64_t  nn_thunk_get_int(const nn_thunk_t *thunk);
double   nn_thunk_get_float(const nn_thunk_t *thunk);
uint8_t  nn_thunk_get_bool(const nn_thunk_t *thunk);
uint32_t nn_thunk_get_primop(const nn_thunk_t *thunk);

/* --- State transitions --- */

/* Transition PENDING -> BLACKHOLE (thunk is being evaluated).
 * Payload remains unchanged (caller reads it before this call).
 * Returns 1 on success, 0 if thunk is not PENDING. */
int nn_thunk_mark_blackhole(nn_thunk_t *thunk);

/* Transition BLACKHOLE -> PENDING (a force threw and was caught upstream).
 * Form, payload and bc_idx are preserved, so the thunk is re-forceable; matches C++
 * Nix restoring a thunk after a caught exception during force.
 * Returns 1 on success, 0 if thunk is not BLACKHOLE. */
int nn_thunk_mark_pending(nn_thunk_t *thunk);

/* Set a non-COMPUTED thunk to COMPUTED, the setter naming the value
 * kind: an inline scalar, a builtin's index, or a C object.  Accepts PENDING or BLACKHOLE state (skips
 * blackhole for direct memoization).  The pending payload is overwritten:
 * it is an env or an application's cells, which the env pages own, so
 * there is nothing to hand back or free.  Returns 1, or 0 with the thunk
 * unchanged if it is already COMPUTED. */
int nn_thunk_set_computed_int(nn_thunk_t *thunk, int64_t value);
int nn_thunk_set_computed_float(nn_thunk_t *thunk, double value);
int nn_thunk_set_computed_bool(nn_thunk_t *thunk, uint8_t value);
int nn_thunk_set_computed_null(nn_thunk_t *thunk);
int nn_thunk_set_computed_str(nn_thunk_t *thunk, void *str);
int nn_thunk_set_computed_path(nn_thunk_t *thunk, void *str);
int nn_thunk_set_computed_list(nn_thunk_t *thunk, void *list);
int nn_thunk_set_computed_attrs(nn_thunk_t *thunk, void *attrset);
int nn_thunk_set_computed_ctxstr(nn_thunk_t *thunk, void *ctxstr);
int nn_thunk_set_computed_lambda(nn_thunk_t *thunk, void *lambda);
int nn_thunk_set_computed_primop(nn_thunk_t *thunk, uint32_t primop);
int nn_thunk_set_computed_primop_app(nn_thunk_t *thunk, void *app);

/* --- Census (nn_stats.h) --- */

/* The arena's blocks, oldest first: for each of the first `max`, its
 * first slot and the slots it has handed out go to `slots` and `counts`.
 * Returns the number of blocks, which may exceed `max`; 0 outside the
 * init .. destroy window. */
uint32_t nn_thunk_blocks(nn_thunk_t **slots, uint32_t *counts, uint32_t max);

/* Bytes the arena holds from the allocator: every block, header and
 * unused slots included. */
uint64_t nn_thunk_reserved_bytes(void);

/* The cells the arena has handed out since nn_thunk_init. */
uint64_t nn_thunk_cells_allocated(void);

/* The bytes of cells a block has handed out, given its first slot. */
uint64_t nn_thunk_block_used(const char *slots);

#endif /* NN_THUNK_H */
