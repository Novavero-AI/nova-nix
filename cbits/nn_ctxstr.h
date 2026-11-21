/*
 * nn_ctxstr.h - Context-bearing strings for nova-nix.
 *
 * Nix strings carry a StringContext: a set of store path references
 * that track derivation dependencies.  Context-free strings use
 * nn_thunk tag 4 (an nn_str_t pointer).  Context-bearing strings use
 * tag 8 (nn_ctxstr_t pointer).
 *
 * Each context element is one of:
 *   SCPlain      - plain store path reference (inputSrcs)
 *   SCDrvOutput  - derivation output (.drv path + output name)
 *   SCAllOutputs - all outputs of a derivation (drvPath itself)
 *
 * The text, a StorePath's hash and name, and an output name are each an
 * nn_str_t (nn_str.h), made in the value arena for the string they
 * belong to, as upstream copies each context element for each value
 * (encodeContext, eval.cc:833-845 at 2.24.9).  None is a symbol: upstream
 * interns no string value, and a symbol taken here would move the place
 * of every name interned after it.
 *
 * Memory: each nn_ctxstr_t is a single contiguous allocation via
 * C99 flexible array member, tracked (nn_tracker.h) for bulk cleanup via
 * nn_ctxstr_free_all().
 */

#ifndef NN_CTXSTR_H
#define NN_CTXSTR_H

#include <stdint.h>

#include "nn_str.h"
#include "nn_tracker.h"

/* --- Context element tags --- */

#define NN_SCE_PLAIN        0
#define NN_SCE_DRV_OUTPUT   1
#define NN_SCE_ALL_OUTPUTS  2

/* --- Types --- */

/* A single string context element: store path reference with kind tag.
 * sp_hash and sp_name are the StorePath's hash and name.  output is the
 * derivation output name when tag == DRV_OUTPUT, and NULL otherwise. */
typedef struct nn_sce {
    uint8_t     tag;
    nn_str_t   *sp_hash;
    nn_str_t   *sp_name;
    nn_str_t   *output;
} nn_sce_t;

/* A string with context.  text is the string's bytes.  ctx_count is the
 * number of context elements; it is a full uint32_t because context sets
 * scale with the evaluated expression (a narrower count would wrap and
 * undersize the array).  ctx[] is a C99 flexible array of context
 * elements, in the order they were set.
 *
 * Single contiguous allocation: header + elements. */
typedef struct nn_ctxstr {
    nn_str_t   *text;
    uint32_t    ctx_count;
    nn_sce_t    ctx[];
} nn_ctxstr_t;

/* --- Lifecycle --- */

/* Allocate a new nn_ctxstr_t with space for ctx_count elements.
 * Elements are zeroed until the caller fills them via nn_ctxstr_set_*.
 * Tracked globally for bulk cleanup.  Returns NULL on allocation
 * failure or if the element array size would overflow - the caller
 * must check. */
nn_ctxstr_t *nn_ctxstr_new(nn_str_t *text, uint32_t ctx_count);

/* Free all tracked nn_ctxstr_t allocations.  Called once at arena
 * teardown. */
void nn_ctxstr_free_all(void);

/* --- Element setters --- */

/* idx must be < ctx_count: an index past it aborts (NN_ASSERT). */

void nn_ctxstr_set_plain(nn_ctxstr_t *s, uint32_t idx,
                         nn_str_t *sp_hash, nn_str_t *sp_name);

void nn_ctxstr_set_drv_output(nn_ctxstr_t *s, uint32_t idx,
                              nn_str_t *sp_hash, nn_str_t *sp_name,
                              nn_str_t *output);

void nn_ctxstr_set_all_outputs(nn_ctxstr_t *s, uint32_t idx,
                               nn_str_t *sp_hash, nn_str_t *sp_name);

/* --- Accessors --- */

nn_str_t *nn_ctxstr_text(const nn_ctxstr_t *s);
uint32_t  nn_ctxstr_ctx_count(const nn_ctxstr_t *s);

/* Element reads take idx < ctx_count: an index past it aborts
 * (NN_ASSERT). */
uint8_t   nn_ctxstr_elem_tag(const nn_ctxstr_t *s, uint32_t idx);
nn_str_t *nn_ctxstr_elem_hash(const nn_ctxstr_t *s, uint32_t idx);
nn_str_t *nn_ctxstr_elem_name(const nn_ctxstr_t *s, uint32_t idx);
nn_str_t *nn_ctxstr_elem_output(const nn_ctxstr_t *s, uint32_t idx);

/* --- Census (nn_stats.h) --- */

/* Every string allocated and not yet freed: the module's own tracker,
 * whose array moves when the next string is created and is freed by
 * nn_ctxstr_free_all. */
const nn_tracker_t *nn_ctxstr_tracked(void);

/* Free the strings from place `from` of the tracker on that `keep`
 * refuses, and close up the rest (nn_tracker_sweep). */
void nn_ctxstr_sweep(uint32_t from, nn_tracker_keep_t keep, void *ctx);

/* Bytes a string holds from the allocator: its header and its context
 * elements. */
uint64_t nn_ctxstr_bytes(const nn_ctxstr_t *s);

#endif /* NN_CTXSTR_H */
