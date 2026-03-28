/*
 * nn_ctxstr.h — Context-bearing strings for nova-nix.
 *
 * Nix strings carry a StringContext: a set of store path references
 * that track derivation dependencies.  Context-free strings use
 * nn_thunk tag 4 (bare nn_symbol_t).  Context-bearing strings use
 * tag 8 (nn_ctxstr_t pointer).
 *
 * Each context element is one of:
 *   SCPlain      — plain store path reference (inputSrcs)
 *   SCDrvOutput  — derivation output (.drv path + output name)
 *   SCAllOutputs — all outputs of a derivation (drvPath itself)
 *
 * StorePaths are represented as two interned symbols (hash + name).
 *
 * Memory: each nn_ctxstr_t is a single contiguous allocation via
 * C99 flexible array member.  Headers are tracked globally for
 * bulk cleanup via nn_ctxstr_free_all().
 */

#ifndef NN_CTXSTR_H
#define NN_CTXSTR_H

#include <stdint.h>

/* --- Context element tags --- */

#define NN_SCE_PLAIN        0
#define NN_SCE_DRV_OUTPUT   1
#define NN_SCE_ALL_OUTPUTS  2

/* --- Types --- */

/* A single string context element: store path reference with kind tag.
 * sp_hash and sp_name are interned nn_symbol_t values representing
 * the StorePath's hash and name fields.  output is an interned symbol
 * for the derivation output name (only valid when tag == DRV_OUTPUT,
 * 0 otherwise). */
typedef struct nn_sce {
    uint8_t     tag;
    uint32_t    sp_hash;
    uint32_t    sp_name;
    uint32_t    output;
} nn_sce_t;

/* A string with context.  text is an interned nn_symbol_t for the
 * string content.  ctx_count is the number of context elements.
 * ctx[] is a C99 flexible array of context elements, sorted by
 * (tag, sp_hash, sp_name, output) for deterministic comparison.
 *
 * Single contiguous allocation: header + elements. */
typedef struct nn_ctxstr {
    uint32_t    text;
    uint16_t    ctx_count;
    nn_sce_t    ctx[];
} nn_ctxstr_t;

/* --- Lifecycle --- */

/* Allocate a new nn_ctxstr_t with space for ctx_count elements.
 * Elements are uninitialized — caller must fill via nn_ctxstr_set_*.
 * Tracked globally for bulk cleanup. */
nn_ctxstr_t *nn_ctxstr_new(uint32_t text, uint16_t ctx_count);

/* Free all tracked nn_ctxstr_t allocations.  Called during arena
 * teardown, after thunk iteration but before env/symbol cleanup. */
void nn_ctxstr_free_all(void);

/* --- Element setters --- */

void nn_ctxstr_set_plain(nn_ctxstr_t *s, uint16_t idx,
                         uint32_t sp_hash, uint32_t sp_name);

void nn_ctxstr_set_drv_output(nn_ctxstr_t *s, uint16_t idx,
                              uint32_t sp_hash, uint32_t sp_name,
                              uint32_t output);

void nn_ctxstr_set_all_outputs(nn_ctxstr_t *s, uint16_t idx,
                               uint32_t sp_hash, uint32_t sp_name);

/* --- Accessors --- */

uint32_t nn_ctxstr_text(const nn_ctxstr_t *s);
uint16_t nn_ctxstr_ctx_count(const nn_ctxstr_t *s);

uint8_t  nn_ctxstr_elem_tag(const nn_ctxstr_t *s, uint16_t idx);
uint32_t nn_ctxstr_elem_hash(const nn_ctxstr_t *s, uint16_t idx);
uint32_t nn_ctxstr_elem_name(const nn_ctxstr_t *s, uint16_t idx);
uint32_t nn_ctxstr_elem_output(const nn_ctxstr_t *s, uint16_t idx);

#endif /* NN_CTXSTR_H */
