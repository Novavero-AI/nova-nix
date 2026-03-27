/*
 * nn_symbol.h — Interned string symbols for nova-nix.
 *
 * Attribute names are the most repeated data in Nix evaluation.
 * nixpkgs uses ~8k unique names across 30k+ packages, but each name
 * appears hundreds of times.  Interning deduplicates storage and
 * replaces O(n) string comparison with O(1) integer comparison.
 *
 * Symbols are uint32_t indices into a global table.  The table owns
 * all string data in a contiguous arena (no per-string malloc).
 * Lookup uses open-addressing with FNV-1a hashing.
 *
 * Lifecycle: nn_symbol_init() before evaluation, nn_symbol_destroy()
 * after.  Not thread-safe — single-threaded evaluation only.
 */

#ifndef NN_SYMBOL_H
#define NN_SYMBOL_H

#include <stddef.h>
#include <stdint.h>

/* --- Types --- */

/* An interned symbol: index into the global symbol table.
 * 0 is reserved as the invalid/empty sentinel. */
typedef uint32_t nn_symbol_t;

/* Invalid symbol constant. */
#define NN_SYMBOL_INVALID ((nn_symbol_t)0)

/* --- Lifecycle --- */

/* Initialize the global symbol table.  Must be called once before
 * any nn_symbol_intern() calls.  initial_capacity is the expected
 * number of unique symbols (hint for pre-allocation; 0 uses default). */
void nn_symbol_init(uint32_t initial_capacity);

/* Destroy the global symbol table, freeing all memory.
 * All nn_symbol_t values become invalid after this call. */
void nn_symbol_destroy(void);

/* --- Core API --- */

/* Intern a string, returning its symbol.  If the string was already
 * interned, returns the existing symbol (O(1) amortized).
 * The input string is copied — the caller may free it after this call.
 * str must not be NULL.  len is the byte length (not null-terminated). */
nn_symbol_t nn_symbol_intern(const char *str, size_t len);

/* Return the string data for a symbol.  The pointer is valid until
 * nn_symbol_destroy().  Returns NULL for NN_SYMBOL_INVALID. */
const char *nn_symbol_text(nn_symbol_t sym);

/* Return the byte length of a symbol's string.
 * Returns 0 for NN_SYMBOL_INVALID. */
size_t nn_symbol_len(nn_symbol_t sym);

/* --- Comparison --- */

/* Symbols are equal iff their uint32_t values are equal.
 * No function call needed — just use == directly.
 * This macro exists for documentation purposes. */
#define nn_symbol_eq(a, b) ((a) == (b))

/* --- Diagnostics --- */

/* Return the number of unique symbols currently interned. */
uint32_t nn_symbol_count(void);

#endif /* NN_SYMBOL_H */
