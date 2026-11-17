/*
 * nn_symbol.h - Interned string symbols for nova-nix.
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
 * after.  Not thread-safe - single-threaded evaluation only.
 *
 * Offsets and lengths are uint32_t, so the string arena holds at most
 * NN_SYMBOL_ARENA_LIMIT bytes, terminators included.  An intern that
 * would cross it, or that needs a table to grow past its uint32_t
 * capacity or past what the allocator gives, is refused with
 * NN_SYMBOL_INVALID and leaves the table as it was.
 */

#ifndef NN_SYMBOL_H
#define NN_SYMBOL_H

#include <stddef.h>
#include <stdint.h>

/* --- Types --- */

/* An interned symbol: index into the global symbol table.  0 is the
 * invalid sentinel; the empty string interns as an ordinary symbol. */
typedef uint32_t nn_symbol_t;

/* Invalid symbol constant. */
#define NN_SYMBOL_INVALID ((nn_symbol_t)0)

/* Bytes the string arena may hold: every offset and length fits uint32_t. */
#define NN_SYMBOL_ARENA_LIMIT ((uint64_t)UINT32_MAX + 1)

/* --- Lifecycle --- */

/* Initialize the global symbol table.  Called only while the table is
 * not live (nn_symbol_live is 0), as a second init would leak the live
 * table, and before any nn_symbol_intern() call: outside the init ..
 * destroy window nn_symbol_intern() returns NN_SYMBOL_INVALID. */
void nn_symbol_init(void);

/* Nonzero between nn_symbol_init and nn_symbol_destroy. */
int nn_symbol_live(void);

/* Destroy the global symbol table, freeing all memory.
 * All nn_symbol_t values become invalid after this call. */
void nn_symbol_destroy(void);

/* --- Core API --- */

/* Intern a string, returning its symbol.  If the string was already
 * interned, returns the existing symbol (O(1) amortized).
 * The input string is copied - the caller may free it after this call.
 * len is the byte length (not null-terminated).  (NULL, 0) denotes the
 * empty string; str must not be NULL when len > 0.  Returns
 * NN_SYMBOL_INVALID outside the init .. destroy window and when the
 * table refuses the string (see above); a length past the arena limit
 * is refused before any byte of str is read. */
nn_symbol_t nn_symbol_intern(const char *str, size_t len);

/* Return the string data for a symbol, valid until the next
 * nn_symbol_intern (which can move the table's bytes) or
 * nn_symbol_destroy().  Returns NULL for NN_SYMBOL_INVALID and for an id
 * the table does not hold. */
const char *nn_symbol_text(nn_symbol_t sym);

/* Return the byte length of a symbol's string.
 * Returns 0 for NN_SYMBOL_INVALID and for an id the table does not hold. */
size_t nn_symbol_len(nn_symbol_t sym);

/* --- Diagnostics --- */

/* Return the number of unique symbols currently interned. */
uint32_t nn_symbol_count(void);

/* --- Census (nn_stats.h) --- */

/* The bytes the table holds: in its string arena, each symbol's text
 * and terminator (*text), and from the allocator, the arena, entry and
 * hash tables at their capacities (*reserved). */
void nn_symbol_footprint(uint64_t *text, uint64_t *reserved);

#endif /* NN_SYMBOL_H */
