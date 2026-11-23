/*
 * nn_attrset.h - Sorted symbol-keyed attribute set for nova-nix.
 *
 * Parallel arrays of symbols, positions and values, sorted by symbol
 * (nn_attrset.c draws the layout), against the 48 bytes per node of the
 * pointer-chasing balanced tree Haskell's Data.Map.Strict would take.
 * A lookup is a binary search of each layer, top first, at most
 * NN_ATTRSET_MAX_LAYERS of them; iteration is a scan, merged across them.
 *
 * Construction is two-phase:
 *   1. nn_attrset_new(cap) + nn_attrset_insert() - append unsorted
 *   2. nn_attrset_freeze() - sort by symbol ID, enable binary search
 *
 * After freeze, the set is immutable (values can be updated in-place
 * but keys cannot change).  This matches Nix semantics: attribute sets
 * are constructed once and never gain/lose keys.
 *
 * A set nn_attrset_update makes can be a layer: its own arrays hold the
 * right operand's entries, and it sits on the left operand, its base,
 * which it binds the rest of, as upstream 2.34's Bindings layer on one
 * another (attr-set.hh:56-112 at 2.34.9).  A lookup searches the layers
 * top first, and the set binds each name once, to its topmost entry, in
 * symbol order.  A set's size counts every name it binds; the functions
 * that take an index read a flat set alone, one with no base, as a name
 * table always is.
 *
 * Values are the nn_thunk_t * of each attribute, or NULL in a frame's
 * name table until the frame fills it.  The set never dereferences them;
 * a census (nn_stats.h) follows them.
 *
 * Each entry also carries the position it was defined at, an opaque
 * uint32_t the evaluator assigns and resolves, as each attribute of
 * upstream's Bindings carries a PosIdx (attr-set.hh at 2.24.9).
 * Positions travel with their entries through freeze, union and key
 * removal.
 *
 * Lifecycle: arena-managed.  Every set is tracked at creation and
 * bulk-freed by nn_attrset_free_all at arena teardown, which frees no
 * value: the values are cells the thunk arena owns.
 *
 * Every constructor names its site, the construct or builtin that makes
 * the set (nn_stats.h), under which the census counts it.
 *
 * One set is not allocated: nn_attrset_empty, the set with no entries
 * the evaluator gives wherever upstream gives its one emptyBindings,
 * which its allocBindings hands out for a capacity of 0
 * (attr-set.cc:14-17 at 2.24.9).  The constructors here make a new set
 * whenever they are called.
 *
 * Counts and indices are uint32_t.  A set holds at most
 * NN_ATTRSET_MAX_ENTRIES entries, the ceiling upstream's Bindings has
 * (its size_t is uint32_t), so every index is below NN_ATTRSET_NOT_FOUND.
 * An operation that would pass it, or that the allocator refuses,
 * reports failure instead of aborting.
 */

#ifndef NN_ATTRSET_H
#define NN_ATTRSET_H

#include "nn_symbol.h"
#include "nn_tracker.h"
#include <stddef.h>
#include <stdint.h>

/* --- Types --- */

/* Opaque attribute set handle. */
typedef struct nn_attrset nn_attrset_t;

/* Most entries one set can hold. */
#define NN_ATTRSET_MAX_ENTRIES UINT32_MAX

/* nn_attrset_index's answer for an absent key: no index reaches it. */
#define NN_ATTRSET_NOT_FOUND UINT32_MAX

/* The most sets one chain of layers holds, the set and its bases,
 * upstream's Bindings::maxLayers (attr-set.hh:117 at 2.34.9). */
#define NN_ATTRSET_MAX_LAYERS 8u

/* The most entries a right operand of nn_attrset_update has for the
 * result to be a layer on the left: upstream's default for
 * eval-attrset-update-layer-rhs-threshold (eval-settings.hh:451-453 at
 * 2.34.9), which is larger where a pointer takes 4 bytes. */
#define NN_ATTRSET_LAYER_RHS_MAX (sizeof(void *) == 4 ? 8192u : 16u)

/* --- Lifecycle --- */

/* Allocate a new empty attribute set at `site` with space for `capacity`
 * entries (0 picks a small default).  Returns NULL on allocation
 * failure. */
nn_attrset_t *nn_attrset_new(uint32_t capacity, uint32_t site);

/* The shared empty set: frozen, no entries, and no arrays behind it.  A
 * file-scope static, not tracked and never freed, so it has one address
 * in every evaluation and outside all of them.  Nothing writes it: it is
 * frozen, so nn_attrset_insert never adds to it, and it has no index for
 * nn_attrset_set_value to write at. */
nn_attrset_t *nn_attrset_empty(void);

/* --- Construction (before freeze) --- */

/* Append a key-value pair defined at `pos`.  Keys need not be sorted or
 * unique at this stage: freeze sorts them, and of a repeated key keeps
 * the last insert, position included.  The parser refuses a repeated
 * name, but an expression built through Nix.Expr can hold one in a
 * frame's names, and the resolver likewise keeps a repeated name's last
 * definition.  Grows the internal arrays if capacity is exceeded.
 * Returns 0, or -1 with the set unchanged when it already holds
 * NN_ATTRSET_MAX_ENTRIES entries or cannot grow.  A frozen set aborts
 * (NN_ASSERT). */
int nn_attrset_insert(nn_attrset_t *set, nn_symbol_t key, void *value, uint32_t pos);

/* Sort the entries by symbol ID, keep the last insert of a repeated key,
 * and freeze the set: a lookup needs the order, and nn_attrset_insert
 * aborts on a frozen set.  Freezing a frozen set does nothing. */
void nn_attrset_freeze(nn_attrset_t *set);

/* --- Query (after freeze) --- */

/* Look up a key by symbol.  Returns the value pointer, or NULL if
 * the key is not present.  A binary search of each layer, top first. */
void *nn_attrset_lookup(const nn_attrset_t *set, nn_symbol_t key);

/* Whether the set binds `key`, writing its entry's position through
 * `pos` and its value through `value`, each only when not NULL.  A value
 * that is NULL, as a name table's is until its frame fills it, is found
 * as well, where nn_attrset_lookup answers NULL as for a key not bound.
 * Returns 1 when the set binds the key, else 0 with nothing written.
 * A binary search of each layer, top first. */
int nn_attrset_find(const nn_attrset_t *set, nn_symbol_t key,
                    uint32_t *pos, void **value);

/* Return the index of a key, or NN_ATTRSET_NOT_FOUND.
 * Useful for updating value slots in place.  This and the functions
 * below that take an index read a flat set; a layered one aborts
 * (NN_ASSERT). */
uint32_t nn_attrset_index(const nn_attrset_t *set, nn_symbol_t key);

/* Update the value at a known index.  Index must be valid (0 <= idx < count).
 * Fills a frame's name table, built with NULL values before its thunks
 * exist. */
void nn_attrset_set_value(nn_attrset_t *set, uint32_t idx, void *value);

/* Return the value at a known index. */
void *nn_attrset_get_value(const nn_attrset_t *set, uint32_t idx);

/* Return the key at a known index. */
nn_symbol_t nn_attrset_get_key(const nn_attrset_t *set, uint32_t idx);

/* Return the position of the entry at a known index. */
uint32_t nn_attrset_get_pos(const nn_attrset_t *set, uint32_t idx);

/* --- Bulk access --- */

/* Number of entries (after freeze: the names the set binds, in every
 * layer, each once). */
uint32_t nn_attrset_size(const nn_attrset_t *set);

/* The set a layer sits on, or NULL for a flat set. */
const nn_attrset_t *nn_attrset_base(const nn_attrset_t *set);

/* Write the entries a frozen set binds, nn_attrset_size of them, in
 * symbol order: each name's key, position and value, from the topmost
 * layer that holds the name, to the three arrays, each with room for
 * that many. */
void nn_attrset_entries(const nn_attrset_t *set, nn_symbol_t *keys,
                        uint32_t *positions, void **values);

/* --- Lifecycle: bulk cleanup --- */

/* Free all tracked attribute sets at once (arena-style cleanup).
 * Every set the constructors here make is tracked.
 * Call this once at evaluation end, before destroying sub-arenas. */
void nn_attrset_free_all(void);

/* --- Set operations --- */

/* Create a new flat set at `site` that is the right-biased union of `a`
 * and `b` (keys in `b` override `a`, positions with them), sized for the
 * entries of both.  Both inputs must be frozen.
 * The result is returned frozen and is arena-tracked (freed by
 * nn_attrset_free_all).  Returns NULL when the union would pass
 * NN_ATTRSET_MAX_ENTRIES or an allocation fails. */
nn_attrset_t *nn_attrset_union(const nn_attrset_t *a, const nn_attrset_t *b,
                               uint32_t site);

/* `a // b` at `site`, as upstream's ExprOpUpdate::eval makes it
 * (eval.cc:1916-1989 at 2.34.9): while a's chain holds fewer than
 * NN_ATTRSET_MAX_LAYERS sets and b at most NN_ATTRSET_LAYER_RHS_MAX
 * entries, a layer on `a` holding b's entries, else the set
 * nn_attrset_union makes.  While the census counts sites (nn_stats.h),
 * the entries copied count: b's for a layer, the result's for a union,
 * as upstream counts them in nrOpUpdateValuesCopied.  The evaluator
 * gives the other operand for an empty one, as upstream does before it
 * counts, so it calls this with two sets that hold entries; given one
 * with none, this makes the union.  Both inputs must be frozen.  Returns
 * NULL as nn_attrset_union does, and for a layer whose size would pass
 * NN_ATTRSET_MAX_ENTRIES. */
nn_attrset_t *nn_attrset_update(const nn_attrset_t *a, const nn_attrset_t *b,
                                uint32_t site);

/* Create a new flat set at `site` with the given keys removed, sized for
 * the entries of `set`.  Input must be frozen.
 * The result is returned frozen and is arena-tracked (freed by
 * nn_attrset_free_all).  Returns NULL when an allocation fails. */
nn_attrset_t *nn_attrset_remove_keys(
    const nn_attrset_t *set,
    const nn_symbol_t *keys,
    uint32_t key_count,
    uint32_t site);

/* --- Census (nn_stats.h) --- */

/* Every set allocated and not yet freed: the module's own tracker, whose
 * array moves when the next set is created and is freed by
 * nn_attrset_free_all. */
const nn_tracker_t *nn_attrset_tracked(void);

/* Free the sets from place `from` of the tracker on that `keep` refuses,
 * and close up the rest (nn_tracker_sweep). */
void nn_attrset_sweep(uint32_t from, nn_tracker_keep_t keep, void *ctx);

/* Bytes a set holds from the allocator: its header, and its keys,
 * positions and values at its capacity. */
uint64_t nn_attrset_bytes(const nn_attrset_t *set);

/* The entries a frozen set's own arrays hold, in symbol order, a
 * layer's without its base's: their number, and the arrays of their
 * keys and their values, which hold that many and are valid until the
 * set is freed.  Before freeze the arrays hold the inserts in their
 * order, a name repeated among them, and move when the set grows.  The
 * object graph follows a set by them and its base (nn_graph.h), and the
 * census counts its size by them. */
uint32_t nn_attrset_layer_count(const nn_attrset_t *set);
const nn_symbol_t *nn_attrset_layer_keys(const nn_attrset_t *set);
void *const *nn_attrset_layer_values(const nn_attrset_t *set);

#endif /* NN_ATTRSET_H */
