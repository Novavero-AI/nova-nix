/*
 * nn_str.h - The bytes of string and path values for nova-nix.
 *
 * A string value is a byte string: any byte, NUL included, with no
 * encoding assumed.  A path value's bytes are its UTF-8 text.  Each is one
 * nn_str_t, its length and then its bytes, never written once made.  A
 * thunk cell holding a string or a path points at its nn_str_t, and so
 * does a string with context for its text and its context's store paths
 * and output names (nn_ctxstr.h), so whatever refers to a string's bytes
 * holds their address.
 *
 * Nothing here interns.  Two equal strings made twice are two objects, as
 * upstream copies a string value's bytes for each value it makes
 * (makeImmutableString and Value::mkString in eval.cc at 2.24.9, and
 * StringData::make at 2.36.0).  The symbol table (nn_symbol.h) holds
 * names alone, so a string value never takes a symbol, and never moves
 * the place of a name interned after it.  Upstream hands every empty
 * string the one static "" (makeImmutableString); here every empty string
 * is nn_str_empty().
 *
 * Strings are made in arenas: blocks a string is bumped from, freed
 * together.  An nn_str_t never moves.  Each string starts on an
 * NN_STR_ALIGNMENT boundary, and within one room the next one starts
 * nn_str_object_bytes after it; while the region collector runs a block
 * can hold several rooms with unused bytes between them.  The values an
 * evaluation makes live in the value arena (nn_str_new); the bytecode keeps
 * its literals' bytes in an arena of its own, freed with the code
 * (nn_bytecode.h).
 *
 * A string's length is a uint32_t, so a string holds at most UINT32_MAX
 * bytes, the per-allocation limit used across the C data layer.
 *
 * Lifecycle: nn_str_init() before evaluation, nn_str_destroy() after.
 * Not thread-safe - single-threaded evaluation only.
 */

#ifndef NN_STR_H
#define NN_STR_H

#include <stddef.h>
#include <stdint.h>

/* --- Types --- */

/* A string's bytes: len of them, with no terminator. */
typedef struct nn_str {
    uint32_t len;
    char     bytes[];
} nn_str_t;

/* Every string starts on a multiple of this many bytes from its block's
 * start: the alignment of its length. */
#define NN_STR_ALIGNMENT ((uint64_t)sizeof(uint32_t))

/* The longest string an arena makes. */
#define NN_STR_MAX_LEN ((uint64_t)UINT32_MAX)

/* The bytes of a block strings share.  A string that takes more than a
 * quarter of one gets a block of its own, sized to it. */
#define NN_STR_BLOCK_BYTES ((uint64_t)1 << 20)
#define NN_STR_LARGE_BYTES (NN_STR_BLOCK_BYTES / 4)

typedef struct nn_str_block nn_str_block_t;

/* An arena of strings.  An all-zero one is empty and ready to use.
 * Strings that fit a shared block are bumped from a room of the current
 * block, from the cursor to below the limit: a new block, or, in the
 * value arena while the region collector runs, lines of a block it freed
 * (nn_region_refill). */
typedef struct {
    nn_str_block_t *blocks;   /* every block, newest first */
    nn_str_block_t *current;  /* the block strings that fit one are bumped from */
    uint64_t        cursor;   /* bytes into it */
    uint64_t        limit;
    uint64_t        count;    /* strings made, the empty string not counted */
    uint64_t        bytes;    /* the bytes they take, padding included */
} nn_str_arena_t;

/* --- Arenas --- */

/* Copy len bytes into a new string in the arena.  (NULL, 0) is the empty
 * string, which is nn_str_empty() and takes no room.  Returns NULL when
 * len is past NN_STR_MAX_LEN, refused before a byte is read, or when the
 * allocator refuses a block; the arena then holds the strings it held,
 * though while the region collector runs the room it bumped from has
 * ended (nn_region_refill). */
nn_str_t *nn_str_arena_new(nn_str_arena_t *arena, const char *bytes, size_t len);

/* Free every block.  The arena is empty after, and every string it made
 * is gone. */
void nn_str_arena_free(nn_str_arena_t *arena);

/* --- The value arena --- */

/* Start the value arena.  Called only while it is not live (nn_str_live
 * is 0), as a second init would leak the live arena's blocks; outside
 * the init .. destroy window nn_str_new returns NULL. */
void nn_str_init(void);

/* Nonzero between nn_str_init and nn_str_destroy. */
int nn_str_live(void);

/* End the value arena, freeing every string it made. */
void nn_str_destroy(void);

/* nn_str_arena_new in the value arena, or NULL outside its window. */
nn_str_t *nn_str_new(const char *bytes, size_t len);

/* The empty string: a static, not in any arena, valid in and outside
 * every window. */
nn_str_t *nn_str_empty(void);

/* --- Reading --- */

uint32_t    nn_str_len(const nn_str_t *s);
const char *nn_str_bytes(const nn_str_t *s);

/* --- Census (nn_stats.h) --- */

/* The value arena. */
const nn_str_arena_t *nn_str_values(void);

/* Bytes a string takes in its block: its length, its bytes and the
 * padding to the next NN_STR_ALIGNMENT boundary. */
uint64_t nn_str_object_bytes(const nn_str_t *s);

/* An arena's blocks: for each of the first `max`, where its strings start
 * and one past the last byte it has ever handed out go to `starts` and
 * `used`.  Returns the number of blocks, which may exceed `max`. */
uint32_t nn_str_arena_blocks(const nn_str_arena_t *arena,
                             const char **starts, uint64_t *used, uint32_t max);

/* Bytes an arena holds from the allocator: every block, header and
 * unused tail included. */
uint64_t nn_str_arena_reserved_bytes(const nn_str_arena_t *arena);

/* Bytes an arena's strings took as they were made, padding included,
 * which a collector reusing lines can put where others were. */
uint64_t nn_str_arena_bytes(const nn_str_arena_t *arena);

/* Where the next string that fits a shared block comes from in the value
 * arena's current block, in bytes from its first byte (nn_region.h). */
uint64_t nn_str_cursor(void);

#endif /* NN_STR_H */
