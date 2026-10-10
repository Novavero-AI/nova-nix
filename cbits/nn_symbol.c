/*
 * nn_symbol.c - Interned string symbol table.
 *
 * Implementation: open-addressed hash table with linear probing.
 * String data is stored in a contiguous arena (bulk allocation,
 * no per-string malloc/free).  Symbols are 1-indexed uint32_t
 * values; 0 is the invalid sentinel.
 *
 * The table doubles when load exceeds 75%.  Deletion is not
 * supported - symbols live for the entire evaluation lifetime.
 */

#include "nn_symbol.h"
#include "nn_assert.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* --- Configuration --- */

#define NN_SYMBOL_DEFAULT_CAPACITY   4096
#define NN_SYMBOL_MAX_INITIAL        (UINT32_C(1) << 30)  /* slot table is twice this */
#define NN_SYMBOL_LOAD_PERCENT         75
#define NN_SYMBOL_ARENA_INITIAL  (256 * 1024)  /* 256 KB */

/* --- Internal types --- */

/* Per-symbol metadata stored in a flat array indexed by symbol ID. */
typedef struct {
    uint32_t offset;    /* byte offset into string arena */
    uint32_t len;       /* byte length of the string */
    uint32_t hash;      /* cached FNV-1a hash */
} nn_symbol_entry_t;

/* Hash table slot: maps hash -> symbol ID.  Empty slots have id == 0. */
typedef struct {
    uint32_t hash;
    uint32_t id;        /* 1-based symbol ID, 0 = empty */
} nn_slot_t;

/* Global symbol table state. */
static struct {
    /* Symbol metadata array (1-indexed; index 0 unused). */
    nn_symbol_entry_t *entries;
    uint32_t           count;       /* number of interned symbols */
    uint32_t           entries_cap; /* allocated entry slots */

    /* Hash table (open addressing, linear probing). */
    nn_slot_t         *slots;
    uint32_t           slots_cap;   /* always a power of two */
    uint32_t           slots_mask;  /* slots_cap - 1 */

    /* Contiguous string arena. */
    char              *arena;
    size_t             arena_used;  /* never above NN_SYMBOL_ARENA_LIMIT */
    size_t             arena_cap;
} g_sym;

/* --- FNV-1a hash --- */

static uint32_t fnv1a(const char *data, size_t len)
{
    uint32_t h = 2166136261u;
    size_t i;
    for (i = 0; i < len; i++) {
        h ^= (uint8_t)data[i];
        h *= 16777619u;
    }
    return h;
}

/* --- Growth --- */

/* Each reserve step returns 0, or -1 when the table cannot grow, and
 * changes no symbol, so nn_symbol_intern can refuse after any of them. */

/* Room for `needed` more arena bytes; the caller has kept the total
 * within NN_SYMBOL_ARENA_LIMIT, which fits size_t on every 64-bit
 * target. */
static int arena_reserve(size_t needed)
{
    size_t want = g_sym.arena_used + needed;
    size_t new_cap = g_sym.arena_cap;
    if (want <= new_cap) return 0;

    while (new_cap < want) {
        new_cap = new_cap > SIZE_MAX / 2 ? want : new_cap * 2;
    }
    char *grown = (char *)realloc(g_sym.arena, new_cap);
    if (!grown) return -1;
    g_sym.arena = grown;
    g_sym.arena_cap = new_cap;
    return 0;
}

/* Room for one more entry.  Entries are 1-indexed, so count + 1 must
 * stay below the capacity. */
static int entries_reserve(void)
{
    if (g_sym.count + 1 < g_sym.entries_cap) return 0;
    if (g_sym.entries_cap > UINT32_MAX / 2) return -1;

    uint32_t new_cap = g_sym.entries_cap * 2;
    nn_symbol_entry_t *grown = (nn_symbol_entry_t *)realloc(
        g_sym.entries, (size_t)new_cap * sizeof(nn_symbol_entry_t));
    if (!grown) return -1;
    g_sym.entries = grown;
    g_sym.entries_cap = new_cap;
    return 0;
}

/* Rebuild the hash table at double capacity.  The slot capacity is a
 * uint32_t power of two, so it stops at 2^31; with the load kept under
 * NN_SYMBOL_LOAD_PERCENT that bounds the symbol count well below
 * UINT32_MAX, which is why no separate ID ceiling is checked. */
static int slots_grow(void)
{
    if (g_sym.slots_cap > UINT32_MAX / 2) return -1;

    uint32_t new_cap = g_sym.slots_cap * 2;
    uint32_t new_mask = new_cap - 1;
    nn_slot_t *new_slots = (nn_slot_t *)calloc((size_t)new_cap, sizeof(nn_slot_t));
    if (!new_slots) return -1;

    uint32_t i;
    for (i = 0; i < g_sym.slots_cap; i++) {
        if (g_sym.slots[i].id == 0) continue;
        uint32_t idx = g_sym.slots[i].hash & new_mask;
        while (new_slots[idx].id != 0) {
            idx = (idx + 1) & new_mask;
        }
        new_slots[idx] = g_sym.slots[i];
    }

    free(g_sym.slots);
    g_sym.slots = new_slots;
    g_sym.slots_cap = new_cap;
    g_sym.slots_mask = new_mask;
    return 0;
}

/* --- Public API --- */

void nn_symbol_init(uint32_t initial_capacity)
{
    if (g_sym.slots) nn_symbol_destroy();

    uint32_t cap = NN_SYMBOL_DEFAULT_CAPACITY;
    if (initial_capacity > cap) {
        /* Round up to a power of two.  The hint is clamped so that the
         * rounding and the slot table's doubling cannot wrap uint32_t. */
        uint32_t want = initial_capacity < NN_SYMBOL_MAX_INITIAL
                            ? initial_capacity : NN_SYMBOL_MAX_INITIAL;
        while (cap < want) cap *= 2;
    }

    memset(&g_sym, 0, sizeof(g_sym));

    /* Entry array (1-indexed, so allocate cap+1 but we just use cap). */
    g_sym.entries_cap = cap;
    g_sym.entries = (nn_symbol_entry_t *)calloc(
        (size_t)cap, sizeof(nn_symbol_entry_t));
    if (!g_sym.entries) { fprintf(stderr, "nn_symbol_init: entries alloc failed\n"); abort(); }
    g_sym.count = 0;

    /* Hash table at 2x entries for low load factor. */
    g_sym.slots_cap = cap * 2;
    g_sym.slots_mask = g_sym.slots_cap - 1;
    g_sym.slots = (nn_slot_t *)calloc(
        (size_t)g_sym.slots_cap, sizeof(nn_slot_t));
    if (!g_sym.slots) { fprintf(stderr, "nn_symbol_init: slots alloc failed\n"); abort(); }

    /* String arena. */
    g_sym.arena_cap = NN_SYMBOL_ARENA_INITIAL;
    g_sym.arena = (char *)malloc(g_sym.arena_cap);
    if (!g_sym.arena) { fprintf(stderr, "nn_symbol_init: arena alloc failed\n"); abort(); }
    g_sym.arena_used = 0;
}

int nn_symbol_live(void)
{
    return g_sym.slots != NULL;
}

void nn_symbol_destroy(void)
{
    free(g_sym.entries);
    free(g_sym.slots);
    free(g_sym.arena);
    memset(&g_sym, 0, sizeof(g_sym));
}

nn_symbol_t nn_symbol_intern(const char *str, size_t len)
{
    /* No table before nn_symbol_init or after nn_symbol_destroy: the
     * probe below would dereference a NULL slot array.  The invalid
     * sentinel is the one value the Haskell boundary refuses to wrap. */
    if (!g_sym.slots) return NN_SYMBOL_INVALID;

    /* A length past uint32_t can be neither stored nor already present;
     * truncated into the entry it would read back as a shorter string.
     * Refused here, before a byte of str is read. */
    if ((uint64_t)len > UINT32_MAX) return NN_SYMBOL_INVALID;

    /* An empty string arrives from Haskell's zero-copy marshalling as
     * (NULL, 0).  memcpy/memcmp require valid pointers even for a zero
     * length, so normalize at the boundary.  Empty symbols are reachable
     * from evaluated input ({ "" = 1; }), so the branch survives release
     * builds. */
    if (len == 0) {
        str = "";
    }
    uint32_t hash = fnv1a(str, len);
    uint32_t idx = hash & g_sym.slots_mask;

    /* Probe for existing entry. */
    for (;;) {
        uint32_t id = g_sym.slots[idx].id;
        if (id == 0) break;  /* empty slot - not found */

        if (g_sym.slots[idx].hash == hash) {
            nn_symbol_entry_t *e = &g_sym.entries[id];
            if (e->len == len &&
                memcmp(g_sym.arena + e->offset, str, len) == 0) {
                return (nn_symbol_t)id;
            }
        }
        idx = (idx + 1) & g_sym.slots_mask;
    }

    /* Not found - insert.  Everything the insert needs is reserved before
     * any of it is committed, so a refusal leaves the table as it was.
     * The slot table grows ahead of the insert that would reach the load
     * limit, which keeps an empty slot for every later probe to stop at. */
    if ((uint64_t)len >= NN_SYMBOL_ARENA_LIMIT - (uint64_t)g_sym.arena_used)
        return NN_SYMBOL_INVALID;
    if (arena_reserve(len + 1) != 0) return NN_SYMBOL_INVALID;
    if (entries_reserve() != 0) return NN_SYMBOL_INVALID;
    if ((uint64_t)(g_sym.count + 1) * 100 >= (uint64_t)g_sym.slots_cap * NN_SYMBOL_LOAD_PERCENT) {
        if (slots_grow() != 0) return NN_SYMBOL_INVALID;
        idx = hash & g_sym.slots_mask;
        while (g_sym.slots[idx].id != 0) {
            idx = (idx + 1) & g_sym.slots_mask;
        }
    }

    g_sym.count++;
    uint32_t new_id = g_sym.count;  /* 1-based */
    uint32_t offset = (uint32_t)g_sym.arena_used;

    memcpy(g_sym.arena + offset, str, len);
    g_sym.arena[offset + len] = '\0';
    g_sym.arena_used += len + 1;

    g_sym.entries[new_id].offset = offset;
    g_sym.entries[new_id].len = (uint32_t)len;
    g_sym.entries[new_id].hash = hash;

    g_sym.slots[idx].hash = hash;
    g_sym.slots[idx].id = new_id;

    return (nn_symbol_t)new_id;
}

const char *nn_symbol_text(nn_symbol_t sym)
{
    if (sym == NN_SYMBOL_INVALID || sym > g_sym.count) return NULL;
    return g_sym.arena + g_sym.entries[sym].offset;
}

size_t nn_symbol_len(nn_symbol_t sym)
{
    if (sym == NN_SYMBOL_INVALID || sym > g_sym.count) return 0;
    return (size_t)g_sym.entries[sym].len;
}

uint32_t nn_symbol_count(void)
{
    return g_sym.count;
}
