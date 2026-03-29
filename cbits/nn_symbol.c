/*
 * nn_symbol.c — Interned string symbol table.
 *
 * Implementation: open-addressed hash table with linear probing.
 * String data is stored in a contiguous arena (bulk allocation,
 * no per-string malloc/free).  Symbols are 1-indexed uint32_t
 * values; 0 is the invalid sentinel.
 *
 * The table doubles when load exceeds 75%.  Deletion is not
 * supported — symbols live for the entire evaluation lifetime.
 */

#include "nn_symbol.h"
#include "nn_assert.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* --- Configuration --- */

#define NN_SYMBOL_DEFAULT_CAPACITY   4096
#define NN_SYMBOL_LOAD_PERCENT         75
#define NN_SYMBOL_ARENA_INITIAL  (256 * 1024)  /* 256 KB */

/* --- Internal types --- */

/* Per-symbol metadata stored in a flat array indexed by symbol ID. */
typedef struct {
    uint32_t offset;    /* byte offset into string arena */
    uint32_t len;       /* byte length of the string */
    uint32_t hash;      /* cached FNV-1a hash */
} nn_symbol_entry_t;

/* Hash table slot: maps hash → symbol ID.  Empty slots have id == 0. */
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
    size_t             arena_used;
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

/* --- Arena --- */

/* Ensure the arena has room for `needed` more bytes. */
static void arena_ensure(size_t needed)
{
    if (g_sym.arena_used + needed <= g_sym.arena_cap) return;

    size_t new_cap = g_sym.arena_cap;
    while (new_cap < g_sym.arena_used + needed) {
        new_cap *= 2;
    }
    char *new_arena = (char *)realloc(g_sym.arena, new_cap);
    if (!new_arena) {
        fprintf(stderr, "nn_symbol: arena realloc failed (requested %zu bytes)\n", new_cap);
        abort();
    }
    g_sym.arena = new_arena;
    g_sym.arena_cap = new_cap;
}

/* Copy `len` bytes into the arena, returning the start offset. */
static uint32_t arena_push(const char *str, size_t len)
{
    arena_ensure(len + 1);  /* +1 for null terminator */
    uint32_t offset = (uint32_t)g_sym.arena_used;
    memcpy(g_sym.arena + offset, str, len);
    g_sym.arena[offset + len] = '\0';
    g_sym.arena_used += len + 1;
    return offset;
}

/* --- Entry array --- */

/* Ensure the entry array has room for one more symbol. */
static void entries_ensure(void)
{
    /* count is 0-based count, entries are 1-indexed, so we need count+1 < cap */
    if (g_sym.count + 1 < g_sym.entries_cap) return;

    uint32_t new_cap = g_sym.entries_cap * 2;
    nn_symbol_entry_t *new_entries = (nn_symbol_entry_t *)realloc(
        g_sym.entries, (size_t)new_cap * sizeof(nn_symbol_entry_t));
    if (!new_entries) {
        fprintf(stderr, "nn_symbol: entries realloc failed\n");
        abort();
    }
    g_sym.entries = new_entries;
    g_sym.entries_cap = new_cap;
}

/* --- Hash table --- */

/* Rebuild the hash table at double capacity. */
static void slots_grow(void)
{
    uint32_t new_cap = g_sym.slots_cap * 2;
    uint32_t new_mask = new_cap - 1;
    nn_slot_t *new_slots = (nn_slot_t *)calloc((size_t)new_cap, sizeof(nn_slot_t));
    if (!new_slots) {
        fprintf(stderr, "nn_symbol: slots calloc failed\n");
        abort();
    }

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
}

/* --- Public API --- */

void nn_symbol_init(uint32_t initial_capacity)
{
    uint32_t cap = NN_SYMBOL_DEFAULT_CAPACITY;
    if (initial_capacity > cap) {
        /* Round up to power of two. */
        cap = 1;
        while (cap < initial_capacity) cap *= 2;
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

void nn_symbol_destroy(void)
{
    free(g_sym.entries);
    free(g_sym.slots);
    free(g_sym.arena);
    memset(&g_sym, 0, sizeof(g_sym));
}

nn_symbol_t nn_symbol_intern(const char *str, size_t len)
{
    uint32_t hash = fnv1a(str, len);
    uint32_t idx = hash & g_sym.slots_mask;

    /* Probe for existing entry. */
    for (;;) {
        uint32_t id = g_sym.slots[idx].id;
        if (id == 0) break;  /* empty slot — not found */

        if (g_sym.slots[idx].hash == hash) {
            nn_symbol_entry_t *e = &g_sym.entries[id];
            if (e->len == (uint32_t)len &&
                memcmp(g_sym.arena + e->offset, str, len) == 0) {
                return (nn_symbol_t)id;
            }
        }
        idx = (idx + 1) & g_sym.slots_mask;
    }

    /* Not found — insert new symbol. */
    entries_ensure();
    g_sym.count++;
    uint32_t new_id = g_sym.count;  /* 1-based */

    uint32_t offset = arena_push(str, len);

    g_sym.entries[new_id].offset = offset;
    g_sym.entries[new_id].len = (uint32_t)len;
    g_sym.entries[new_id].hash = hash;

    g_sym.slots[idx].hash = hash;
    g_sym.slots[idx].id = new_id;

    /* Grow hash table if load exceeds threshold. */
    if ((uint64_t)g_sym.count * 100 >= (uint64_t)g_sym.slots_cap * NN_SYMBOL_LOAD_PERCENT) {
        slots_grow();
    }

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
