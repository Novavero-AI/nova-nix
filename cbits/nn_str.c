/*
 * nn_str.c - String arenas (nn_str.h).
 *
 * A block holds the strings bumped from it, one after another, each
 * padded to NN_STR_ALIGNMENT.  A string larger than NN_STR_LARGE_BYTES
 * gets a block of its own, sized to it, so a fresh shared block is
 * started only for a string no larger than that, and the room a shared
 * block leaves unused at its end is under NN_STR_LARGE_BYTES.
 */

#include "nn_str.h"

#include "nn_region.h"

#include <stdint.h>
#include <stdlib.h>
#include <string.h>

/* --- Blocks --- */

struct nn_str_block {
    nn_str_block_t *next;
    uint64_t        used;      /* bytes handed out from data */
    uint64_t        capacity;  /* bytes data holds */
    char            data[];
};

/* Strings start on NN_STR_ALIGNMENT boundaries from data, so data itself
 * must start on one.  C99 compile-time check: */
typedef char nn_str_block_align_check_[
    (offsetof(nn_str_block_t, data) % NN_STR_ALIGNMENT == 0) ? 1 : -1];

/* A block with room for `capacity` bytes of strings, or NULL. */
static nn_str_block_t *block_new(uint64_t capacity)
{
    nn_str_block_t *b;
    if (capacity > (uint64_t)SIZE_MAX - offsetof(nn_str_block_t, data)) return NULL;
    b = (nn_str_block_t *)malloc(offsetof(nn_str_block_t, data) + (size_t)capacity);
    if (!b) return NULL;
    b->next = NULL;
    b->used = 0;
    b->capacity = capacity;
    return b;
}

/* Bytes a string of `len` bytes takes in a block.  len is at most
 * NN_STR_MAX_LEN, so the sum cannot wrap uint64_t. */
static uint64_t object_bytes(uint64_t len)
{
    uint64_t raw = (uint64_t)offsetof(nn_str_t, bytes) + len;
    return (raw + NN_STR_ALIGNMENT - 1) / NN_STR_ALIGNMENT * NN_STR_ALIGNMENT;
}

/* --- The empty string --- */

static nn_str_t g_empty = { 0 };

nn_str_t *nn_str_empty(void)
{
    return &g_empty;
}

/* --- Arenas --- */

/* The value arena.  The one other arena is the code's (nn_bytecode.h),
 * whose strings a census of regions places outside every region. */
static nn_str_arena_t g_values;

static void block_made(const nn_str_arena_t *arena, const nn_str_block_t *b, int current)
{
    if (nn_region_mode)
        nn_region_block(arena == &g_values ? NN_REGION_STR_BLOCK : NN_REGION_CODE_BLOCK,
                        b->data, b->capacity, current);
}

nn_str_t *nn_str_arena_new(nn_str_arena_t *arena, const char *bytes, size_t len)
{
    uint64_t need;
    nn_str_block_t *b;
    nn_str_t *s;

    if ((uint64_t)len > NN_STR_MAX_LEN) return NULL;
    if (len == 0) return &g_empty;

    need = object_bytes((uint64_t)len);
    if (need > NN_STR_LARGE_BYTES) {
        b = block_new(need);
        if (!b) return NULL;
        b->next = arena->blocks;
        arena->blocks = b;
        block_made(arena, b, 0);
    } else {
        b = arena->current;
        if (!b || b->capacity - b->used < need) {
            b = block_new(NN_STR_BLOCK_BYTES);
            if (!b) return NULL;
            b->next = arena->blocks;
            arena->blocks = b;
            arena->current = b;
            block_made(arena, b, 1);
        }
    }

    s = (nn_str_t *)(b->data + b->used);
    b->used += need;
    s->len = (uint32_t)len;
    memcpy(s->bytes, bytes, len);
    arena->count++;
    arena->bytes += need;
    return s;
}

void nn_str_arena_free(nn_str_arena_t *arena)
{
    nn_str_block_t *b = arena->blocks;
    while (b) {
        nn_str_block_t *next = b->next;
        free(b);
        b = next;
    }
    memset(arena, 0, sizeof(*arena));
}

/* --- The value arena --- */

static int g_live;

void nn_str_init(void)
{
    g_live = 1;
}

int nn_str_live(void)
{
    return g_live;
}

void nn_str_destroy(void)
{
    nn_str_arena_free(&g_values);
    g_live = 0;
}

nn_str_t *nn_str_new(const char *bytes, size_t len)
{
    if (!g_live) return NULL;
    return nn_str_arena_new(&g_values, bytes, len);
}

/* --- Reading --- */

uint32_t nn_str_len(const nn_str_t *s)
{
    return s->len;
}

const char *nn_str_bytes(const nn_str_t *s)
{
    return s->bytes;
}

/* --- Census --- */

const nn_str_arena_t *nn_str_values(void)
{
    return &g_values;
}

uint64_t nn_str_object_bytes(const nn_str_t *s)
{
    return object_bytes((uint64_t)s->len);
}

uint32_t nn_str_arena_blocks(const nn_str_arena_t *arena,
                             const char **starts, uint64_t *used, uint32_t max)
{
    const nn_str_block_t *b;
    uint32_t n = 0;
    for (b = arena->blocks; b; b = b->next) {
        if (n < max) {
            starts[n] = b->data;
            used[n] = b->used;
        }
        n++;
    }
    return n;
}

uint64_t nn_str_block_used(const char *data)
{
    const nn_str_block_t *b = (const nn_str_block_t *)(const void *)(data - offsetof(nn_str_block_t, data));
    return b->used;
}

uint64_t nn_str_arena_reserved_bytes(const nn_str_arena_t *arena)
{
    const nn_str_block_t *b;
    uint64_t total = 0;
    for (b = arena->blocks; b; b = b->next)
        total += (uint64_t)offsetof(nn_str_block_t, data) + b->capacity;
    return total;
}
