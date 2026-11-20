/*
 * nn_region.c - A census of regions (nn_region.h).
 *
 * Marks are bits the census keeps beside the data layer: a bit per unit
 * of each block (a thunk cell, NN_ENV_ALIGNMENT bytes of an env page,
 * NN_STR_ALIGNMENT bytes of strings) and a bit per place in each tracker.
 * A survivor's bits cover its whole extent, so whether a block or a part
 * of one holds a survivor is a scan of its bits.  Between closes every
 * mark is clear: a close marks only objects of its own region, and clears
 * its region's bits as it retires them.  A second bit per unit and place
 * remembers what a close found unreached.
 *
 * The address index maps each NN_REGION_UNIT_BYTES of address space a
 * block covers to that block.  Every block is at least a unit, so no
 * block lies inside a unit without reaching its edge, and at most two
 * blocks touch a unit: the one over its first byte and the one over its
 * last.  The tracked objects are found by address in a table of their
 * own.  Both are open-addressing tables kept at most half full.
 */

#include "nn_region.h"

#include "nn_assert.h"
#include "nn_attrset.h"
#include "nn_ctxstr.h"
#include "nn_env.h"
#include "nn_graph.h"
#include "nn_lambda.h"
#include "nn_list.h"
#include "nn_primop.h"
#include "nn_roots.h"
#include "nn_str.h"
#include "nn_thunk.h"

#include <stdlib.h>
#include <string.h>

/* --- Constants --- */

#define NN_REGION_UNIT_SHIFT 18
#define NN_REGION_UNIT_BYTES ((uint64_t)1 << NN_REGION_UNIT_SHIFT)

/* No block, and no slot of the address index. */
#define NN_REGION_NONE UINT32_MAX

/* The low bit of a record: set for a with-scope entry, clear for a cell.
 * Both are pointer-aligned, so the bit is otherwise zero. */
#define NN_REGION_RECORD_WITH ((uintptr_t)1)

/* The first capacity of every table, which doubles from there.  A power
 * of two, as the address and object tables' probes need. */
#define NN_REGION_INITIAL 1024u

/* Each grain's block is a quarter of the one before: this many bits
 * shorter. */
#define NN_REGION_GRAIN_SHIFT 2u

/* Multiplicative hashing's constant, 2^64 divided by the golden ratio. */
#define NN_REGION_HASH 0x9E3779B97F4A7C15ull

/* The finest grain's blocks must hold whole bytes of marks, and the
 * thresholds must fit in 64 bits. */
typedef char nn_region_env_unit_check_[(NN_ENV_ALIGNMENT == sizeof(void *)) ? 1 : -1];
typedef char nn_region_threshold_check_[(NN_REGION_THRESHOLDS < 30) ? 1 : -1];

/* --- Types --- */

typedef struct {
    uintptr_t start;   /* the block's first byte of data */
    uintptr_t end;     /* one past its capacity */
    uint8_t  *marks;   /* a bit per unit: reached by the close under way */
    uint8_t  *died;    /* a bit per unit: found unreached by a close */
    int       kind;
} block_t;

/* A unit of address space and the blocks that touch it. */
typedef struct {
    uintptr_t unit;    /* UINTPTR_MAX for an empty slot */
    uint32_t  blocks[2];
} unit_slot_t;

/* A tracked object, its place in its tracker and its kind; for a list
 * header, also the first entry of the items array its elements are the
 * tail of. */
typedef struct {
    uintptr_t object;  /* 0 for an empty slot */
    uintptr_t first;
    uint32_t  position;
    int       kind;
} object_slot_t;

/* A tracked kind's marks and died bits, by place. */
typedef struct {
    uint8_t *marks;
    uint8_t *died;
    uint32_t capacity; /* places the bits have room for */
} places_t;

/* An open region: what each allocator had handed out as it opened. */
typedef struct {
    uint32_t blocks;                                 /* blocks numbered before it */
    uint32_t current[NN_REGION_BUMP_KINDS];          /* the block each bumped from */
    uint64_t current_used[NN_REGION_BUMP_KINDS];     /* and the bytes it had handed out */
    uint32_t tracked[NN_REGION_TRACKED_KINDS];       /* each tracker's count */
    uint64_t tracked_bytes;
    uint64_t allocated;
    uint32_t records;
    uint32_t root_thunks;
    uint32_t root_envs;
    /* Bytes the simulated collector freed inside this region, at closes
     * of the regions inside it, under each threshold at each grain. */
    uint64_t freed[NN_REGION_THRESHOLDS][NN_REGION_GRAINS];
} frame_t;

typedef struct {
    const void *object;
    int         kind;
} work_t;

/* Whether an object is younger than a region, older, or in no block the
 * census numbers. */
typedef enum { AGE_YOUNGER, AGE_OLDER, AGE_OUTSIDE } age_t;

/* --- State --- */

int nn_region_census = 0;

static struct {
    int failed;

    block_t    *blocks;
    uint32_t    block_count;
    uint32_t    block_cap;
    uint32_t    current[NN_REGION_BUMP_KINDS];
    uint32_t    last;               /* the block the last lookup found */

    unit_slot_t *units;
    uint32_t     unit_cap;
    uint32_t     unit_used;

    object_slot_t *objects;
    uint32_t       object_cap;
    uint32_t       object_used;
    places_t       places[NN_REGION_TRACKED_KINDS];
    uint64_t       tracked_bytes;

    uintptr_t *records;
    uint32_t   record_count;
    uint32_t   record_cap;

    frame_t  *frames;
    uint32_t  frame_count;
    uint32_t  frame_cap;

    work_t   *work;
    uint32_t  work_count;
    uint32_t  work_cap;

    uint8_t  *live;                 /* scratch: a flag per finest block */
    uint64_t  live_cap;

    /* The close under way. */
    const frame_t *closing;
    uint64_t surviving;
    uint64_t surviving_tracked;
    uint64_t entered;
    uint64_t examined;

    uint64_t enclosed;              /* bytes the outermost regions allocated */
    uint64_t totals[NN_REGION_TOTALS];
    uint64_t classes[NN_REGION_SIZE_CLASSES][NN_REGION_CLASS_FIGURES];
    uint64_t thresholds[NN_REGION_THRESHOLDS][NN_REGION_THRESHOLD_FIGURES];
    uint64_t freed[NN_REGION_THRESHOLDS][NN_REGION_GRAINS];
} c;

static void fail(void)
{
    c.failed = 1;
    c.totals[NN_REGION_TOTAL_FAILED] = 1;
}

/* An array of `size`-byte elements with room for `need`, doubling its
 * capacity *cap as it grows: the array, moved or not, or NULL with the
 * array and *cap unchanged. */
static void *reserve(void *array, uint32_t *cap, size_t size, uint32_t need)
{
    uint32_t grown = *cap ? *cap : NN_REGION_INITIAL;
    void *moved;
    if (need <= *cap) return array;
    while (grown < need) {
        if (grown > UINT32_MAX / 2) return NULL;
        grown *= 2;
    }
    moved = realloc(array, (size_t)grown * size);
    if (!moved) return NULL;
    *cap = grown;
    return moved;
}

/* --- Bits --- */

static uint8_t *bits_new(uint64_t bits)
{
    return (uint8_t *)calloc((size_t)(bits / 8 + 1), 1);
}

static int bit_get(const uint8_t *bits, uint64_t i)
{
    return (bits[i / 8] >> (i % 8)) & 1u;
}

static void bit_set(uint8_t *bits, uint64_t i)
{
    bits[i / 8] |= (uint8_t)(1u << (i % 8));
}

static void bits_set(uint8_t *bits, uint64_t from, uint64_t to)
{
    uint64_t i;
    for (i = from; i < to; i++) bit_set(bits, i);
}

/* Whether any bit from `from` to below `to` is set. */
static int bits_any(const uint8_t *bits, uint64_t from, uint64_t to)
{
    uint64_t i = from;
    for (; i < to && i % 8 != 0; i++)
        if (bit_get(bits, i)) return 1;
    for (; i + 8 <= to; i += 8)
        if (bits[i / 8]) return 1;
    for (; i < to; i++)
        if (bit_get(bits, i)) return 1;
    return 0;
}

/* A close's retirement of the units or places from `from` to below `to`:
 * each one unmarked has died, and every mark is cleared.  The bits around
 * the range belong to objects outside the region and are left alone. */
static void bits_retire(uint8_t *marks, uint8_t *died, uint64_t from, uint64_t to)
{
    uint64_t i = from;
    for (; i < to && i % 8 != 0; i++) {
        uint8_t mask = (uint8_t)(1u << (i % 8));
        if (!(marks[i / 8] & mask)) died[i / 8] |= mask;
        marks[i / 8] &= (uint8_t)~mask;
    }
    for (; i + 8 <= to; i += 8) {
        died[i / 8] |= (uint8_t)~marks[i / 8];
        marks[i / 8] = 0;
    }
    for (; i < to; i++) {
        uint8_t mask = (uint8_t)(1u << (i % 8));
        if (!(marks[i / 8] & mask)) died[i / 8] |= mask;
        marks[i / 8] &= (uint8_t)~mask;
    }
}

/* --- Kinds --- */

/* The bytes a mark bit of a bump kind's block stands for. */
static uint64_t unit_bytes(int kind)
{
    switch (kind) {
    case NN_REGION_THUNK_BLOCK: return sizeof(nn_thunk_t);
    case NN_REGION_ENV_PAGE:    return NN_ENV_ALIGNMENT;
    default:                    return NN_STR_ALIGNMENT;
    }
}

/* The block size a bump kind's allocator makes when no object needs a
 * larger one. */
static uint64_t block_bytes(int kind)
{
    switch (kind) {
    case NN_REGION_THUNK_BLOCK: return (uint64_t)NN_THUNK_BLOCK_CELLS * sizeof(nn_thunk_t);
    case NN_REGION_ENV_PAGE:    return NN_ENV_PAGE_SIZE;
    default:                    return NN_STR_BLOCK_BYTES;
    }
}

/* The bytes a block has handed out. */
static uint64_t block_used(const block_t *b)
{
    const char *data = (const char *)b->start;
    switch (b->kind) {
    case NN_REGION_THUNK_BLOCK: return nn_thunk_block_used(data);
    case NN_REGION_ENV_PAGE:    return nn_env_page_used(data);
    default:                    return nn_str_block_used(data);
    }
}

/* Every grain's block holds whole bytes of marks, so the finest does. */
typedef char nn_region_thunk_grain_check_[
    ((NN_THUNK_BLOCK_CELLS >> (NN_REGION_GRAIN_SHIFT * (NN_REGION_GRAINS - 2))) % 8 == 0) ? 1 : -1];
typedef char nn_region_env_grain_check_[
    (((NN_ENV_PAGE_SIZE >> (NN_REGION_GRAIN_SHIFT * (NN_REGION_GRAINS - 2))) / NN_ENV_ALIGNMENT) % 8 == 0) ? 1 : -1];
typedef char nn_region_str_grain_check_[
    (((NN_STR_BLOCK_BYTES >> (NN_REGION_GRAIN_SHIFT * (NN_REGION_GRAINS - 2))) / NN_STR_ALIGNMENT) % 8 == 0) ? 1 : -1];

uint64_t nn_region_grain_bytes(int kind, uint32_t grain)
{
    if (grain == 0 || grain >= NN_REGION_GRAINS) return 0;
    return block_bytes(kind) >> (NN_REGION_GRAIN_SHIFT * (grain - 1));
}

uint64_t nn_region_threshold_bytes(uint32_t k)
{
    if (k == 0) return 0;
    return (uint64_t)NN_REGION_THRESHOLD_FIRST << (NN_REGION_GRAIN_SHIFT * (k - 1));
}

static uint32_t tracker_count(int kind)
{
    switch (kind) {
    case NN_REGION_SET:        return nn_attrset_tracked()->count;
    case NN_REGION_LIST:       return nn_list_tracked()->count;
    case NN_REGION_LAMBDA:     return nn_lambda_tracked()->count;
    case NN_REGION_PRIMOP_APP: return nn_primop_app_tracked()->count;
    default:                   return nn_ctxstr_tracked()->count;
    }
}

static uint64_t tracked_bytes(int kind, const void *object)
{
    switch (kind) {
    case NN_REGION_SET:        return nn_attrset_bytes((const nn_attrset_t *)object);
    case NN_REGION_LIST:       return sizeof(nn_list_t);
    case NN_REGION_LAMBDA:     return nn_lambda_bytes((const nn_lambda_t *)object);
    case NN_REGION_PRIMOP_APP: return nn_primop_app_bytes((const nn_primop_app_t *)object);
    default:                   return nn_ctxstr_bytes((const nn_ctxstr_t *)object);
    }
}

/* Every byte the data layer has handed out for objects. */
static uint64_t allocated_now(void)
{
    return nn_thunk_cells_allocated() * sizeof(nn_thunk_t) + nn_env_bytes_allocated()
         + nn_str_values()->bytes + c.tracked_bytes;
}

/* --- The address index --- */

static uint32_t hash(uint64_t key, uint32_t cap)
{
    return (uint32_t)((key * NN_REGION_HASH) >> 32) & (cap - 1);
}

/* The slot of a unit, or of the empty slot where it would go. */
static unit_slot_t *unit_slot(unit_slot_t *slots, uint32_t cap, uintptr_t unit)
{
    uint32_t i = hash((uint64_t)unit, cap);
    while (slots[i].unit != UINTPTR_MAX && slots[i].unit != unit) i = (i + 1) & (cap - 1);
    return &slots[i];
}

static unit_slot_t *units_new(uint32_t cap)
{
    uint32_t i;
    unit_slot_t *slots = (unit_slot_t *)malloc((size_t)cap * sizeof(unit_slot_t));
    if (!slots) return NULL;
    for (i = 0; i < cap; i++) slots[i].unit = UINTPTR_MAX;
    return slots;
}

/* Index a unit's block.  Returns 0 or -1. */
static int unit_add(uintptr_t unit, uint32_t block)
{
    unit_slot_t *slot;
    if ((uint64_t)(c.unit_used + 1) * 2 > c.unit_cap) {
        uint32_t cap = c.unit_cap * 2, i;
        unit_slot_t *slots;
        if (c.unit_cap > UINT32_MAX / 2 || !(slots = units_new(cap))) return -1;
        for (i = 0; i < c.unit_cap; i++)
            if (c.units[i].unit != UINTPTR_MAX) *unit_slot(slots, cap, c.units[i].unit) = c.units[i];
        free(c.units);
        c.units = slots;
        c.unit_cap = cap;
    }
    slot = unit_slot(c.units, c.unit_cap, unit);
    if (slot->unit == UINTPTR_MAX) {
        slot->unit = unit;
        slot->blocks[0] = block;
        slot->blocks[1] = NN_REGION_NONE;
        c.unit_used++;
        return 0;
    }
    NN_ASSERT(slot->blocks[1] == NN_REGION_NONE, "nn_region_block: a third block touches a unit");
    slot->blocks[1] = block;
    return 0;
}

/* The block of a bump kind a pointer is in, or NN_REGION_NONE. */
static uint32_t block_of(uintptr_t p, int kind)
{
    const unit_slot_t *slot;
    int i;
    if (c.last != NN_REGION_NONE && p >= c.blocks[c.last].start && p < c.blocks[c.last].end)
        return c.blocks[c.last].kind == kind ? c.last : NN_REGION_NONE;
    slot = unit_slot(c.units, c.unit_cap, p >> NN_REGION_UNIT_SHIFT);
    if (slot->unit == UINTPTR_MAX) return NN_REGION_NONE;
    for (i = 0; i < 2; i++) {
        uint32_t b = slot->blocks[i];
        if (b != NN_REGION_NONE && p >= c.blocks[b].start && p < c.blocks[b].end) {
            c.last = b;
            return c.blocks[b].kind == kind ? b : NN_REGION_NONE;
        }
    }
    return NN_REGION_NONE;
}

/* --- The tracked objects' table --- */

static object_slot_t *object_slot(object_slot_t *slots, uint32_t cap, uintptr_t object)
{
    /* malloc aligns every object, so the low bits carry no information. */
    uint32_t i = hash((uint64_t)object >> 4, cap);
    while (slots[i].object != 0 && slots[i].object != object) i = (i + 1) & (cap - 1);
    return &slots[i];
}

static int object_add(uintptr_t object, uint32_t position, int kind, uintptr_t first)
{
    object_slot_t *slot;
    if ((uint64_t)(c.object_used + 1) * 2 > c.object_cap) {
        uint32_t cap = c.object_cap * 2, i;
        object_slot_t *slots;
        if (c.object_cap > UINT32_MAX / 2) return -1;
        slots = (object_slot_t *)calloc((size_t)cap, sizeof(object_slot_t));
        if (!slots) return -1;
        for (i = 0; i < c.object_cap; i++)
            if (c.objects[i].object != 0) *object_slot(slots, cap, c.objects[i].object) = c.objects[i];
        free(c.objects);
        c.objects = slots;
        c.object_cap = cap;
    }
    slot = object_slot(c.objects, c.object_cap, object);
    if (slot->object == 0) c.object_used++;
    slot->object = object;
    slot->first = first;
    slot->position = position;
    slot->kind = kind;
    return 0;
}

/* Room in a tracked kind's bits for `need` places. */
static int places_reserve(places_t *places, uint32_t need)
{
    uint32_t cap = places->capacity ? places->capacity : NN_REGION_INITIAL;
    uint8_t *marks, *died;
    size_t old_bytes = (size_t)places->capacity / 8 + 1, bytes;
    if (need <= places->capacity) return 0;
    while (cap < need) {
        if (cap > UINT32_MAX / 2) return -1;
        cap *= 2;
    }
    bytes = (size_t)cap / 8 + 1;
    marks = (uint8_t *)realloc(places->marks, bytes);
    if (!marks) return -1;
    places->marks = marks;
    died = (uint8_t *)realloc(places->died, bytes);
    if (!died) return -1;
    places->died = died;
    /* A first allocation has no old bytes to keep. */
    if (places->capacity == 0) old_bytes = 0;
    memset(places->marks + old_bytes, 0, bytes - old_bytes);
    memset(places->died + old_bytes, 0, bytes - old_bytes);
    places->capacity = cap;
    return 0;
}

/* --- Lifecycle --- */

static void release(void)
{
    uint32_t i;
    for (i = 0; i < c.block_count; i++) {
        free(c.blocks[i].marks);
        free(c.blocks[i].died);
    }
    for (i = 0; i < NN_REGION_TRACKED_KINDS; i++) {
        free(c.places[i].marks);
        free(c.places[i].died);
    }
    free(c.blocks);
    free(c.units);
    free(c.objects);
    free(c.records);
    free(c.frames);
    free(c.work);
    free(c.live);
    memset(&c, 0, sizeof(c));
    nn_region_census = 0;
}

int nn_region_census_begin(void)
{
    int k;
    NN_ASSERT(!nn_thunk_live() && !nn_env_live() && !nn_str_live(),
              "nn_region_census_begin: the arena is live");
    release();
    c.units = units_new(NN_REGION_INITIAL);
    c.objects = (object_slot_t *)calloc(NN_REGION_INITIAL, sizeof(object_slot_t));
    if (!c.units || !c.objects) {
        release();
        return -1;
    }
    c.unit_cap = NN_REGION_INITIAL;
    c.object_cap = NN_REGION_INITIAL;
    for (k = 0; k < NN_REGION_BUMP_KINDS; k++) c.current[k] = NN_REGION_NONE;
    c.last = NN_REGION_NONE;
    nn_region_census = 1;
    return 0;
}

void nn_region_census_end(void)
{
    release();
}

/* --- The allocators' half --- */

void nn_region_block(int kind, const char *data, uint64_t capacity, int current)
{
    block_t *blocks, *b;
    uint32_t index = c.block_count;
    uintptr_t unit, last;
    if (c.failed) return;
    NN_ASSERT(capacity >= NN_REGION_UNIT_BYTES, "nn_region_block: a block smaller than the index's unit");
    blocks = (block_t *)reserve(c.blocks, &c.block_cap, sizeof(block_t), index + 1);
    if (!blocks) {
        fail();
        return;
    }
    c.blocks = blocks;
    b = &c.blocks[index];
    b->start = (uintptr_t)data;
    b->end = b->start + (uintptr_t)capacity;
    b->kind = kind;
    b->marks = NULL;
    b->died = NULL;
    c.block_count++;
    /* Nothing in a block of code is ever marked. */
    if (kind != NN_REGION_CODE_BLOCK) {
        b->marks = bits_new(capacity / unit_bytes(kind));
        b->died = bits_new(capacity / unit_bytes(kind));
        if (!b->marks || !b->died) {
            fail();
            return;
        }
    }
    last = (b->end - 1) >> NN_REGION_UNIT_SHIFT;
    for (unit = b->start >> NN_REGION_UNIT_SHIFT; unit <= last; unit++) {
        if (unit_add(unit, index) != 0) {
            fail();
            return;
        }
    }
    if (current && kind < NN_REGION_BUMP_KINDS) c.current[kind] = index;
}

static void track(int kind, const void *object, uint32_t position, uint64_t bytes, uintptr_t first)
{
    if (c.failed) return;
    if (object_add((uintptr_t)object, position, kind, first) != 0
        || places_reserve(&c.places[kind], position + 1) != 0) {
        fail();
        return;
    }
    c.tracked_bytes += bytes;
}

void nn_region_tracked(int kind, const void *object, uint32_t position, uint64_t bytes)
{
    track(kind, object, position, bytes, 0);
}

void nn_region_list(const void *list, uint32_t position, const void *of)
{
    uintptr_t first = (uintptr_t)((const nn_list_t *)list)->items;
    if (of) {
        const object_slot_t *slot = object_slot(c.objects, c.object_cap, (uintptr_t)of);
        if (slot->object == (uintptr_t)of && slot->kind == NN_REGION_LIST) first = slot->first;
    }
    track(NN_REGION_LIST, list, position, sizeof(nn_list_t), first);
}

void nn_region_tracked_grew(uint64_t bytes)
{
    c.tracked_bytes += bytes;
}

static void record(uintptr_t r)
{
    uintptr_t *records;
    if (c.failed || c.frame_count == 0) return;
    records = (uintptr_t *)reserve(c.records, &c.record_cap, sizeof(uintptr_t), c.record_count + 1);
    if (!records) {
        fail();
        return;
    }
    c.records = records;
    c.records[c.record_count++] = r;
    if (c.record_count > c.totals[NN_REGION_TOTAL_RECORDS])
        c.totals[NN_REGION_TOTAL_RECORDS] = c.record_count;
}

void nn_region_record_cell(const struct nn_thunk *cell)
{
    record((uintptr_t)cell);
}

void nn_region_record_with(void *const *entry)
{
    record((uintptr_t)entry | NN_REGION_RECORD_WITH);
}

/* --- Age --- */

static age_t bump_age(const frame_t *f, uint32_t b, uintptr_t p)
{
    int kind = c.blocks[b].kind;
    if (b >= f->blocks) return AGE_YOUNGER;
    if (b == f->current[kind] && (uint64_t)(p - c.blocks[b].start) >= f->current_used[kind])
        return AGE_YOUNGER;
    return AGE_OLDER;
}

/* The age of what a record names: a cell, or a with-scope entry in an
 * env page. */
static age_t record_age(const frame_t *f, uintptr_t r)
{
    int with = (r & NN_REGION_RECORD_WITH) != 0;
    uintptr_t p = r & ~NN_REGION_RECORD_WITH;
    uint32_t b = block_of(p, with ? NN_REGION_ENV_PAGE : NN_REGION_THUNK_BLOCK);
    if (b == NN_REGION_NONE) return AGE_OUTSIDE;
    return bump_age(f, b, p);
}

/* --- The mark --- */

static void work_push(int kind, const void *object)
{
    work_t *work = (work_t *)reserve(c.work, &c.work_cap, sizeof(work_t), c.work_count + 1);
    if (!work) {
        fail();
        return;
    }
    c.work = work;
    c.work[c.work_count].object = object;
    c.work[c.work_count].kind = kind;
    c.work_count++;
}

/* A survivor of `bytes` whose died bit is `died`. */
static void survive(uint64_t bytes, int died)
{
    c.surviving += bytes;
    if (died) {
        c.totals[NN_REGION_TOTAL_RESURRECTED]++;
        c.totals[NN_REGION_TOTAL_RESURRECTED_BYTES] += bytes;
    }
}

static int is_static(int kind, const void *object)
{
    if (kind == NN_GRAPH_THUNK) return nn_thunk_is_static((const nn_thunk_t *)object);
    if (kind == NN_GRAPH_ENV) return object == nn_env_empty();
    if (kind == NN_GRAPH_ATTRSET) return object == nn_attrset_empty();
    return 0;
}

/* Reach a thunk cell or an env: mark its extent when it is younger than
 * the closing region and unmarked, and push it. */
static void reach_bump(int kind, const void *object)
{
    int block_kind = kind == NN_GRAPH_THUNK ? NN_REGION_THUNK_BLOCK : NN_REGION_ENV_PAGE;
    uintptr_t p = (uintptr_t)object;
    uint32_t b = block_of(p, block_kind);
    uint64_t unit, offset, u, bytes;
    block_t *blk;
    if (b == NN_REGION_NONE) {
        c.totals[NN_REGION_TOTAL_UNPLACED]++;
        return;
    }
    if (bump_age(c.closing, b, p) != AGE_YOUNGER) return;
    blk = &c.blocks[b];
    unit = unit_bytes(block_kind);
    offset = (uint64_t)(p - blk->start);
    u = offset / unit;
    if (offset % unit != 0) {
        c.totals[NN_REGION_TOTAL_UNPLACED]++;
        return;
    }
    if (bit_get(blk->marks, u)) return;
    bytes = kind == NN_GRAPH_THUNK ? sizeof(nn_thunk_t) : nn_env_bytes((const nn_env_t *)object);
    bits_set(blk->marks, u, u + bytes / unit);
    survive(bytes, bit_get(blk->died, u));
    work_push(kind, object);
}

static int tracked_kind(int kind)
{
    switch (kind) {
    case NN_GRAPH_ATTRSET:    return NN_REGION_SET;
    case NN_GRAPH_LIST:       return NN_REGION_LIST;
    case NN_GRAPH_LAMBDA:     return NN_REGION_LAMBDA;
    case NN_GRAPH_PRIMOP_APP: return NN_REGION_PRIMOP_APP;
    default:                  return NN_REGION_CTXSTR;
    }
}

/* Reach a tracked object: mark its place when it is younger than the
 * closing region and unmarked, and push it. */
static void reach_tracked(int kind, const void *object)
{
    int tracked = tracked_kind(kind);
    const object_slot_t *slot = object_slot(c.objects, c.object_cap, (uintptr_t)object);
    places_t *places = &c.places[tracked];
    uint64_t bytes;
    if (slot->object == 0 || slot->kind != tracked) {
        c.totals[NN_REGION_TOTAL_UNPLACED]++;
        return;
    }
    if (slot->position < c.closing->tracked[tracked]) return;
    if (bit_get(places->marks, slot->position)) return;
    bit_set(places->marks, slot->position);
    bytes = tracked_bytes(tracked, object);
    c.surviving_tracked += bytes;
    survive(bytes, bit_get(places->died, slot->position));
    work_push(kind, object);
}

static void visit_object(void *ctx, int kind, const void *object)
{
    (void)ctx;
    c.examined++;
    if (is_static(kind, object)) return;
    if (kind == NN_GRAPH_THUNK || kind == NN_GRAPH_ENV)
        reach_bump(kind, object);
    else
        reach_tracked(kind, object);
}

/* A string of the code's is outside every region, as code is. */
static void visit_string(void *ctx, const nn_str_t *s)
{
    uintptr_t p = (uintptr_t)s;
    uint32_t b;
    uint64_t offset, u, bytes;
    block_t *blk;
    (void)ctx;
    c.examined++;
    if (s == nn_str_empty()) return;
    b = block_of(p, NN_REGION_STR_BLOCK);
    if (b == NN_REGION_NONE) {
        if (block_of(p, NN_REGION_CODE_BLOCK) == NN_REGION_NONE) c.totals[NN_REGION_TOTAL_UNPLACED]++;
        return;
    }
    if (bump_age(c.closing, b, p) != AGE_YOUNGER) return;
    blk = &c.blocks[b];
    offset = (uint64_t)(p - blk->start);
    u = offset / NN_STR_ALIGNMENT;
    if (bit_get(blk->marks, u)) return;
    bytes = nn_str_object_bytes(s);
    bits_set(blk->marks, u, u + bytes / NN_STR_ALIGNMENT);
    survive(bytes, bit_get(blk->died, u));
}

static void visit_symbol(void *ctx, uint32_t sym)
{
    (void)ctx;
    (void)sym;
}

static void visit_unknown(void *ctx)
{
    (void)ctx;
    c.totals[NN_REGION_TOTAL_UNPLACED]++;
}

static void visit_array(void *ctx, int kind, const void *owner, void *const *base, uint32_t count);

static const nn_graph_visitor_t visitor = {
    visit_object, visit_array, visit_string, visit_symbol, visit_unknown
};

/* The elements of a list's items array before the ones it spans: kept
 * with the array, which an allocator frees whole, and not followed, as
 * the list cannot reach them. */
static void keep_head(const block_t *blk, const void *list, uintptr_t p)
{
    const object_slot_t *slot = object_slot(c.objects, c.object_cap, (uintptr_t)list);
    uint64_t u;
    if (slot->object != (uintptr_t)list || slot->first < blk->start || slot->first >= p) return;
    for (u = (slot->first - blk->start) / NN_ENV_ALIGNMENT; u < (p - blk->start) / NN_ENV_ALIGNMENT; u++) {
        if (bit_get(blk->marks, u)) continue;
        bit_set(blk->marks, u);
        survive(sizeof(void *), bit_get(blk->died, u));
    }
}

/* An array of the env pages, entry by entry: several list headers can
 * span parts of one items array, so each entry is marked on its own. */
static void visit_array(void *ctx, int kind, const void *owner, void *const *base, uint32_t count)
{
    uintptr_t p = (uintptr_t)base;
    uint32_t b, i;
    uint64_t u;
    block_t *blk;
    c.examined++;
    b = block_of(p, NN_REGION_ENV_PAGE);
    if (b == NN_REGION_NONE) {
        c.totals[NN_REGION_TOTAL_UNPLACED]++;
        return;
    }
    if (bump_age(c.closing, b, p) != AGE_YOUNGER) return;
    blk = &c.blocks[b];
    if (kind == NN_GRAPH_ITEMS) keep_head(blk, owner, p);
    u = (uint64_t)(p - blk->start) / NN_ENV_ALIGNMENT;
    for (i = 0; i < count; i++, u++) {
        if (bit_get(blk->marks, u)) continue;
        bit_set(blk->marks, u);
        survive(sizeof(void *), bit_get(blk->died, u));
        nn_graph_entry(kind, base[i], &visitor, ctx);
    }
}

/* Mark from the closing region's records of older objects and the roots
 * registered since it opened. */
static void mark(const frame_t *f)
{
    uint32_t i, n;
    for (i = f->records; i < c.record_count; i++) {
        uintptr_t r = c.records[i];
        age_t age = record_age(f, r);
        if (age == AGE_OUTSIDE) c.totals[NN_REGION_TOTAL_UNPLACED]++;
        if (age != AGE_OLDER) continue;
        if (r & NN_REGION_RECORD_WITH)
            nn_graph_entry(NN_GRAPH_WITH_SCOPES, *(void *const *)(r & ~NN_REGION_RECORD_WITH), &visitor, NULL);
        else
            nn_graph_children(NN_GRAPH_THUNK, (const void *)r, &visitor, NULL);
    }
    for (i = f->root_thunks, n = nn_roots_thunk_count(); i < n; i++)
        visit_object(NULL, NN_GRAPH_THUNK, nn_roots_thunk(i));
    for (i = f->root_envs, n = nn_roots_env_count(); i < n; i++)
        visit_object(NULL, NN_GRAPH_ENV, nn_roots_env(i));
    while (c.work_count > 0 && !c.failed) {
        work_t item = c.work[--c.work_count];
        c.entered++;
        nn_graph_children(item.kind, item.object, &visitor, NULL);
    }
}

/* --- Retiring a region --- */

/* Retire a block's units from `from` bytes in, the closing region's part
 * of it, and add to dead[g] the bytes of each block of grain g wholly in
 * that part that holds no survivor. */
static int retire_block(block_t *blk, uint64_t from, uint64_t *dead)
{
    uint64_t unit = unit_bytes(blk->kind);
    uint64_t used = block_used(blk);
    uint64_t finest = nn_region_grain_bytes(blk->kind, NN_REGION_GRAINS - 1);
    uint64_t first, count, j;
    uint32_t g;
    if (used <= from) return 0;
    first = (from + finest - 1) / finest;
    count = (used + finest - 1) / finest;
    if (count > first && count - first > c.live_cap) {
        uint8_t *live = (uint8_t *)realloc(c.live, (size_t)(count - first));
        if (!live) return -1;
        c.live = live;
        c.live_cap = count - first;
    }
    for (j = first; j < count; j++) {
        uint64_t end = (j + 1) * finest < used ? (j + 1) * finest : used;
        c.live[j - first] = (uint8_t)bits_any(blk->marks, j * finest / unit, end / unit);
    }
    for (g = 1; g < NN_REGION_GRAINS; g++) {
        uint64_t size = nn_region_grain_bytes(blk->kind, g);
        uint64_t ratio = size / finest;
        uint64_t big;
        for (big = (from + size - 1) / size; big * size < used; big++) {
            uint64_t end = (big + 1) * size < used ? (big + 1) * size : used;
            int live = 0;
            for (j = big * ratio; j < (big + 1) * ratio && j < count && !live; j++) live = c.live[j - first];
            if (!live) dead[g] += end - big * size;
        }
    }
    bits_retire(blk->marks, blk->died, from / unit, used / unit);
    return 0;
}

/* Retire every block the closing region allocated in, and every tracked
 * object it made. */
static void retire(const frame_t *f, uint64_t *dead)
{
    uint32_t b;
    int k;
    for (k = 0; k < NN_REGION_BUMP_KINDS && !c.failed; k++) {
        if (f->current[k] != NN_REGION_NONE
            && retire_block(&c.blocks[f->current[k]], f->current_used[k], dead) != 0)
            fail();
    }
    for (b = f->blocks; b < c.block_count && !c.failed; b++)
        if (c.blocks[b].kind < NN_REGION_BUMP_KINDS && retire_block(&c.blocks[b], 0, dead) != 0) fail();
    /* Every object a tracker gained while the census ran has a place in
     * the bits; one it held before has none and is in no region. */
    for (k = 0; k < NN_REGION_TRACKED_KINDS; k++) {
        uint32_t end = tracker_count(k);
        if (end > c.places[k].capacity) end = c.places[k].capacity;
        if (f->tracked[k] < end)
            bits_retire(c.places[k].marks, c.places[k].died, f->tracked[k], end);
    }
}

/* --- Figures --- */

static uint32_t size_class(uint64_t bytes)
{
    uint32_t n = 0;
    while (bytes > 0 && n < NN_REGION_SIZE_CLASSES - 1) {
        bytes >>= 1;
        n++;
    }
    return n;
}

static void count_close(uint64_t allocated, uint32_t records)
{
    uint64_t *row = c.classes[size_class(allocated)];
    row[NN_REGION_CLASS_CLOSES]++;
    row[NN_REGION_CLASS_ALLOCATED] += allocated;
    row[NN_REGION_CLASS_SURVIVING] += c.surviving;
    row[NN_REGION_CLASS_ENTERED] += c.entered;
    row[NN_REGION_CLASS_EXAMINED] += c.examined;
    row[NN_REGION_CLASS_RECORDS] += records;
    c.totals[NN_REGION_TOTAL_CLOSES]++;
}

/* The simulated collector at this close.  The bytes held peak just before
 * a close frees, so each close first counts what is held.  Under a
 * threshold the region reaches, the close frees what died in it less what
 * the closes inside it freed, and the region around it learns that all
 * of it is freed; under one it does not reach, it frees nothing and passes
 * on what the closes inside it freed.  What died in a region includes
 * everything that died in the regions inside it unless a close reached it
 * again, which the census counts as resurrected. */
static void simulate(const frame_t *f, frame_t *parent, uint64_t allocated, const uint64_t *dead)
{
    uint64_t now = allocated_now();
    uint32_t k, g;
    for (k = 0; k < NN_REGION_THRESHOLDS; k++) {
        int marks = allocated >= nn_region_threshold_bytes(k);
        uint64_t *row = c.thresholds[k];
        if (marks) {
            row[NN_REGION_THRESHOLD_CLOSES]++;
            row[NN_REGION_THRESHOLD_ENTERED] += c.entered;
            row[NN_REGION_THRESHOLD_EXAMINED] += c.examined;
        }
        for (g = 0; g < NN_REGION_GRAINS; g++) {
            uint64_t held = now - c.freed[k][g];
            uint64_t inside = f->freed[k][g];
            uint64_t up = inside;
            if (held > row[NN_REGION_THRESHOLD_PEAK + g]) row[NN_REGION_THRESHOLD_PEAK + g] = held;
            if (marks && dead[g] > inside) {
                c.freed[k][g] += dead[g] - inside;
                up = dead[g];
            }
            if (parent) parent->freed[k][g] += up;
        }
    }
}

/* Keep for the region around the closing one the records of objects
 * older than it too. */
static void pass_records(const frame_t *f, const frame_t *parent)
{
    uint32_t kept = f->records, i;
    if (parent) {
        for (i = f->records; i < c.record_count; i++)
            if (record_age(parent, c.records[i]) == AGE_OLDER) c.records[kept++] = c.records[i];
    }
    c.record_count = kept;
}

/* --- The evaluator's half --- */

void nn_region_open(void)
{
    frame_t *frames, *f;
    int k;
    if (!nn_region_census || c.failed) return;
    frames = (frame_t *)reserve(c.frames, &c.frame_cap, sizeof(frame_t), c.frame_count + 1);
    if (!frames) {
        fail();
        return;
    }
    c.frames = frames;
    f = &c.frames[c.frame_count++];
    f->blocks = c.block_count;
    for (k = 0; k < NN_REGION_BUMP_KINDS; k++) {
        f->current[k] = c.current[k];
        f->current_used[k] = c.current[k] == NN_REGION_NONE ? 0 : block_used(&c.blocks[c.current[k]]);
    }
    for (k = 0; k < NN_REGION_TRACKED_KINDS; k++) f->tracked[k] = tracker_count(k);
    f->tracked_bytes = c.tracked_bytes;
    f->allocated = allocated_now();
    f->records = c.record_count;
    nn_roots_mark(&f->root_thunks, &f->root_envs);
    memset(f->freed, 0, sizeof(f->freed));
    if (c.frame_count > c.totals[NN_REGION_TOTAL_DEPTH]) c.totals[NN_REGION_TOTAL_DEPTH] = c.frame_count;
}

void nn_region_close(void)
{
    frame_t *f, *parent;
    uint64_t allocated, tracked, dead[NN_REGION_GRAINS];
    uint32_t g, records;
    if (!nn_region_census || c.failed) return;
    NN_ASSERT(c.frame_count > 0, "nn_region_close: no region is open");
    f = &c.frames[c.frame_count - 1];
    parent = c.frame_count > 1 ? &c.frames[c.frame_count - 2] : NULL;
    c.closing = f;
    c.surviving = 0;
    c.surviving_tracked = 0;
    c.entered = 0;
    c.examined = 0;
    records = c.record_count - f->records;

    mark(f);
    memset(dead, 0, sizeof(dead));
    if (!c.failed) retire(f, dead);
    if (c.failed) return;

    allocated = allocated_now() - f->allocated;
    tracked = c.tracked_bytes - f->tracked_bytes;
    NN_ASSERT(c.surviving <= allocated && c.surviving_tracked <= tracked,
              "nn_region_close: more survives than the region allocated");
    dead[0] = allocated - c.surviving;
    for (g = 1; g < NN_REGION_GRAINS; g++) dead[g] += tracked - c.surviving_tracked;

    count_close(allocated, records);
    if (!parent) c.enclosed += allocated;
    simulate(f, parent, allocated, dead);
    pass_records(f, parent);
    c.closing = NULL;
    c.frame_count--;
}

/* --- Reading the figures --- */

void nn_region_totals(uint64_t *totals)
{
    uint64_t allocated = nn_region_census ? allocated_now() : 0;
    memcpy(totals, c.totals, sizeof(c.totals));
    totals[NN_REGION_TOTAL_ALLOCATED] = allocated;
    totals[NN_REGION_TOTAL_OUTSIDE] = allocated > c.enclosed ? allocated - c.enclosed : 0;
}

void nn_region_classes(uint64_t *figures)
{
    memcpy(figures, c.classes, sizeof(c.classes));
}

/* The bytes held now count toward each peak too: nothing has freed them
 * since the last close. */
void nn_region_thresholds(uint64_t *figures)
{
    uint64_t now = nn_region_census ? allocated_now() : 0;
    uint32_t k, g;
    memcpy(figures, c.thresholds, sizeof(c.thresholds));
    for (k = 0; k < NN_REGION_THRESHOLDS; k++) {
        for (g = 0; g < NN_REGION_GRAINS; g++) {
            uint64_t *peak = &figures[k * NN_REGION_THRESHOLD_FIGURES + NN_REGION_THRESHOLD_PEAK + g];
            if (now > c.freed[k][g] && now - c.freed[k][g] > *peak) *peak = now - c.freed[k][g];
        }
    }
}
