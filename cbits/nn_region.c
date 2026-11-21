/*
 * nn_region.c - Regions: the collector and the census (nn_region.h).
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
 * Each tenure collector marks with the same bits, after the census's own
 * mark at a close has retired its region, and keeps three of its own: a
 * unit or place it has settled, tenuring or freeing it, one it has freed,
 * and one a close has remembered for a region around, so that it is
 * remembered once.  Collecting, only the collector's are kept, and no
 * census mark runs.
 *
 * A block also keeps, per line, the number of the span that last handed
 * it out and whether the collector has freed it, and the collector keeps
 * each bump kind's blocks with free lines for refills.  A region's part
 * of a span is the runs of the span's lines that no later span has
 * taken: a line a refill hands out again is retired with its new span.
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

/* The collector's line, the census's finest: what a close frees and a
 * refill hands out at a time, and what a span number is kept for. */
#define NN_REGION_LINE ((uint64_t)NN_REGION_LINE_FIRST >> (NN_REGION_LINE_GRAINS - 1))

/* The blocks with free lines a refill looks at in vain before the
 * allocator makes a block, so that a refill costs a bounded scan however
 * fragmented the free lines are.  Any free line holds a request of a line
 * or less, so only a larger one looks in vain. */
#define NN_REGION_REFILL_BLOCKS 4u

/* Collecting, the span table drops the spans that hold no line once it
 * holds NN_REGION_SPAN_GROWTH times what it kept the last time it did,
 * and at least NN_REGION_SPAN_FLOOR, so that each drop's scan, at most a
 * span's lines for each span kept, is paid for by the spans begun since
 * the last. */
#define NN_REGION_SPAN_FLOOR  4096u
#define NN_REGION_SPAN_GROWTH 2u

/* Under AddressSanitizer a freed line is poisoned until a refill hands
 * it out again, so that a read of it fails where a pointer the collector
 * missed reads it. */
#if defined(__SANITIZE_ADDRESS__)
#define NN_REGION_ASAN 1
#elif defined(__has_feature)
#if __has_feature(address_sanitizer)
#define NN_REGION_ASAN 1
#endif
#endif
#ifdef NN_REGION_ASAN
#include <sanitizer/asan_interface.h>
#define NN_REGION_POISON(p, n)   ASAN_POISON_MEMORY_REGION((p), (n))
#define NN_REGION_UNPOISON(p, n) ASAN_UNPOISON_MEMORY_REGION((p), (n))
#else
#define NN_REGION_POISON(p, n)   ((void)(p), (void)(n))
#define NN_REGION_UNPOISON(p, n) ((void)(p), (void)(n))
#endif

/* The low bits of a record: one set for a with-scope entry, clear for a
 * cell; and in a tenure collector's records, one set once a close of its
 * has read the record.  Both are pointer-aligned, so the bits are
 * otherwise zero. */
#define NN_REGION_RECORD_WITH  ((uintptr_t)1)
#define NN_REGION_RECORD_READ  ((uintptr_t)2)
#define NN_REGION_RECORD_FLAGS (NN_REGION_RECORD_WITH | NN_REGION_RECORD_READ)

/* The first capacity of every table, which doubles from there.  A power
 * of two, as the address and object tables' probes need.  What a close
 * remembers for a frame starts smaller: a frame slot keeps its buffer, and
 * each depth and tenure collector has one. */
#define NN_REGION_INITIAL 1024u
#define NN_REGION_ENTRIES_INITIAL 16u

/* Each block grain's block is a quarter of the one before: this many
 * bits shorter. */
#define NN_REGION_GRAIN_SHIFT 2u

/* Multiplicative hashing's constant, 2^64 divided by the golden ratio. */
#define NN_REGION_HASH 0x9E3779B97F4A7C15ull

/* An env page's unit is a pointer, a record's flags fit in the low bits
 * a pointer's alignment leaves clear, and the thresholds fit in 64
 * bits. */
typedef char nn_region_env_unit_check_[(NN_ENV_ALIGNMENT == sizeof(void *)) ? 1 : -1];
typedef char nn_region_record_flags_check_[(sizeof(void *) > NN_REGION_RECORD_FLAGS) ? 1 : -1];
typedef char nn_region_threshold_check_[(NN_REGION_THRESHOLDS < 30) ? 1 : -1];

/* --- Types --- */

/* A tenure collector's bits, a bit per unit or place: `tenured` for one
 * no mark of its enters again but a full one, tenured or freed. */
typedef struct {
    uint8_t *tenured;
    uint8_t *freed;
    uint8_t *remembered;
} tenure_bits_t;

typedef struct {
    uintptr_t     start;   /* the block's first byte of data */
    uintptr_t     end;     /* one past its capacity */
    uint8_t      *marks;   /* a bit per unit: reached by the close under way */
    uint8_t      *died;    /* a bit per unit: found unreached by a close */
    tenure_bits_t tenure[NN_REGION_TENURE_VARIANTS];
    uint32_t     *spans;   /* a span number per line: the room that last handed it out */
    uint8_t      *free;    /* a bit per line: freed by the collector and not handed out since */
    uint32_t      lines;   /* whole lines its capacity holds */
    uint32_t      free_count;
    uint32_t      hint;    /* no line below it is free */
    int           recycling; /* whether it is on its kind's list of blocks with free lines */
    int           kind;
} block_t;

/* A room an allocator bumped from, a new block or lines a refill handed
 * out, numbered in the order the rooms started: from `start` to below
 * `limit` bytes into its block, and bumped up to `end` once it ended. */
typedef struct {
    uint32_t number;
    uint32_t block;
    uint64_t start;
    uint64_t limit;
    uint64_t end;
} span_t;

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
    uint8_t      *marks;
    uint8_t      *died;
    tenure_bits_t tenure[NN_REGION_TENURE_VARIANTS];
    uint32_t      capacity; /* places the bits have room for */
} places_t;

/* What a close remembered for a region around it: an object of an
 * NN_GRAPH object kind, a string, or an array of an NN_GRAPH array kind
 * with its owner and length; and where its remembered bit is, a unit of
 * a block or a place of a tracked kind. */
typedef enum { ENTRY_OBJECT, ENTRY_STRING, ENTRY_ARRAY } entry_what_t;

typedef struct {
    const void  *target;
    const void  *owner;
    uint64_t     index;    /* the unit or place */
    uint32_t     block;    /* NN_REGION_NONE for a place */
    uint32_t     count;
    int          kind;     /* the NN_GRAPH object or array kind */
    int          tracked;  /* a place's tracked kind */
    entry_what_t what;
} entry_t;

/* What a tenure collector does at a close: nothing, below its threshold;
 * mark what no close of its inside tenured; mark the whole region, where
 * what closes inside tenured since it was last marked whole passes its
 * budget; or, at an outermost close, take the census's own mark of the
 * whole region. */
typedef enum { TENURE_NONE, TENURE_YOUNG, TENURE_FULL, TENURE_OUTERMOST } tenure_action_t;

/* An open region's share of a tenure collector: where its records start;
 * the bytes in it that closes inside tenured or freed, and of them
 * tracked objects'; the bytes they freed at each grain, and of them
 * tracked objects'; the bytes they tenured that no mark of the whole has
 * reached since; and what closes inside remembered for it. */
typedef struct {
    uint32_t records;
    uint64_t unchecked;
    uint64_t tenured;
    uint64_t tenured_tracked;
    uint64_t freed[NN_REGION_GRAINS];
    uint64_t freed_tracked;
    entry_t *entries;
    uint32_t entry_count;
    uint32_t entry_cap;
} frame_tenure_t;

/* A tenure collector's records, which a close it does not mark at passes
 * on more of than the census's own; its figures; the bytes it has freed
 * at each grain; and the entries remembered and not yet read. */
typedef struct {
    uintptr_t *records;
    uint32_t   record_count;
    uint32_t   record_cap;
    uint64_t   figures[NN_REGION_TENURE_FIGURES];
    uint64_t   freed[NN_REGION_GRAINS];
    uint64_t   held;
} tenure_t;

/* An open region: what each allocator had handed out as it opened. */
typedef struct {
    uint32_t spans;                                  /* the number the first span after it opened takes */
    uint32_t current_span[NN_REGION_BUMP_KINDS];     /* the span each bump kind bumped from */
    uint64_t current_used[NN_REGION_BUMP_KINDS];     /* and how far into its block it had bumped */
    uint32_t tracked[NN_REGION_TRACKED_KINDS];       /* each tracker's count */
    uint64_t tracked_bytes;
    uint64_t allocated;
    uint32_t records;
    uint32_t root_thunks;
    uint32_t root_envs;
    /* Bytes the simulated collector freed inside this region, at closes
     * of the regions inside it, under each threshold at each grain. */
    uint64_t freed[NN_REGION_THRESHOLDS][NN_REGION_GRAINS];
    frame_tenure_t t[NN_REGION_TENURE_VARIANTS];
} frame_t;

/* The entries of an items array before the ones a list spans, from unit
 * `from` to below `to` of a block: kept with the array, and marked once
 * the mark has followed everything, so that no visit of a list over the
 * whole array finds them marked and passes over what they point at. */
typedef struct {
    uint64_t from;
    uint64_t to;
    uint32_t block;
} head_t;

/* An object a mark pushed, and for a tenure collector's whether the close
 * tenures it, so that the pointers it holds are remembered. */
typedef struct {
    const void *object;
    int         kind;
    int         remember;
} work_t;

/* Whether an object is younger than a region, older, or in no block the
 * census numbers. */
typedef enum { AGE_YOUNGER, AGE_OLDER, AGE_OUTSIDE } age_t;

/* --- State --- */

int nn_region_mode = NN_REGION_OFF;

static struct {
    int failed;

    block_t    *blocks;
    uint32_t    block_count;
    uint32_t    block_cap;
    uint32_t    last;               /* the block the last lookup found */

    /* The spans an open region's part takes in, oldest first, and each
     * bump kind's current one: every span begun since the last close of
     * an outermost region (spans_trim) that may still hold a line
     * (spans_drop). */
    span_t     *spans;
    uint32_t    span_count;
    uint32_t    span_cap;
    uint32_t    span_next;          /* the number the next span takes */
    uint32_t    span_drop_at;       /* the count at which spans_drop runs */
    uint32_t    span_most;          /* the most it has held since the last trim */
    uint32_t    span_most_ended;    /* the most it held before the last trim */
    uint32_t    current_span[NN_REGION_BUMP_KINDS];

    /* Each bump kind's blocks with free lines, and where its next refill
     * looks first. */
    uint32_t   *recycle[NN_REGION_BUMP_KINDS];
    uint32_t    recycle_count[NN_REGION_BUMP_KINDS];
    uint32_t    recycle_cap[NN_REGION_BUMP_KINDS];
    uint32_t    recycle_at[NN_REGION_BUMP_KINDS];

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

    head_t   *heads;
    uint32_t  head_count;
    uint32_t  head_cap;

    uint8_t  *live;                 /* scratch: a flag per finest line */
    uint64_t  live_cap;

    /* The close under way; the tenure collector whose mark it is, or -1
     * for the census's own, and whether that mark is of the whole region;
     * and whether the pointers the mark finds where no later close looks
     * are remembered: those in an object it enters or a cell a record
     * names, and not those of a root or of what was remembered before. */
    const frame_t *closing;
    uint64_t surviving;
    uint64_t surviving_tracked;
    uint64_t entered;
    uint64_t examined;
    int      tv;
    int      full;
    int      remembering;

    uint64_t enclosed;              /* bytes the outermost regions allocated */
    uint64_t totals[NN_REGION_TOTALS];
    uint64_t classes[NN_REGION_SIZE_CLASSES][NN_REGION_CLASS_FIGURES];
    uint64_t thresholds[NN_REGION_THRESHOLDS][NN_REGION_THRESHOLD_FIGURES];
    uint64_t freed[NN_REGION_THRESHOLDS][NN_REGION_GRAINS];

    tenure_t t[NN_REGION_TENURE_VARIANTS];
} c;

/* The bytes each tenure collector lets closes inside a region tenure
 * before a close marks the region whole, or 0 for none but the outermost.
 * The collector is the one with NN_REGION_COLLECTOR_BUDGET. */
#define NN_REGION_MIB ((uint64_t)1 << 20)
#define NN_REGION_COLLECTOR        1
#define NN_REGION_COLLECTOR_BUDGET NN_REGION_MIB
static const uint64_t tenure_budgets[NN_REGION_TENURE_VARIANTS] = {
    0, NN_REGION_COLLECTOR_BUDGET, 16 * NN_REGION_MIB, 256 * NN_REGION_MIB
};

/* Whether regions run for the collector, and whether tenure collector v
 * runs: every one under a census, only the collector collecting. */
static int collecting(void)
{
    return nn_region_mode == NN_REGION_COLLECT;
}

static int active(int v)
{
    return !collecting() || v == NN_REGION_COLLECTOR;
}

static void fail(void)
{
    c.failed = 1;
    c.totals[NN_REGION_TOTAL_FAILED] = 1;
}

/* An array of `size`-byte elements with room for `need`, doubling its
 * capacity *cap from `initial` as it grows: the array, moved or not, or
 * NULL with the array and *cap unchanged. */
static void *reserve_from(void *array, uint32_t *cap, size_t size, uint32_t need, uint32_t initial)
{
    uint32_t grown = *cap ? *cap : initial;
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

static void *reserve(void *array, uint32_t *cap, size_t size, uint32_t need)
{
    return reserve_from(array, cap, size, need, NN_REGION_INITIAL);
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

static void bits_clear(uint8_t *bits, uint64_t from, uint64_t to)
{
    uint64_t i;
    for (i = from; i < to; i++) bits[i / 8] &= (uint8_t)~(1u << (i % 8));
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

/* Whether every bit from `from` to below `to` is set. */
static int bits_all(const uint8_t *bits, uint64_t from, uint64_t to)
{
    uint64_t i = from;
    for (; i < to && i % 8 != 0; i++)
        if (!bit_get(bits, i)) return 0;
    for (; i + 8 <= to; i += 8)
        if (bits[i / 8] != 0xFFu) return 0;
    for (; i < to; i++)
        if (!bit_get(bits, i)) return 0;
    return 1;
}

static uint32_t ones(uint8_t byte)
{
    uint32_t n = 0;
    for (; byte; byte &= (uint8_t)(byte - 1)) n++;
    return n;
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

/* The bits of eight units or places from `j` * 8 at a tenure collector's
 * retirement (bits_tenure), `mask` saying which of them it retires: each
 * its mark left unmarked is freed, if it was untenured, or for a mark of
 * the whole region if it was not freed before; every one is tenured, and
 * every mark cleared.  Returns the ones it freed. */
static uint8_t byte_tenure(uint8_t *marks, const tenure_bits_t *t, int full, uint64_t j, uint8_t mask)
{
    uint8_t open = (uint8_t)(~(full ? t->freed[j] : t->tenured[j]) & mask);
    uint8_t lost = (uint8_t)(open & ~marks[j]);
    t->freed[j] |= lost;
    t->tenured[j] |= mask;
    marks[j] &= (uint8_t)~mask;
    return lost;
}

/* A tenure collector's retirement of the units or places from `from` to
 * below `to` at a close where it marked (byte_tenure), counting in
 * *reached each it freed that the census's own mark at the close reached,
 * which it did if the census, retiring the region first, left its died
 * bit clear. */
static void bits_tenure(uint8_t *marks, const uint8_t *died, const tenure_bits_t *t, int full, uint64_t from,
                        uint64_t to, uint64_t *reached)
{
    uint64_t i = from;
    for (; i < to && i % 8 != 0; i++) {
        uint8_t mask = (uint8_t)(1u << (i % 8));
        *reached += ones((uint8_t)(byte_tenure(marks, t, full, i / 8, mask) & ~died[i / 8]));
    }
    for (; i + 8 <= to; i += 8) *reached += ones((uint8_t)(byte_tenure(marks, t, full, i / 8, 0xFFu) & ~died[i / 8]));
    for (; i < to; i++) {
        uint8_t mask = (uint8_t)(1u << (i % 8));
        *reached += ones((uint8_t)(byte_tenure(marks, t, full, i / 8, mask) & ~died[i / 8]));
    }
}

/* A tenure collector's retirement at an outermost close, from the
 * census's own mark of the whole region before the census retires it:
 * each unit or place the mark left unmarked is freed, and every one is
 * tenured. */
static void bits_tenure_full(const uint8_t *marks, const tenure_bits_t *t, uint64_t from, uint64_t to)
{
    uint64_t i = from;
    for (; i < to && i % 8 != 0; i++) {
        if (!bit_get(marks, i)) bit_set(t->freed, i);
        bit_set(t->tenured, i);
    }
    for (; i + 8 <= to; i += 8) {
        t->freed[i / 8] |= (uint8_t)~marks[i / 8];
        t->tenured[i / 8] = 0xFFu;
    }
    for (; i < to; i++) {
        if (!bit_get(marks, i)) bit_set(t->freed, i);
        bit_set(t->tenured, i);
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

/* A grain's liveness is read from the finest line's (dead_grains), so
 * every grain is a whole number of the next: each kind's unit divides the
 * finest line, each line is twice the next, and each kind's block is a
 * whole number of the longest line times the block grains' quarterings,
 * which makes every block grain exact and its smallest a whole number of
 * the longest line. */
#define NN_REGION_BLOCK_MULTIPLE \
    ((uint64_t)NN_REGION_LINE_FIRST << (NN_REGION_GRAIN_SHIFT * (NN_REGION_BLOCK_GRAINS - 1)))
typedef char nn_region_line_halves_check_[(NN_REGION_LINE_FIRST % (1u << (NN_REGION_LINE_GRAINS - 1)) == 0) ? 1 : -1];
typedef char nn_region_thunk_line_check_[(NN_REGION_LINE % sizeof(nn_thunk_t) == 0) ? 1 : -1];
typedef char nn_region_env_line_check_[(NN_REGION_LINE % NN_ENV_ALIGNMENT == 0) ? 1 : -1];
typedef char nn_region_str_line_check_[(NN_REGION_LINE % NN_STR_ALIGNMENT == 0) ? 1 : -1];
typedef char nn_region_thunk_grain_check_[
    (((uint64_t)NN_THUNK_BLOCK_CELLS * sizeof(nn_thunk_t)) % NN_REGION_BLOCK_MULTIPLE == 0) ? 1 : -1];
typedef char nn_region_env_grain_check_[(NN_ENV_PAGE_SIZE % NN_REGION_BLOCK_MULTIPLE == 0) ? 1 : -1];
typedef char nn_region_str_grain_check_[(NN_STR_BLOCK_BYTES % NN_REGION_BLOCK_MULTIPLE == 0) ? 1 : -1];

uint64_t nn_region_grain_bytes(int kind, uint32_t grain)
{
    if (grain == 0 || grain >= NN_REGION_GRAINS) return 0;
    if (grain <= NN_REGION_BLOCK_GRAINS) return block_bytes(kind) >> (NN_REGION_GRAIN_SHIFT * (grain - 1));
    return (uint64_t)NN_REGION_LINE_FIRST >> (grain - 1 - NN_REGION_BLOCK_GRAINS);
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

/* Take a tracked object out of the table, moving back each slot after it
 * that its probe passed through it to reach. */
static void object_remove(uintptr_t object)
{
    uint32_t cap = c.object_cap;
    uint32_t hole = (uint32_t)(object_slot(c.objects, cap, object) - c.objects), next = hole;
    if (c.objects[hole].object == 0) return;
    for (;;) {
        uint32_t home;
        next = (next + 1) & (cap - 1);
        if (c.objects[next].object == 0) break;
        home = hash((uint64_t)c.objects[next].object >> 4, cap);
        /* A slot whose probe starts after the hole and at or before it
         * never passed through the hole. */
        if (hole <= next ? (hole < home && home <= next) : (hole < home || home <= next)) continue;
        c.objects[hole] = c.objects[next];
        hole = next;
    }
    c.objects[hole].object = 0;
    c.object_used--;
}

/* Grow an array of bits from `old_bytes` to `bytes`, the new ones clear. */
static int bits_grow(uint8_t **bits, size_t old_bytes, size_t bytes)
{
    uint8_t *grown = (uint8_t *)realloc(*bits, bytes);
    if (!grown) return -1;
    memset(grown + old_bytes, 0, bytes - old_bytes);
    *bits = grown;
    return 0;
}

/* Room in a tracked kind's bits for `need` places. */
static int places_reserve(places_t *places, uint32_t need)
{
    uint32_t cap = places->capacity ? places->capacity : NN_REGION_INITIAL;
    /* A first allocation has no old bytes to keep. */
    size_t old_bytes = places->capacity ? (size_t)places->capacity / 8 + 1 : 0, bytes;
    int v;
    if (need <= places->capacity) return 0;
    while (cap < need) {
        if (cap > UINT32_MAX / 2) return -1;
        cap *= 2;
    }
    bytes = (size_t)cap / 8 + 1;
    if (bits_grow(&places->marks, old_bytes, bytes) != 0 || bits_grow(&places->died, old_bytes, bytes) != 0)
        return -1;
    for (v = 0; v < NN_REGION_TENURE_VARIANTS; v++) {
        tenure_bits_t *t = &places->tenure[v];
        if (!active(v)) continue;
        if (bits_grow(&t->tenured, old_bytes, bytes) != 0 || bits_grow(&t->freed, old_bytes, bytes) != 0
            || bits_grow(&t->remembered, old_bytes, bytes) != 0)
            return -1;
    }
    places->capacity = cap;
    return 0;
}

/* --- Spans and lines --- */

/* Where the allocator of a bump kind bumps from next, in bytes into its
 * current block. */
static uint64_t cursor_of(int kind)
{
    switch (kind) {
    case NN_REGION_THUNK_BLOCK: return nn_thunk_cursor();
    case NN_REGION_ENV_PAGE:    return nn_env_cursor();
    default:                    return nn_str_cursor();
    }
}

/* The span numbered n, or NULL when it is no longer kept. */
static span_t *span_numbered(uint32_t n)
{
    uint32_t lo = 0, hi = c.span_count;
    while (lo < hi) {
        uint32_t mid = lo + (hi - lo) / 2;
        if (c.spans[mid].number < n)
            lo = mid + 1;
        else
            hi = mid;
    }
    return lo < c.span_count && c.spans[lo].number == n ? &c.spans[lo] : NULL;
}

/* The first kept span numbered n or after. */
static uint32_t span_index_from(uint32_t n)
{
    uint32_t lo = 0, hi = c.span_count;
    while (lo < hi) {
        uint32_t mid = lo + (hi - lo) / 2;
        if (c.spans[mid].number < n)
            lo = mid + 1;
        else
            hi = mid;
    }
    return lo;
}

/* Whether a span is the room its kind bumps from now. */
static int span_current(const span_t *s)
{
    return s->number == c.current_span[c.blocks[s->block].kind];
}

/* How far a span has bumped: to the allocator's cursor while it is
 * current, to where it ended otherwise. */
static uint64_t span_end(const span_t *s)
{
    return span_current(s) ? cursor_of(c.blocks[s->block].kind) : s->end;
}

/* Below which offset a span's lines take no more objects: the line the
 * cursor is in still does while the span is current; an ended span's
 * last line is settled, the rest of it unallocated, unless it runs past
 * the block's last whole line. */
static uint64_t span_settled(const span_t *s)
{
    const block_t *blk = &c.blocks[s->block];
    uint64_t whole = (uint64_t)blk->lines * NN_REGION_LINE;
    uint64_t settled;
    if (span_current(s)) return cursor_of(blk->kind) / NN_REGION_LINE * NN_REGION_LINE;
    settled = (s->end + NN_REGION_LINE - 1) / NN_REGION_LINE * NN_REGION_LINE;
    return settled < whole ? settled : whole;
}

/* Put a block on its kind's list of blocks with free lines. */
static void recycle(block_t *blk)
{
    int kind = blk->kind;
    uint32_t *list;
    if (blk->recycling) return;
    list = (uint32_t *)reserve_from(c.recycle[kind], &c.recycle_cap[kind], sizeof(uint32_t),
                                    c.recycle_count[kind] + 1, NN_REGION_ENTRIES_INITIAL);
    if (!list) {
        fail();
        return;
    }
    c.recycle[kind] = list;
    c.recycle[kind][c.recycle_count[kind]++] = (uint32_t)(blk - c.blocks);
    blk->recycling = 1;
}

/* Free a line of a block for a refill to hand out. */
static void free_line(block_t *blk, uint64_t line)
{
    if (bit_get(blk->free, line)) return;
    bit_set(blk->free, line);
    blk->free_count++;
    if (line < blk->hint) blk->hint = (uint32_t)line;
    NN_REGION_POISON((void *)(blk->start + line * NN_REGION_LINE), NN_REGION_LINE);
    recycle(blk);
}

/* End a bump kind's current span where its allocator stopped.
 * Collecting, the whole lines past that it never bumped into are free
 * again. */
static void span_stop(int kind, uint64_t cursor)
{
    span_t *s;
    block_t *blk;
    uint64_t line;
    if (c.current_span[kind] == NN_REGION_NONE) return;
    s = span_numbered(c.current_span[kind]);
    c.current_span[kind] = NN_REGION_NONE;
    if (!s) return;
    s->end = cursor;
    if (!collecting()) return;
    blk = &c.blocks[s->block];
    for (line = (cursor + NN_REGION_LINE - 1) / NN_REGION_LINE; line < s->limit / NN_REGION_LINE; line++)
        free_line(blk, line);
    /* The line the cursor stopped in takes no more objects now, and a
     * close may have freed every one it holds while it still could. */
    line = cursor / NN_REGION_LINE;
    if (cursor % NN_REGION_LINE != 0 && line * NN_REGION_LINE >= s->start && line < blk->lines
        && blk->spans[line] == s->number
        && bits_all(blk->tenure[NN_REGION_COLLECTOR].freed, line * NN_REGION_LINE / unit_bytes(blk->kind),
                    cursor / unit_bytes(blk->kind)))
        free_line(blk, line);
}

/* Whether a span holds a line: one that carries its number and is not
 * free, or the part of a line past its block's last whole one, which is
 * never freed.  A span that holds none gives a close nothing to retire,
 * its lines being free or another span's. */
static int span_holds(const span_t *s)
{
    const block_t *blk = &c.blocks[s->block];
    uint64_t line, last = (s->limit + NN_REGION_LINE - 1) / NN_REGION_LINE;
    for (line = s->start / NN_REGION_LINE; line < last; line++)
        if (blk->spans[line] == s->number && (line >= blk->lines || !bit_get(blk->free, line))) return 1;
    return 0;
}

/* Drop from the table every span but the current ones that holds no
 * line, in order, and run again once it holds NN_REGION_SPAN_GROWTH
 * times what it keeps.  It runs only as a span begins, where no pointer
 * into the table is held. */
static void spans_drop(void)
{
    uint32_t i, kept = 0;
    uint64_t next;
    for (i = 0; i < c.span_count; i++)
        if (span_current(&c.spans[i]) || span_holds(&c.spans[i])) c.spans[kept++] = c.spans[i];
    c.span_count = kept;
    next = (uint64_t)kept * NN_REGION_SPAN_GROWTH;
    c.span_drop_at = next < NN_REGION_SPAN_FLOOR ? NN_REGION_SPAN_FLOOR : next > UINT32_MAX ? UINT32_MAX : (uint32_t)next;
}

/* Start a span of block b from `start` to below `limit` bytes in, as its
 * kind's current one when `current`, the previous one ended first; one
 * that is not current is a block's single object, ended at its limit.
 * Returns 0 or -1. */
static int span_begin(uint32_t b, uint64_t start, uint64_t limit, int current)
{
    block_t *blk = &c.blocks[b];
    span_t *spans, *span;
    uint64_t line, last;
    if (current) span_stop(blk->kind, cursor_of(blk->kind));
    if (collecting() && c.span_count >= c.span_drop_at) spans_drop();
    spans = (span_t *)reserve(c.spans, &c.span_cap, sizeof(span_t), c.span_count + 1);
    if (!spans || c.span_next == NN_REGION_NONE) return -1;
    c.spans = spans;
    span = &c.spans[c.span_count++];
    if (c.span_count > c.span_most) c.span_most = c.span_count;
    span->number = c.span_next++;
    span->block = b;
    span->start = start;
    span->limit = limit;
    span->end = current ? start : limit;
    last = (limit + NN_REGION_LINE - 1) / NN_REGION_LINE;
    for (line = start / NN_REGION_LINE; line < last; line++) blk->spans[line] = span->number;
    if (current) c.current_span[blk->kind] = span->number;
    return 0;
}

/* The first run of a block's free lines at least `lines` long: its first
 * line and length, or 0 for none.  The scan starts at the block's hint
 * and moves it up to the first free line it meets. */
static int free_run(block_t *blk, uint64_t lines, uint64_t *first, uint64_t *count)
{
    uint64_t line = blk->hint;
    int met = 0;
    while (line < blk->lines) {
        uint64_t end;
        if (line % 8 == 0 && blk->free[line / 8] == 0) {
            line += 8;
            continue;
        }
        if (!bit_get(blk->free, line)) {
            line++;
            continue;
        }
        if (!met) {
            blk->hint = (uint32_t)line;
            met = 1;
        }
        for (end = line; end < blk->lines && bit_get(blk->free, end); end++) {}
        if (end - line >= lines) {
            *first = line;
            *count = end - line;
            return 1;
        }
        line = end;
    }
    if (!met) blk->hint = blk->lines;
    return 0;
}

/* Hand out a block's free lines from `first` for `count` lines: cleared
 * of every bit a close left on their units, for objects younger than
 * every open region. */
static void hand_out(block_t *blk, uint64_t first, uint64_t count)
{
    uint64_t unit = unit_bytes(blk->kind);
    uint64_t from = first * NN_REGION_LINE / unit, to = (first + count) * NN_REGION_LINE / unit;
    tenure_bits_t *t = &blk->tenure[NN_REGION_COLLECTOR];
    NN_ASSERT(!bits_any(t->remembered, from, to), "nn_region_refill: a free line holds a remembered unit");
    NN_ASSERT(!bits_any(blk->marks, from, to), "nn_region_refill: a free line holds a marked unit");
    bits_clear(t->tenured, from, to);
    bits_clear(t->freed, from, to);
    bits_clear(blk->died, from, to);
    bits_clear(blk->free, first, first + count);
    blk->free_count -= (uint32_t)count;
    if (first == blk->hint) blk->hint = (uint32_t)(first + count);
    NN_REGION_UNPOISON((void *)(blk->start + first * NN_REGION_LINE), count * NN_REGION_LINE);
}

int nn_region_refill(int kind, uint64_t cursor, uint64_t need, const char **data, uint64_t *start,
                     uint64_t *limit)
{
    uint64_t lines = (need + NN_REGION_LINE - 1) / NN_REGION_LINE, first, count;
    uint32_t looked = 0;
    if (!collecting() || c.failed) return -1;
    span_stop(kind, cursor);
    while (c.recycle_count[kind] > 0 && looked < NN_REGION_REFILL_BLOCKS && !c.failed) {
        uint32_t at = c.recycle_at[kind] % c.recycle_count[kind];
        uint32_t b = c.recycle[kind][at];
        block_t *blk = &c.blocks[b];
        if (blk->free_count == 0) {
            c.recycle[kind][at] = c.recycle[kind][--c.recycle_count[kind]];
            blk->recycling = 0;
            continue;
        }
        if (free_run(blk, lines, &first, &count)) {
            hand_out(blk, first, count);
            if (span_begin(b, first * NN_REGION_LINE, (first + count) * NN_REGION_LINE, 1) != 0) {
                fail();
                return -1;
            }
            *data = (const char *)blk->start;
            *start = first * NN_REGION_LINE;
            *limit = (first + count) * NN_REGION_LINE;
            return 0;
        }
        c.recycle_at[kind] = at + 1;
        looked++;
    }
    return -1;
}

/* As an outermost region closes no part takes a span in, so only the
 * current ones are kept, spans_drop runs again from its floor, and the
 * stretch since the last trim ends with the most spans it held. */
static void spans_trim(void)
{
    uint32_t i, kept = 0;
    for (i = 0; i < c.span_count; i++)
        if (span_current(&c.spans[i])) c.spans[kept++] = c.spans[i];
    c.span_count = kept;
    c.span_drop_at = NN_REGION_SPAN_FLOOR;
    c.span_most_ended = c.span_most;
    c.span_most = kept;
}

/* --- Lifecycle --- */

static void tenure_bits_free(tenure_bits_t *t)
{
    int v;
    for (v = 0; v < NN_REGION_TENURE_VARIANTS; v++) {
        free(t[v].tenured);
        free(t[v].freed);
        free(t[v].remembered);
    }
}

static void release(void)
{
    uint32_t i;
    int v;
    for (i = 0; i < c.block_count; i++) {
        free(c.blocks[i].marks);
        free(c.blocks[i].died);
        free(c.blocks[i].spans);
        free(c.blocks[i].free);
        tenure_bits_free(c.blocks[i].tenure);
    }
    for (i = 0; i < NN_REGION_BUMP_KINDS; i++) free(c.recycle[i]);
    free(c.spans);
    for (i = 0; i < NN_REGION_TRACKED_KINDS; i++) {
        free(c.places[i].marks);
        free(c.places[i].died);
        tenure_bits_free(c.places[i].tenure);
    }
    for (i = 0; i < c.frame_cap; i++)
        for (v = 0; v < NN_REGION_TENURE_VARIANTS; v++) free(c.frames[i].t[v].entries);
    for (v = 0; v < NN_REGION_TENURE_VARIANTS; v++) free(c.t[v].records);
    free(c.blocks);
    free(c.units);
    free(c.objects);
    free(c.records);
    free(c.frames);
    free(c.work);
    free(c.heads);
    free(c.live);
    memset(&c, 0, sizeof(c));
    nn_region_mode = NN_REGION_OFF;
}

int nn_region_begin(int mode)
{
    int k;
    NN_ASSERT(!nn_thunk_live() && !nn_env_live() && !nn_str_live(),
              "nn_region_begin: the arena is live");
    NN_ASSERT(mode == NN_REGION_CENSUS || mode == NN_REGION_COLLECT, "nn_region_begin: no such mode");
    release();
    /* The bits a block or a tracked kind gets depend on the mode. */
    nn_region_mode = mode;
    c.units = units_new(NN_REGION_INITIAL);
    c.objects = (object_slot_t *)calloc(NN_REGION_INITIAL, sizeof(object_slot_t));
    if (!c.units || !c.objects) {
        release();
        return -1;
    }
    c.unit_cap = NN_REGION_INITIAL;
    c.object_cap = NN_REGION_INITIAL;
    for (k = 0; k < NN_REGION_BUMP_KINDS; k++) c.current_span[k] = NN_REGION_NONE;
    c.span_drop_at = NN_REGION_SPAN_FLOOR;
    c.last = NN_REGION_NONE;
    c.tv = -1;
    return 0;
}

void nn_region_end(void)
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
    memset(b->tenure, 0, sizeof(b->tenure));
    b->spans = NULL;
    b->free = NULL;
    b->lines = (uint32_t)(capacity / NN_REGION_LINE);
    b->free_count = 0;
    b->hint = b->lines;
    b->recycling = 0;
    c.block_count++;
    /* Nothing in a block of code is ever marked. */
    if (kind != NN_REGION_CODE_BLOCK) {
        uint64_t units = capacity / unit_bytes(kind);
        int v;
        b->marks = bits_new(units);
        b->died = bits_new(units);
        b->spans = (uint32_t *)malloc((size_t)((capacity + NN_REGION_LINE - 1) / NN_REGION_LINE) * sizeof(uint32_t));
        b->free = bits_new(b->lines);
        if (!b->marks || !b->died || !b->spans || !b->free) {
            fail();
            return;
        }
        for (v = 0; v < NN_REGION_TENURE_VARIANTS; v++) {
            tenure_bits_t *t = &b->tenure[v];
            if (!active(v)) continue;
            t->tenured = bits_new(units);
            t->freed = bits_new(units);
            t->remembered = bits_new(units);
            if (!t->tenured || !t->freed || !t->remembered) {
                fail();
                return;
            }
        }
    }
    last = (b->end - 1) >> NN_REGION_UNIT_SHIFT;
    for (unit = b->start >> NN_REGION_UNIT_SHIFT; unit <= last; unit++) {
        if (unit_add(unit, index) != 0) {
            fail();
            return;
        }
    }
    if (kind < NN_REGION_BUMP_KINDS && span_begin(index, 0, capacity, current) != 0) fail();
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
    int v;
    if (c.failed || c.frame_count == 0) return;
    if (!collecting()) {
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
    for (v = 0; v < NN_REGION_TENURE_VARIANTS; v++) {
        tenure_t *t = &c.t[v];
        if (!active(v)) continue;
        records = (uintptr_t *)reserve(t->records, &t->record_cap, sizeof(uintptr_t), t->record_count + 1);
        if (!records) {
            fail();
            return;
        }
        t->records = records;
        t->records[t->record_count++] = r;
        if (t->record_count > t->figures[NN_REGION_TENURE_RECORDS]) t->figures[NN_REGION_TENURE_RECORDS] = t->record_count;
    }
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

/* An object is younger than a region when the span its line was last
 * handed out in started after the region opened, or is the one its kind
 * bumped from as the region opened and the object lies at or past where
 * the cursor was then.  A refill hands out lines in a span of their own,
 * so what it puts in a line an older object left is younger than every
 * region open. */
static age_t bump_age(const frame_t *f, uint32_t b, uintptr_t p)
{
    const block_t *blk = &c.blocks[b];
    uint64_t offset = (uint64_t)(p - blk->start);
    uint32_t n = blk->spans[offset / NN_REGION_LINE];
    if (n >= f->spans) return AGE_YOUNGER;
    if (n == f->current_span[blk->kind] && offset >= f->current_used[blk->kind]) return AGE_YOUNGER;
    return AGE_OLDER;
}

/* The age of what a record names: a cell, or a with-scope entry in an
 * env page. */
static age_t record_age(const frame_t *f, uintptr_t r)
{
    int with = (r & NN_REGION_RECORD_WITH) != 0;
    uintptr_t p = r & ~NN_REGION_RECORD_FLAGS;
    uint32_t b = block_of(p, with ? NN_REGION_ENV_PAGE : NN_REGION_THUNK_BLOCK);
    if (b == NN_REGION_NONE) return AGE_OUTSIDE;
    return bump_age(f, b, p);
}

/* The block and unit of what a record names, or NN_REGION_NONE. */
static uint32_t record_unit(uintptr_t r, uint64_t *unit)
{
    int with = (r & NN_REGION_RECORD_WITH) != 0;
    uintptr_t p = r & ~NN_REGION_RECORD_FLAGS;
    uint32_t b = block_of(p, with ? NN_REGION_ENV_PAGE : NN_REGION_THUNK_BLOCK);
    if (b != NN_REGION_NONE) *unit = (uint64_t)(p - c.blocks[b].start) / unit_bytes(c.blocks[b].kind);
    return b;
}

/* --- What the tenure collector remembers --- */

static void bit_clear(uint8_t *bits, uint64_t i)
{
    bits[i / 8] &= (uint8_t)~(1u << (i % 8));
}

/* The innermost region around the closing one that an object of a block
 * or a tracked place is younger than, or -1 for none.  Each region opened
 * after the one around it, so an object younger than a region is younger
 * than every region around it too. */
static int32_t holder_bump(uint32_t b, uintptr_t p)
{
    int32_t lo = 0, hi = (int32_t)c.frame_count - 2, found = -1;
    while (lo <= hi) {
        int32_t mid = lo + (hi - lo) / 2;
        if (bump_age(&c.frames[mid], b, p) == AGE_YOUNGER) {
            found = mid;
            lo = mid + 1;
        } else {
            hi = mid - 1;
        }
    }
    return found;
}

static int32_t holder_place(int tracked, uint32_t position)
{
    int32_t lo = 0, hi = (int32_t)c.frame_count - 2, found = -1;
    while (lo <= hi) {
        int32_t mid = lo + (hi - lo) / 2;
        if (position >= c.frames[mid].tracked[tracked]) {
            found = mid;
            lo = mid + 1;
        } else {
            hi = mid - 1;
        }
    }
    return found;
}

/* Remember an entry for the region at frame `at`, for the tenure
 * collector whose mark is under way, once: `remembered` is the bits its
 * index is in. */
static void remember(int32_t at, const entry_t *entry, uint8_t *remembered)
{
    frame_tenure_t *ft = &c.frames[at].t[c.tv];
    tenure_t *t = &c.t[c.tv];
    entry_t *entries;
    if (bit_get(remembered, entry->index)) return;
    entries = (entry_t *)reserve_from(ft->entries, &ft->entry_cap, sizeof(entry_t), ft->entry_count + 1,
                                      NN_REGION_ENTRIES_INITIAL);
    if (!entries) {
        fail();
        return;
    }
    ft->entries = entries;
    ft->entries[ft->entry_count++] = *entry;
    bit_set(remembered, entry->index);
    t->figures[NN_REGION_TENURE_REMEMBERED]++;
    if (++t->held > t->figures[NN_REGION_TENURE_HELD]) t->figures[NN_REGION_TENURE_HELD] = t->held;
}

static void remember_unit(int32_t at, entry_what_t what, int kind, const void *target, const void *owner,
                          uint32_t count, uint32_t b, uint64_t u)
{
    entry_t entry;
    entry.target = target;
    entry.owner = owner;
    entry.index = u;
    entry.block = b;
    entry.count = count;
    entry.kind = kind;
    entry.tracked = 0;
    entry.what = what;
    remember(at, &entry, c.blocks[b].tenure[c.tv].remembered);
}

static void remember_place(int32_t at, int kind, int tracked, const void *target, uint32_t position)
{
    entry_t entry;
    entry.target = target;
    entry.owner = NULL;
    entry.index = position;
    entry.block = NN_REGION_NONE;
    entry.count = 0;
    entry.kind = kind;
    entry.tracked = tracked;
    entry.what = ENTRY_OBJECT;
    remember(at, &entry, c.places[tracked].tenure[c.tv].remembered);
}

/* A pointer a tenure collector's mark found, in an object it entered or
 * a cell a record names, to a unit of a block older than the closing
 * region: remembered for the innermost region around that the unit is
 * younger than, unless it is tenured.  No later close but a mark of the
 * whole looks inside the object, which this close tenures, or reads the
 * record, which it drops, so no later close finds the pointer otherwise.
 * A tenured unit stays tenured in every region around, and needs none. */
static void boundary_unit(entry_what_t what, int kind, const void *target, const void *owner, uint32_t count,
                          uint32_t b, uintptr_t p)
{
    block_t *blk = &c.blocks[b];
    uint64_t unit = unit_bytes(blk->kind);
    uint64_t offset = (uint64_t)(p - blk->start);
    int32_t at;
    if (!c.remembering) return;
    if (offset % unit != 0) {
        c.totals[NN_REGION_TOTAL_UNPLACED]++;
        return;
    }
    if (bit_get(blk->tenure[c.tv].tenured, offset / unit)) return;
    at = holder_bump(b, p);
    if (at >= 0) remember_unit(at, what, kind, target, owner, count, b, offset / unit);
}

/* The same for a tracked object older than the closing region. */
static void boundary_place(int kind, int tracked, const void *target, uint32_t position)
{
    int32_t at;
    if (!c.remembering || bit_get(c.places[tracked].tenure[c.tv].tenured, position)) return;
    at = holder_place(tracked, position);
    if (at >= 0) remember_place(at, kind, tracked, target, position);
}


static void entry_forget(const entry_t *entry, int v)
{
    uint8_t *remembered = entry->block != NN_REGION_NONE ? c.blocks[entry->block].tenure[v].remembered
                                                         : c.places[entry->tracked].tenure[v].remembered;
    bit_clear(remembered, entry->index);
}

/* Drop what was remembered for a region, which no close reads. */
static void entries_drop(frame_t *f, int v)
{
    frame_tenure_t *ft = &f->t[v];
    uint32_t i;
    for (i = 0; i < ft->entry_count; i++) entry_forget(&ft->entries[i], v);
    c.t[v].held -= ft->entry_count;
    ft->entry_count = 0;
}

/* --- The mark --- */

static void work_push(int kind, const void *object, int remember)
{
    work_t *work = (work_t *)reserve(c.work, &c.work_cap, sizeof(work_t), c.work_count + 1);
    if (!work) {
        fail();
        return;
    }
    c.work = work;
    c.work[c.work_count].object = object;
    c.work[c.work_count].kind = kind;
    c.work[c.work_count].remember = remember;
    c.work_count++;
}

static void resurrect(int v, uint64_t bytes)
{
    c.t[v].figures[NN_REGION_TENURE_RESURRECTED]++;
    c.t[v].figures[NN_REGION_TENURE_RESURRECTED_BYTES] += bytes;
}

/* A survivor of `bytes` whose died bit is `died`, and whose tenure
 * collectors' bits are `t` at index `i`.  Only the census's own mark
 * counts what died: a tenure collector's takes its tenured objects for
 * reached, and may enter what they hold that the census's mark found
 * unreached.  The census's mark counts what any tenure collector freed,
 * and a tenure collector's mark of the whole what it freed itself, which
 * its other marks pass over as tenured. */
static void survive(uint64_t bytes, int died, const tenure_bits_t *t, uint64_t i)
{
    int v;
    c.surviving += bytes;
    if (c.tv < 0) {
        if (died) {
            c.totals[NN_REGION_TOTAL_RESURRECTED]++;
            c.totals[NN_REGION_TOTAL_RESURRECTED_BYTES] += bytes;
        }
        for (v = 0; v < NN_REGION_TENURE_VARIANTS; v++)
            if (bit_get(t[v].freed, i)) resurrect(v, bytes);
        return;
    }
    if (bit_get(t[c.tv].freed, i)) {
        NN_ASSERT(!collecting(), "nn_region_close: the collector's mark reached what it freed");
        resurrect(c.tv, bytes);
    }
}

/* Whether a tenure collector's mark under way passes over a unit or place
 * it reaches in the closing region: one it tenured, unless the mark is of
 * the whole region. */
static int passes_over(const tenure_bits_t *t, uint64_t i)
{
    return !c.full && bit_get(t->tenured, i);
}

static int is_static(int kind, const void *object)
{
    if (kind == NN_GRAPH_THUNK) return nn_thunk_is_static((const nn_thunk_t *)object);
    if (kind == NN_GRAPH_ENV) return object == nn_env_empty();
    if (kind == NN_GRAPH_ATTRSET) return object == nn_attrset_empty();
    return 0;
}

/* Reach a thunk cell or an env: mark its extent when it is younger than
 * the closing region, one a tenure collector's mark does not pass over,
 * and unmarked, and push it. */
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
    if (bump_age(c.closing, b, p) != AGE_YOUNGER) {
        if (c.tv >= 0) boundary_unit(ENTRY_OBJECT, kind, object, NULL, 0, b, p);
        return;
    }
    blk = &c.blocks[b];
    unit = unit_bytes(block_kind);
    offset = (uint64_t)(p - blk->start);
    u = offset / unit;
    if (offset % unit != 0) {
        c.totals[NN_REGION_TOTAL_UNPLACED]++;
        return;
    }
    if (c.tv >= 0 && passes_over(&blk->tenure[c.tv], u)) return;
    if (bit_get(blk->marks, u)) return;
    bytes = kind == NN_GRAPH_THUNK ? sizeof(nn_thunk_t) : nn_env_bytes((const nn_env_t *)object);
    bits_set(blk->marks, u, u + bytes / unit);
    survive(bytes, bit_get(blk->died, u), blk->tenure, u);
    work_push(kind, object, c.tv >= 0);
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
 * closing region, one a tenure collector's mark does not pass over, and
 * unmarked, and push it. */
static void reach_tracked(int kind, const void *object)
{
    int tracked = tracked_kind(kind);
    const object_slot_t *slot = object_slot(c.objects, c.object_cap, (uintptr_t)object);
    places_t *places = &c.places[tracked];
    uint64_t bytes;
    if (slot->object == 0 || slot->kind != tracked) {
        NN_ASSERT(!collecting(), "nn_region_close: the collector's mark reached a tracked object it holds no place for");
        c.totals[NN_REGION_TOTAL_UNPLACED]++;
        return;
    }
    if (slot->position < c.closing->tracked[tracked]) {
        if (c.tv >= 0) boundary_place(kind, tracked, object, slot->position);
        return;
    }
    if (c.tv >= 0 && passes_over(&places->tenure[c.tv], slot->position)) return;
    if (bit_get(places->marks, slot->position)) return;
    bit_set(places->marks, slot->position);
    bytes = tracked_bytes(tracked, object);
    c.surviving_tracked += bytes;
    survive(bytes, bit_get(places->died, slot->position), places->tenure, slot->position);
    work_push(kind, object, c.tv >= 0);
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
    if (bump_age(c.closing, b, p) != AGE_YOUNGER) {
        if (c.tv >= 0) boundary_unit(ENTRY_STRING, 0, s, NULL, 0, b, p);
        return;
    }
    blk = &c.blocks[b];
    offset = (uint64_t)(p - blk->start);
    u = offset / NN_STR_ALIGNMENT;
    if (c.tv >= 0 && passes_over(&blk->tenure[c.tv], u)) return;
    if (bit_get(blk->marks, u)) return;
    bytes = nn_str_object_bytes(s);
    bits_set(blk->marks, u, u + bytes / NN_STR_ALIGNMENT);
    survive(bytes, bit_get(blk->died, u), blk->tenure, u);
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
 * the list cannot reach them (head_t). */
static void keep_head(const block_t *blk, const void *list, uintptr_t p)
{
    const object_slot_t *slot = object_slot(c.objects, c.object_cap, (uintptr_t)list);
    head_t *heads;
    if (slot->object != (uintptr_t)list || slot->first < blk->start || slot->first >= p) return;
    heads = (head_t *)reserve(c.heads, &c.head_cap, sizeof(head_t), c.head_count + 1);
    if (!heads) {
        fail();
        return;
    }
    c.heads = heads;
    c.heads[c.head_count].from = (slot->first - blk->start) / NN_ENV_ALIGNMENT;
    c.heads[c.head_count].to = (p - blk->start) / NN_ENV_ALIGNMENT;
    c.heads[c.head_count].block = (uint32_t)(blk - c.blocks);
    c.head_count++;
}

/* Mark the heads kept, once the mark has followed everything. */
static void mark_heads(void)
{
    uint32_t i;
    for (i = 0; i < c.head_count; i++) {
        block_t *blk = &c.blocks[c.heads[i].block];
        uint64_t u;
        for (u = c.heads[i].from; u < c.heads[i].to; u++) {
            if (bit_get(blk->marks, u)) continue;
            bit_set(blk->marks, u);
            survive(sizeof(void *), bit_get(blk->died, u), blk->tenure, u);
        }
    }
    c.head_count = 0;
}

/* An array of the env pages, entry by entry: several list headers can
 * span parts of one items array, so each entry is marked on its own.  An
 * array is allocated whole, so its entries are tenured together, and what
 * a tenure collector's mark finds in them is remembered as for an object
 * it entered. */
static void visit_array(void *ctx, int kind, const void *owner, void *const *base, uint32_t count)
{
    uintptr_t p = (uintptr_t)base;
    uint32_t b, i;
    uint64_t u;
    block_t *blk;
    int remembering = c.remembering;
    c.examined++;
    b = block_of(p, NN_REGION_ENV_PAGE);
    if (b == NN_REGION_NONE) {
        c.totals[NN_REGION_TOTAL_UNPLACED]++;
        return;
    }
    if (bump_age(c.closing, b, p) != AGE_YOUNGER) {
        if (c.tv >= 0) boundary_unit(ENTRY_ARRAY, kind, base, owner, count, b, p);
        return;
    }
    blk = &c.blocks[b];
    u = (uint64_t)(p - blk->start) / NN_ENV_ALIGNMENT;
    if (c.tv >= 0) {
        if (passes_over(&blk->tenure[c.tv], u)) return;
        c.remembering = 1;
    }
    if (kind == NN_GRAPH_ITEMS) keep_head(blk, owner, p);
    for (i = 0; i < count; i++, u++) {
        if (bit_get(blk->marks, u)) continue;
        bit_set(blk->marks, u);
        survive(sizeof(void *), bit_get(blk->died, u), blk->tenure, u);
        nn_graph_entry(kind, base[i], &visitor, ctx);
    }
    c.remembering = remembering;
}

/* What a record names holds: a cell's value, or a with-scope entry's set
 * or thunk. */
static void visit_record(uintptr_t r)
{
    if (r & NN_REGION_RECORD_WITH)
        nn_graph_entry(NN_GRAPH_WITH_SCOPES, *(void *const *)(r & ~NN_REGION_RECORD_FLAGS), &visitor, NULL);
    else
        nn_graph_children(NN_GRAPH_THUNK, (const void *)(r & ~NN_REGION_RECORD_FLAGS), &visitor, NULL);
}

/* The roots registered since a region opened, and then what the mark
 * pushed, each remembering what it points at as the close tenures it. */
static void mark_rest(const frame_t *f)
{
    uint32_t i, n;
    c.remembering = 0;
    for (i = f->root_thunks, n = nn_roots_thunk_count(); i < n; i++)
        visit_object(NULL, NN_GRAPH_THUNK, nn_roots_thunk(i));
    for (i = f->root_envs, n = nn_roots_env_count(); i < n; i++)
        visit_object(NULL, NN_GRAPH_ENV, nn_roots_env(i));
    while (c.work_count > 0 && !c.failed) {
        work_t item = c.work[--c.work_count];
        c.entered++;
        c.remembering = item.remember;
        nn_graph_children(item.kind, item.object, &visitor, NULL);
    }
    c.remembering = 0;
    mark_heads();
}

/* Mark from the closing region's records of older objects and the roots
 * registered since it opened. */
static void mark(const frame_t *f)
{
    uint32_t i;
    for (i = f->records; i < c.record_count; i++) {
        uintptr_t r = c.records[i];
        age_t age = record_age(f, r);
        if (age == AGE_OUTSIDE) c.totals[NN_REGION_TOTAL_UNPLACED]++;
        if (age == AGE_OLDER) visit_record(r);
    }
    mark_rest(f);
}

static void entry_visit(const entry_t *entry)
{
    switch (entry->what) {
    case ENTRY_OBJECT:
        visit_object(NULL, entry->kind, entry->target);
        break;
    case ENTRY_STRING:
        visit_string(NULL, (const nn_str_t *)entry->target);
        break;
    default:
        visit_array(NULL, entry->kind, entry->owner, (void *const *)entry->target, entry->count);
        break;
    }
}

/* A tenure collector's mark at a close.  Its roots are the records of
 * cells and entries the mark does not enter, which it reads, what closes
 * inside remembered for the region, and the roots registered since it
 * opened, and it enters only what is younger than the region and
 * untenured.  A mark of the whole region enters what is younger than it,
 * and its roots are every record of a cell or entry older than it and the
 * registered roots: a cell or entry inside the region it enters itself if
 * it is reached, and so with whatever else holds a remembered pointer.
 * Otherwise only records no close of its has read are roots, those of a
 * cell or entry older than the region or tenured: a cell or entry is
 * stored into once, so what an earlier close read of it is tenured or
 * remembered for the region it is untenured in.  Every record a close of
 * its has seen counts as read: no later close but a mark of the whole
 * reads it, and what the cell or entry it names holds is remembered for
 * the regions around where it is untenured. */
static void tenure_mark(frame_t *f, int v, int full)
{
    frame_tenure_t *ft = &f->t[v];
    tenure_t *t = &c.t[v];
    uint32_t i;
    c.tv = v;
    c.full = full;
    c.remembering = 1;
    for (i = ft->records; i < t->record_count; i++) {
        uintptr_t r = t->records[i];
        uint64_t u = 0;
        uint32_t b = record_unit(r, &u);
        if (b == NN_REGION_NONE) continue;
        t->records[i] = r | NN_REGION_RECORD_READ;
        if (bump_age(f, b, r & ~NN_REGION_RECORD_FLAGS) == AGE_YOUNGER
            && (full || !bit_get(c.blocks[b].tenure[v].tenured, u)))
            continue;
        if (!full && (r & NN_REGION_RECORD_READ)) continue;
        visit_record(r);
    }
    for (i = 0; i < ft->entry_count; i++) {
        entry_forget(&ft->entries[i], v);
        if (!full) entry_visit(&ft->entries[i]);
    }
    t->held -= ft->entry_count;
    ft->entry_count = 0;
    mark_rest(f);
    c.full = 0;
    c.tv = -1;
}

/* --- Retiring a region --- */

/* The finest lines a block's part from `from` bytes to `used` covers,
 * from the one numbered *first to below *count, with room in the scratch
 * flags for one each.  Returns 0 or -1. */
static int live_span(const block_t *blk, uint64_t from, uint64_t used, uint64_t *first, uint64_t *count)
{
    uint64_t finest = nn_region_grain_bytes(blk->kind, NN_REGION_GRAINS - 1);
    *first = (from + finest - 1) / finest;
    *count = (used + finest - 1) / finest;
    if (*count > *first && *count - *first > c.live_cap) {
        uint8_t *live = (uint8_t *)realloc(c.live, (size_t)(*count - *first));
        if (!live) return -1;
        c.live = live;
        c.live_cap = *count - *first;
    }
    return 0;
}

/* Add to dead[g] the bytes of each block or line of grain g wholly in a
 * block's part from `from` bytes to `used` that no scratch flag of a
 * finest line in it says is live. */
static void dead_grains(const block_t *blk, uint64_t from, uint64_t used, uint64_t first, uint64_t count, uint64_t *dead)
{
    uint64_t finest = nn_region_grain_bytes(blk->kind, NN_REGION_GRAINS - 1);
    uint64_t j;
    uint32_t g;
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
}

/* byte_tenure over the units from `from` to below `to`: whether it freed
 * any. */
static int tenure_range(uint8_t *marks, const tenure_bits_t *t, int full, uint64_t from, uint64_t to)
{
    uint64_t i = from;
    uint8_t lost = 0;
    for (; i < to && i % 8 != 0; i++) lost |= byte_tenure(marks, t, full, i / 8, (uint8_t)(1u << (i % 8)));
    for (; i + 8 <= to; i += 8) lost |= byte_tenure(marks, t, full, i / 8, 0xFFu);
    for (; i < to; i++) lost |= byte_tenure(marks, t, full, i / 8, (uint8_t)(1u << (i % 8)));
    return lost != 0;
}

/* The collector's retirement of a block's units from `from` bytes in to
 * `used`, a line at a time: a line below `settled`, where no more objects
 * go, is free once this close frees the last of its units, the ones past
 * `used` never handed out.  The units of a line below `from` are older
 * objects of the same span, as a span starts on a line, so a line this
 * region shares with closes before it is free once theirs and its own are
 * all freed.  A line whose last unit a close freed while the cursor was
 * in it is freed by a later close that frees a unit in it, or by
 * span_stop when its span ends with the cursor still in it. */
static void collect_retire_block(block_t *blk, uint64_t from, uint64_t used, uint64_t settled, int full)
{
    uint64_t unit = unit_bytes(blk->kind), at = from;
    tenure_bits_t *t = &blk->tenure[NN_REGION_COLLECTOR];
    while (at < used) {
        uint64_t line = at / NN_REGION_LINE;
        uint64_t stop = (line + 1) * NN_REGION_LINE < used ? (line + 1) * NN_REGION_LINE : used;
        if (tenure_range(blk->marks, t, full, at / unit, stop / unit) && (line + 1) * NN_REGION_LINE <= settled
            && line < blk->lines && bits_all(t->freed, line * NN_REGION_LINE / unit, stop / unit))
            free_line(blk, line);
        at = stop;
    }
}

/* Retire a block's units from `from` bytes in, the closing region's part
 * of it, and add to dead[g] the bytes of each block or line of grain g
 * wholly in that part that holds no survivor. */
static int retire_block(block_t *blk, uint64_t from, uint64_t used, uint64_t *dead)
{
    uint64_t unit = unit_bytes(blk->kind);
    uint64_t finest = nn_region_grain_bytes(blk->kind, NN_REGION_GRAINS - 1);
    uint64_t first, count, j;
    if (used <= from) return 0;
    if (live_span(blk, from, used, &first, &count) != 0) return -1;
    for (j = first; j < count; j++) {
        uint64_t end = (j + 1) * finest < used ? (j + 1) * finest : used;
        c.live[j - first] = (uint8_t)bits_any(blk->marks, j * finest / unit, end / unit);
    }
    dead_grains(blk, from, used, first, count, dead);
    bits_retire(blk->marks, blk->died, from / unit, used / unit);
    return 0;
}

/* Tenure collector v's retirement of a block's units from `from` bytes
 * in at a close where it marked (bits_tenure), adding to dead[g] the
 * bytes of each block or line of grain g wholly in that part in which it
 * has freed every unit; or, at an outermost close, where it takes the
 * census's own mark of the whole region, before the census retires it,
 * freeing what that mark left unmarked. */
static int tenure_retire_block(block_t *blk, uint64_t from, uint64_t used, uint64_t settled, tenure_action_t action,
                               int v, uint64_t *dead, uint64_t *reached)
{
    uint64_t unit = unit_bytes(blk->kind);
    uint64_t finest = nn_region_grain_bytes(blk->kind, NN_REGION_GRAINS - 1);
    uint64_t first, count, j;
    if (used <= from) return 0;
    if (action == TENURE_OUTERMOST) {
        bits_tenure_full(blk->marks, &blk->tenure[v], from / unit, used / unit);
        return 0;
    }
    if (collecting()) {
        collect_retire_block(blk, from, used, settled, action == TENURE_FULL);
        return 0;
    }
    bits_tenure(blk->marks, blk->died, &blk->tenure[v], action == TENURE_FULL, from / unit, used / unit, reached);
    if (live_span(blk, from, used, &first, &count) != 0) return -1;
    for (j = first; j < count; j++) {
        uint64_t end = (j + 1) * finest < used ? (j + 1) * finest : used;
        c.live[j - first] = (uint8_t)!bits_all(blk->tenure[v].freed, j * finest / unit, end / unit);
    }
    dead_grains(blk, from, used, first, count, dead);
    return 0;
}

/* The next run of a span's own lines from `at` bytes in, to below `end`:
 * its start, with *to past its last byte, or `end` for none.  A line a
 * refill handed out again is a newer span's. */
static uint64_t span_run(const span_t *s, uint64_t at, uint64_t end, uint64_t *to)
{
    const block_t *blk = &c.blocks[s->block];
    while (at < end && blk->spans[at / NN_REGION_LINE] != s->number) at = (at / NN_REGION_LINE + 1) * NN_REGION_LINE;
    *to = at;
    while (*to < end && blk->spans[*to / NN_REGION_LINE] == s->number) *to = (*to / NN_REGION_LINE + 1) * NN_REGION_LINE;
    if (*to > end) *to = end;
    return at < end ? at : end;
}

/* The closing region's part of a span, from `from` bytes in. */
static void retire_span(const span_t *s, uint64_t from, uint64_t *dead)
{
    uint64_t end = span_end(s), to, at = from > s->start ? from : s->start;
    while (!c.failed && (at = span_run(s, at, end, &to)) < end) {
        if (retire_block(&c.blocks[s->block], at, to, dead) != 0) fail();
        at = to;
    }
}

/* The spans a region's part takes in: the one each bump kind bumped from
 * as it opened, from where the cursor was, and every one after.  One the
 * table no longer keeps holds no line, so its part is empty. */
static const span_t *opening_span(const frame_t *f, int kind)
{
    return f->current_span[kind] == NN_REGION_NONE ? NULL : span_numbered(f->current_span[kind]);
}

/* Retire every span the closing region allocated in, and every tracked
 * object it made. */
static void retire(const frame_t *f, uint64_t *dead)
{
    uint32_t i;
    int k;
    for (k = 0; k < NN_REGION_BUMP_KINDS && !c.failed; k++) {
        const span_t *opening = opening_span(f, k);
        if (opening) retire_span(opening, f->current_used[k], dead);
    }
    for (i = span_index_from(f->spans); i < c.span_count && !c.failed; i++) retire_span(&c.spans[i], c.spans[i].start, dead);
    /* Every object a tracker gained while the census ran has a place in
     * the bits; one it held before has none and is in no region. */
    for (k = 0; k < NN_REGION_TRACKED_KINDS; k++) {
        uint32_t end = tracker_count(k);
        if (end > c.places[k].capacity) end = c.places[k].capacity;
        if (f->tracked[k] < end)
            bits_retire(c.places[k].marks, c.places[k].died, f->tracked[k], end);
    }
}

/* The collector's sweep of a tracked kind's places from `from` to below
 * `end`, the closing region's, once its close has settled them: it frees
 * the objects whose places it freed, and the rest close up in order,
 * tenured, behind the places before `from`.  No close remembers one of
 * them: a pointer into the region a close inside found is remembered
 * for the region itself, whose mark has forgotten it, reading it first
 * unless the mark is of the whole. */
static int sweep_keep(void *ctx, void *object, uint32_t place, uint32_t kept_at)
{
    const tenure_bits_t *bits = (const tenure_bits_t *)ctx;
    if (bit_get(bits->freed, place)) {
        object_remove((uintptr_t)object);
        return 0;
    }
    if (kept_at != place) object_slot(c.objects, c.object_cap, (uintptr_t)object)->position = kept_at;
    return 1;
}

static void sweep(int kind, uint32_t from, uint32_t end)
{
    tenure_bits_t *bits = &c.places[kind].tenure[NN_REGION_COLLECTOR];
    uint32_t kept;
    NN_ASSERT(!bits_any(bits->remembered, from, end), "nn_region_close: a place the sweep moves is remembered");
    switch (kind) {
    case NN_REGION_SET:        nn_attrset_sweep(from, sweep_keep, bits); break;
    case NN_REGION_LIST:       nn_list_sweep(from, sweep_keep, bits); break;
    case NN_REGION_LAMBDA:     nn_lambda_sweep(from, sweep_keep, bits); break;
    case NN_REGION_PRIMOP_APP: nn_primop_app_sweep(from, sweep_keep, bits); break;
    default:                   nn_ctxstr_sweep(from, sweep_keep, bits); break;
    }
    kept = tracker_count(kind);
    bits_set(bits->tenured, from, kept);
    bits_clear(bits->tenured, kept, end);
    bits_clear(bits->freed, from, end);
}

/* Tenure collector v's retirement of the closing region's part of a
 * span, from `from` bytes in (tenure_retire_block). */
static void tenure_retire_span(const span_t *s, uint64_t from, tenure_action_t action, int v, uint64_t *dead,
                               uint64_t *reached)
{
    uint64_t end = span_end(s), settled = span_settled(s), to, at = from > s->start ? from : s->start;
    while (!c.failed && (at = span_run(s, at, end, &to)) < end) {
        if (tenure_retire_block(&c.blocks[s->block], at, to, settled, action, v, dead, reached) != 0) fail();
        at = to;
    }
}

/* Tenure collector v's retirement of every span the closing region
 * allocated in, and every tracked object it made. */
static void tenure_retire(const frame_t *f, tenure_action_t action, int v, uint64_t *dead, uint64_t *reached)
{
    uint32_t i;
    int k;
    for (k = 0; k < NN_REGION_BUMP_KINDS && !c.failed; k++) {
        const span_t *opening = opening_span(f, k);
        if (opening) tenure_retire_span(opening, f->current_used[k], action, v, dead, reached);
    }
    for (i = span_index_from(f->spans); i < c.span_count && !c.failed; i++)
        tenure_retire_span(&c.spans[i], c.spans[i].start, action, v, dead, reached);
    for (k = 0; k < NN_REGION_TRACKED_KINDS && !c.failed; k++) {
        places_t *places = &c.places[k];
        uint32_t end = tracker_count(k);
        if (end > places->capacity) end = places->capacity;
        if (f->tracked[k] >= end) continue;
        if (action == TENURE_OUTERMOST)
            bits_tenure_full(places->marks, &places->tenure[v], f->tracked[k], end);
        else
            bits_tenure(places->marks, places->died, &places->tenure[v], action == TENURE_FULL, f->tracked[k], end,
                        reached);
        if (collecting()) sweep(k, f->tracked[k], end);
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

/* Keep for the region around the closing one tenure collector v's
 * records of cells and entries older than it, which a mark of the whole
 * region around reads, and of those no close has read that are tenured
 * and not freed, which its next close that marks reads. */
static void pass_tenure_records(const frame_t *f, const frame_t *parent, int v)
{
    tenure_t *t = &c.t[v];
    uint32_t kept = f->t[v].records, i;
    if (parent) {
        for (i = f->t[v].records; i < t->record_count; i++) {
            uintptr_t r = t->records[i];
            uint64_t u = 0;
            uint32_t b = record_unit(r, &u);
            const tenure_bits_t *bits;
            if (b == NN_REGION_NONE) continue;
            bits = &c.blocks[b].tenure[v];
            if (bump_age(parent, b, r & ~NN_REGION_RECORD_FLAGS) != AGE_YOUNGER
                || (!(r & NN_REGION_RECORD_READ) && bit_get(bits->tenured, u) && !bit_get(bits->freed, u)))
                t->records[kept++] = r;
        }
    }
    t->record_count = kept;
}

/* What tenure collector v does at the close of a region that allocated
 * `allocated` bytes. */
static tenure_action_t tenure_action(const frame_t *f, const frame_t *parent, int v, uint64_t allocated)
{
    if (allocated < nn_region_threshold_bytes(NN_REGION_TENURE_THRESHOLD)) return TENURE_NONE;
    if (!parent) return collecting() ? TENURE_FULL : TENURE_OUTERMOST;
    if (tenure_budgets[v] != 0 && f->t[v].unchecked >= tenure_budgets[v]) return TENURE_FULL;
    return TENURE_YOUNG;
}

/* Tenure collector v at this close, after the census's own has counted
 * and retired the region.  The bytes it holds peak just before it frees.
 * Where it marks, it frees what died in the region less what it freed
 * inside, as simulate does, and the region around takes on the whole
 * region as tenured or freed, and what it tenured as unchecked unless its
 * mark was of the whole.  `dead` is what died in the region, by the
 * census's own mark. */
static void tenure_close(frame_t *f, frame_t *parent, tenure_action_t action, int v, uint64_t allocated,
                         uint64_t tracked, const uint64_t *dead)
{
    frame_tenure_t *ft = &f->t[v];
    tenure_t *t = &c.t[v];
    uint64_t lost[NN_REGION_GRAINS], lost_tracked = 0, reached = 0, unchecked = 0;
    uint32_t g;
    memset(lost, 0, sizeof(lost));
    /* The peaks are the census's figures, which collecting keeps none of. */
    if (!collecting()) {
        uint64_t now = allocated_now();
        for (g = 0; g < NN_REGION_GRAINS; g++) {
            uint64_t held = now - t->freed[g];
            if (held > t->figures[NN_REGION_TENURE_PEAK + g]) t->figures[NN_REGION_TENURE_PEAK + g] = held;
        }
    }
    if (action == TENURE_YOUNG || action == TENURE_FULL) {
        int full = action == TENURE_FULL;
        /* What the mark may reach: what the closes inside left untenured,
         * or for a mark of the whole what they left unfreed. */
        uint64_t open = allocated - (full ? ft->freed[0] : ft->tenured);
        uint64_t open_tracked = tracked - (full ? ft->freed_tracked : ft->tenured_tracked);
        c.surviving = 0;
        c.surviving_tracked = 0;
        c.entered = 0;
        c.examined = 0;
        tenure_mark(f, v, full);
        if (!c.failed) tenure_retire(f, action, v, lost, &reached);
        if (c.failed) return;
        /* A mark of the whole that reaches what was freed counts it as
         * resurrected, so only the region bounds what it reaches. */
        NN_ASSERT(full ? c.surviving <= allocated && c.surviving_tracked <= tracked
                       : c.surviving <= open && c.surviving_tracked <= open_tracked,
                  "nn_region_close: more survives than the tenure collector's mark could reach");
        if (full) {
            lost_tracked = tracked - c.surviving_tracked;
            lost[0] = allocated - c.surviving;
        } else {
            lost_tracked = ft->freed_tracked + open_tracked - c.surviving_tracked;
            lost[0] = ft->freed[0] + open - c.surviving;
            unchecked = ft->unchecked + c.surviving;
        }
        for (g = 1; g < NN_REGION_GRAINS; g++) lost[g] += lost_tracked;
        t->figures[NN_REGION_TENURE_REACHED] += reached;
    } else if (action == TENURE_OUTERMOST) {
        /* The census's mark of the whole region is this close's, and its
         * counts are still the close's. */
        memcpy(lost, dead, sizeof(lost));
        lost_tracked = tracked - c.surviving_tracked;
    }
    if (action != TENURE_NONE) {
        t->figures[NN_REGION_TENURE_CLOSES]++;
        t->figures[NN_REGION_TENURE_ENTERED] += c.entered;
        t->figures[NN_REGION_TENURE_EXAMINED] += c.examined;
        if (action != TENURE_YOUNG) {
            t->figures[NN_REGION_TENURE_FULL]++;
            if (lost[0] > ft->freed[0]) t->figures[NN_REGION_TENURE_FULL_FREED] += lost[0] - ft->freed[0];
        }
        /* What it freed inside died, so only a premise the counts would
         * show broken makes it more than what died. */
        for (g = 0; g < NN_REGION_GRAINS; g++)
            if (lost[g] > ft->freed[g]) t->freed[g] += lost[g] - ft->freed[g];
    }
    /* A region below the threshold holds none past it, as what a region
     * holds allocated in it, so no close inside it marked: it tenured,
     * freed and remembered nothing. */
    NN_ASSERT(action != TENURE_NONE || (ft->tenured == 0 && ft->unchecked == 0 && ft->entry_count == 0),
              "nn_region_close: a region below the threshold holds what a close that marked left");
    if (!parent) {
        entries_drop(f, v);
    } else if (action != TENURE_NONE) {
        frame_tenure_t *pt = &parent->t[v];
        pt->tenured += allocated;
        pt->tenured_tracked += tracked;
        for (g = 0; g < NN_REGION_GRAINS; g++) pt->freed[g] += lost[g];
        pt->freed_tracked += lost_tracked;
        pt->unchecked += unchecked;
    }
    pass_tenure_records(f, parent, v);
}

/* --- The evaluator's half --- */

void nn_region_open(void)
{
    frame_t *frames, *f;
    uint32_t cap = c.frame_cap;
    int k, v;
    if (!nn_region_mode || c.failed) return;
    frames = (frame_t *)reserve(c.frames, &c.frame_cap, sizeof(frame_t), c.frame_count + 1);
    if (!frames) {
        fail();
        return;
    }
    /* A frame keeps its entries' buffer from one region to the next. */
    if (c.frame_cap > cap) memset(frames + cap, 0, (size_t)(c.frame_cap - cap) * sizeof(frame_t));
    c.frames = frames;
    f = &c.frames[c.frame_count++];
    f->spans = c.span_next;
    for (k = 0; k < NN_REGION_BUMP_KINDS; k++) {
        f->current_span[k] = c.current_span[k];
        f->current_used[k] = c.current_span[k] == NN_REGION_NONE ? 0 : cursor_of(k);
    }
    for (k = 0; k < NN_REGION_TRACKED_KINDS; k++) f->tracked[k] = tracker_count(k);
    f->tracked_bytes = c.tracked_bytes;
    f->allocated = allocated_now();
    f->records = c.record_count;
    nn_roots_mark(&f->root_thunks, &f->root_envs);
    /* What the census simulates is the census's alone. */
    if (!collecting()) memset(f->freed, 0, sizeof(f->freed));
    for (v = 0; v < NN_REGION_TENURE_VARIANTS; v++) {
        frame_tenure_t *ft = &f->t[v];
        if (!active(v)) continue;
        ft->records = c.t[v].record_count;
        ft->unchecked = 0;
        ft->tenured = 0;
        ft->tenured_tracked = 0;
        memset(ft->freed, 0, sizeof(ft->freed));
        ft->freed_tracked = 0;
        ft->entry_count = 0;
    }
    if (c.frame_count > c.totals[NN_REGION_TOTAL_DEPTH]) c.totals[NN_REGION_TOTAL_DEPTH] = c.frame_count;
}

/* A census's close: its own mark, and then every tenure collector's. */
static void census_close(frame_t *f, frame_t *parent)
{
    uint64_t allocated, tracked, dead[NN_REGION_GRAINS];
    uint32_t g, records;
    tenure_action_t actions[NN_REGION_TENURE_VARIANTS];
    int v;
    c.surviving = 0;
    c.surviving_tracked = 0;
    c.entered = 0;
    c.examined = 0;
    records = c.record_count - f->records;
    allocated = allocated_now() - f->allocated;
    tracked = c.tracked_bytes - f->tracked_bytes;
    for (v = 0; v < NN_REGION_TENURE_VARIANTS; v++) actions[v] = tenure_action(f, parent, v, allocated);

    mark(f);
    for (v = 0; v < NN_REGION_TENURE_VARIANTS && !c.failed; v++)
        if (actions[v] == TENURE_OUTERMOST) tenure_retire(f, actions[v], v, NULL, NULL);
    memset(dead, 0, sizeof(dead));
    if (!c.failed) retire(f, dead);
    if (c.failed) return;
    NN_ASSERT(c.surviving <= allocated && c.surviving_tracked <= tracked,
              "nn_region_close: more survives than the region allocated");
    dead[0] = allocated - c.surviving;
    for (g = 1; g < NN_REGION_GRAINS; g++) dead[g] += tracked - c.surviving_tracked;

    count_close(allocated, records);
    if (!parent) c.enclosed += allocated;
    simulate(f, parent, allocated, dead);
    pass_records(f, parent);
    for (v = 0; v < NN_REGION_TENURE_VARIANTS && !c.failed; v++)
        tenure_close(f, parent, actions[v], v, allocated, tracked, dead);
}

/* The collector's close: its tenure collector's alone, which frees. */
static void collect_close(frame_t *f, frame_t *parent)
{
    uint64_t allocated = allocated_now() - f->allocated;
    uint64_t tracked = c.tracked_bytes - f->tracked_bytes;
    tenure_close(f, parent, tenure_action(f, parent, NN_REGION_COLLECTOR, allocated), NN_REGION_COLLECTOR,
                 allocated, tracked, NULL);
}

void nn_region_close(void)
{
    frame_t *f, *parent;
    if (!nn_region_mode || c.failed) return;
    NN_ASSERT(c.frame_count > 0, "nn_region_close: no region is open");
    f = &c.frames[c.frame_count - 1];
    parent = c.frame_count > 1 ? &c.frames[c.frame_count - 2] : NULL;
    c.closing = f;
    if (collecting())
        collect_close(f, parent);
    else
        census_close(f, parent);
    if (c.failed) return;
    c.closing = NULL;
    c.frame_count--;
    if (c.frame_count == 0) spans_trim();
}

/* --- Reading the figures --- */

void nn_region_totals(uint64_t *totals)
{
    uint64_t allocated = nn_region_mode ? allocated_now() : 0;
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
    uint64_t now = nn_region_mode ? allocated_now() : 0;
    uint32_t k, g;
    memcpy(figures, c.thresholds, sizeof(c.thresholds));
    for (k = 0; k < NN_REGION_THRESHOLDS; k++) {
        for (g = 0; g < NN_REGION_GRAINS; g++) {
            uint64_t *peak = &figures[k * NN_REGION_THRESHOLD_FIGURES + NN_REGION_THRESHOLD_PEAK + g];
            if (now > c.freed[k][g] && now - c.freed[k][g] > *peak) *peak = now - c.freed[k][g];
        }
    }
}

uint64_t nn_region_tenure_budget(uint32_t variant)
{
    return variant < NN_REGION_TENURE_VARIANTS ? tenure_budgets[variant] : 0;
}

uint32_t nn_region_spans_begun(void)
{
    return c.span_next;
}

uint32_t nn_region_spans_most(void)
{
    return c.span_most_ended;
}

uint64_t nn_region_lines_lost(void)
{
    uint64_t lost = 0, line;
    uint32_t b;
    if (!collecting()) return 0;
    for (b = 0; b < c.block_count; b++) {
        const block_t *blk = &c.blocks[b];
        uint64_t per;
        if (blk->kind >= NN_REGION_BUMP_KINDS || !blk->free) continue;
        per = NN_REGION_LINE / unit_bytes(blk->kind);
        for (line = 0; line < blk->lines; line++)
            if (!bit_get(blk->free, line)
                && bits_all(blk->tenure[NN_REGION_COLLECTOR].freed, line * per, (line + 1) * per))
                lost++;
    }
    return lost;
}

void nn_region_tenure(uint32_t variant, uint64_t *figures)
{
    uint64_t now = nn_region_mode ? allocated_now() : 0;
    const tenure_t *t;
    uint32_t g;
    if (variant >= NN_REGION_TENURE_VARIANTS) return;
    t = &c.t[variant];
    memcpy(figures, t->figures, sizeof(t->figures));
    for (g = 0; g < NN_REGION_GRAINS; g++) {
        uint64_t *peak = &figures[NN_REGION_TENURE_PEAK + g];
        if (now > t->freed[g] && now - t->freed[g] > *peak) *peak = now - t->freed[g];
    }
}
