/*
 * nn_stats.c - A census of the C data layer (nn_stats.h).
 *
 * The index places a pointer in one of two ways.  Thunk cells, env page
 * allocations and strings sit in a few large regions (the arena's blocks,
 * the env pages, the string arenas' blocks): a region index sorts the
 * regions by address and gives each a run of bits, one per thunk cell,
 * per NN_ENV_ALIGNMENT bytes of page or per NN_STR_ALIGNMENT bytes of
 * strings, so a pointer finds its bit by a binary search over regions;
 * the code's strings are indexed only to be placed, as code is not
 * marked.  Sets, list headers, lambdas, builtins' partial applications
 * and strings with context are each their own allocation: an object index sorts a copy of the module's
 * tracking array and gives each object a bit, found by a binary search
 * for its address.
 * Addresses are compared as uintptr_t, since C orders pointers only
 * within one object.
 *
 * A list's items array has no header of its own: the headers that end at
 * one address share the array the longest of them spans (nn_list.h), so
 * the index groups headers by that address.  The mark records, per array,
 * the lowest address it has scanned from; every header over the array is
 * a suffix of it, so a header reached later scans only the elements below
 * that address, and a chain of suffixes costs its longest one.
 *
 * The mark sets an object's bit as it first reaches it and pushes it on a
 * work stack, so each object is pushed once and entered once.
 */

#include "nn_stats.h"

#include "nn_assert.h"
#include "nn_attrset.h"
#include "nn_bytecode.h"
#include "nn_ctxstr.h"
#include "nn_env.h"
#include "nn_lambda.h"
#include "nn_list.h"
#include "nn_primop.h"
#include "nn_roots.h"
#include "nn_str.h"
#include "nn_symbol.h"
#include "nn_thunk.h"

#include <stdlib.h>
#include <string.h>

/* --- Index types --- */

/* A run of address space and the bit its first unit has. */
typedef struct {
    uintptr_t start;
    uintptr_t end;        /* one past the last byte handed out */
    uint64_t  first_bit;
} nn_span_t;

/* Regions holding objects at multiples of `unit` bytes from each start. */
typedef struct {
    nn_span_t *spans;     /* ascending start */
    uint32_t   count;
    uint64_t   unit;
    uint8_t   *bits;
} nn_region_index_t;

/* Objects that are each an allocation of their own. */
typedef struct {
    uintptr_t *objects;   /* ascending address */
    uint32_t   count;
    uint8_t   *bits;
} nn_object_index_t;

/* An items array and how much of it the mark has scanned. */
typedef struct {
    uintptr_t end;        /* where every header over it ends */
    uintptr_t scanned;    /* the lowest address scanned from; end when none */
    uint32_t  length;     /* elements the array holds */
} nn_items_t;

/* An object the mark has reached and not yet entered. */
typedef struct {
    const void *object;
    uint8_t     kind;
} nn_work_t;

/* A list header's end address, length and place in the list index,
 * sorted to group headers by array. */
typedef struct {
    uintptr_t end;
    uint32_t  length;
    uint32_t  pos;
} nn_header_end_t;

/* --- Session state --- */

#define NN_STATS_WORK_INITIAL      4096u

static struct {
    int open;
    int failed;

    nn_region_index_t thunks;
    nn_region_index_t pages;
    nn_object_index_t sets;
    nn_object_index_t lists;
    nn_object_index_t lambdas;
    nn_object_index_t primop_apps;
    nn_object_index_t ctxstrs;
    nn_region_index_t strings;
    nn_region_index_t code_strings;

    nn_items_t *items;
    uint32_t    item_count;
    uint64_t    items_bytes;
    uint32_t   *list_items;   /* per list in index order: its items array */

    uint8_t *symbol_bits;
    uint32_t symbol_count;

    nn_work_t *work;
    size_t     work_count;
    size_t     work_cap;

    uint64_t allocated_count[NN_STATS_KIND_COUNT];
    uint64_t allocated_bytes[NN_STATS_KIND_COUNT];
    uint64_t reached_count[NN_STATS_KIND_COUNT];
    uint64_t reached_bytes[NN_STATS_KIND_COUNT];
    uint64_t overhead_bytes;
    uint64_t code_bytes;
    uint64_t visited_objects;
    uint64_t visited_bytes;
    uint64_t unplaced;
} g;

/* --- Counting by site --- */

int nn_stats_counting = 0;

/* Apart from the session: counting runs for the whole evaluation, and a
 * census reads it. */
static struct {
    int complete;   /* counting began before the env pages' first byte */
    uint64_t env_count[NN_STATS_ENV_SITES];
    uint64_t env_slots[NN_STATS_ENV_SITES];
    uint64_t env_bytes[NN_STATS_ENV_SITES];
    uint64_t env_sizes[NN_STATS_ENV_SIZES];
    uint64_t set_count[NN_STATS_SET_SITES];
    uint64_t set_shareable[NN_STATS_SET_SITES];
    uint64_t set_bytes[NN_STATS_SET_SITES];
} sites;

void nn_stats_count_sites(void)
{
    memset(&sites, 0, sizeof(sites));
    sites.complete = nn_env_bytes_allocated() == 0;
    nn_stats_counting = 1;
}

/* A site the evaluator never numbers is a defect in it: asserted, and
 * counted nowhere in a build without asserts. */
void nn_stats_count_env(uint32_t site, uint32_t slots, uint64_t bytes)
{
    NN_ASSERT(site < NN_STATS_ENV_SITES, "nn_stats_count_env: unknown site");
    if (site >= NN_STATS_ENV_SITES) return;
    sites.env_count[site]++;
    sites.env_slots[site] += slots;
    sites.env_bytes[site] += bytes;
    sites.env_sizes[slots < NN_STATS_ENV_SIZES - 1 ? slots : NN_STATS_ENV_SIZES - 1]++;
}

void nn_stats_count_env_bytes(uint32_t site, uint64_t bytes)
{
    NN_ASSERT(site < NN_STATS_ENV_SITES, "nn_stats_count_env_bytes: unknown site");
    if (site >= NN_STATS_ENV_SITES) return;
    sites.env_bytes[site] += bytes;
}

void nn_stats_count_set(uint32_t site, uint64_t bytes, int shareable)
{
    NN_ASSERT(site < NN_STATS_SET_SITES, "nn_stats_count_set: unknown site");
    if (site >= NN_STATS_SET_SITES) return;
    sites.set_count[site]++;
    sites.set_bytes[site] += bytes;
    if (shareable) sites.set_shareable[site]++;
}

/* The first `max` of `n` figures from `from` to `to`.  No copy for none:
 * memcpy takes no NULL, whatever the length. */
static void copy_figures(uint64_t *to, const uint64_t *from, uint32_t n, uint32_t max)
{
    uint32_t k = max < n ? max : n;
    if (k > 0) memcpy(to, from, (size_t)k * sizeof(uint64_t));
}

uint32_t nn_stats_env_sites(uint64_t *envs, uint64_t *slots, uint64_t *bytes, uint32_t max)
{
    copy_figures(envs, sites.env_count, NN_STATS_ENV_SITES, max);
    copy_figures(slots, sites.env_slots, NN_STATS_ENV_SITES, max);
    copy_figures(bytes, sites.env_bytes, NN_STATS_ENV_SITES, max);
    return NN_STATS_ENV_SITES;
}

uint32_t nn_stats_env_sizes(uint64_t *envs, uint32_t max)
{
    copy_figures(envs, sites.env_sizes, NN_STATS_ENV_SIZES, max);
    return NN_STATS_ENV_SIZES;
}

uint32_t nn_stats_set_sites(uint64_t *sets, uint64_t *shareable, uint64_t *bytes, uint32_t max)
{
    copy_figures(sets, sites.set_count, NN_STATS_SET_SITES, max);
    copy_figures(shareable, sites.set_shareable, NN_STATS_SET_SITES, max);
    copy_figures(bytes, sites.set_bytes, NN_STATS_SET_SITES, max);
    return NN_STATS_SET_SITES;
}

/* Every env, when counting has covered every one; NN_STATS_UNCOUNTED
 * otherwise. */
static uint64_t counted_envs(void)
{
    uint64_t total = 0;
    uint32_t i;
    if (!nn_stats_counting || !sites.complete) return NN_STATS_UNCOUNTED;
    for (i = 0; i < NN_STATS_ENV_SITES; i++) total += sites.env_count[i];
    return total;
}

/* --- Bits --- */

/* A zeroed bitmap of `bits` bits; never a zero-byte request, whose answer
 * malloc may give as NULL. */
static uint8_t *bitmap_new(uint64_t bits)
{
    return (uint8_t *)calloc((size_t)(bits / 8 + 1), 1);
}

/* Set a bit; 1 when it was clear. */
static int bit_set(uint8_t *bits, uint64_t i)
{
    uint8_t mask = (uint8_t)(1u << (i % 8));
    if (bits[i / 8] & mask) return 0;
    bits[i / 8] |= mask;
    return 1;
}

static int bit_get(const uint8_t *bits, uint64_t i)
{
    return (bits[i / 8] >> (i % 8)) & 1u;
}

/* --- Ordering --- */

static int cmp_address(const void *a, const void *b)
{
    uintptr_t x = *(const uintptr_t *)a;
    uintptr_t y = *(const uintptr_t *)b;
    return (x > y) - (x < y);
}

static int cmp_span(const void *a, const void *b)
{
    uintptr_t x = ((const nn_span_t *)a)->start;
    uintptr_t y = ((const nn_span_t *)b)->start;
    return (x > y) - (x < y);
}

static int cmp_header_end(const void *a, const void *b)
{
    const nn_header_end_t *x = (const nn_header_end_t *)a;
    const nn_header_end_t *y = (const nn_header_end_t *)b;
    if (x->end != y->end) return (x->end > y->end) - (x->end < y->end);
    return (x->length < y->length) - (x->length > y->length);
}

static int cmp_u32(const void *a, const void *b)
{
    uint32_t x = *(const uint32_t *)a;
    uint32_t y = *(const uint32_t *)b;
    return (x > y) - (x < y);
}

/* --- Region index --- */

/* Sort the spans, number their units and allocate their bits.  The spans
 * array becomes the index's.  Returns 0 or -1. */
static int region_build(nn_region_index_t *r, nn_span_t *spans, uint32_t count, uint64_t unit)
{
    uint64_t bits = 0;
    uint32_t i;
    qsort(spans, count, sizeof(nn_span_t), cmp_span);
    for (i = 0; i < count; i++) {
        spans[i].first_bit = bits;
        bits += (uint64_t)(spans[i].end - spans[i].start) / unit;
    }
    r->spans = spans;
    r->count = count;
    r->unit = unit;
    r->bits = bitmap_new(bits);
    return r->bits ? 0 : -1;
}

/* The bit of the unit `p` starts, when `p` is a unit boundary inside a
 * span.  Returns 1 and writes *bit, or 0. */
static int region_find(const nn_region_index_t *r, uintptr_t p, uint64_t *bit)
{
    uint32_t lo = 0, hi = r->count;
    const nn_span_t *span;
    uint64_t offset;
    /* The last span starting at or below p. */
    while (lo < hi) {
        uint32_t mid = lo + (hi - lo) / 2;
        if (r->spans[mid].start <= p) lo = mid + 1;
        else hi = mid;
    }
    if (lo == 0) return 0;
    span = &r->spans[lo - 1];
    if (p >= span->end) return 0;
    offset = (uint64_t)(p - span->start);
    if (offset % r->unit != 0) return 0;
    *bit = span->first_bit + offset / r->unit;
    return 1;
}

static void region_free(nn_region_index_t *r)
{
    free(r->spans);
    free(r->bits);
    memset(r, 0, sizeof(*r));
}

/* --- Object index --- */

/* Room for `count` objects and their bits.  The caller writes each
 * object's address and then sorts them (object_sort).  Returns 0 or -1. */
static int object_alloc(nn_object_index_t *o, uint32_t count)
{
    o->count = count;
    o->objects = (uintptr_t *)malloc((size_t)count * sizeof(uintptr_t) + 1);
    o->bits = bitmap_new(count);
    return o->objects && o->bits ? 0 : -1;
}

static void object_sort(nn_object_index_t *o)
{
    qsort(o->objects, o->count, sizeof(uintptr_t), cmp_address);
}

/* The position of the object at `p`.  Returns 1 and writes *pos, or 0. */
static int object_find(const nn_object_index_t *o, uintptr_t p, uint32_t *pos)
{
    uint32_t lo = 0, hi = o->count;
    while (lo < hi) {
        uint32_t mid = lo + (hi - lo) / 2;
        if (o->objects[mid] < p) lo = mid + 1;
        else if (o->objects[mid] > p) hi = mid;
        else { *pos = mid; return 1; }
    }
    return 0;
}

static void object_free(nn_object_index_t *o)
{
    free(o->objects);
    free(o->bits);
    memset(o, 0, sizeof(*o));
}

/* --- Building the index --- */

static int index_thunks(void)
{
    uint32_t n = nn_thunk_blocks(NULL, NULL, 0), i;
    nn_thunk_t **slots = (nn_thunk_t **)malloc((size_t)n * sizeof(nn_thunk_t *) + 1);
    uint32_t *counts = (uint32_t *)malloc((size_t)n * sizeof(uint32_t) + 1);
    nn_span_t *spans = (nn_span_t *)malloc((size_t)n * sizeof(nn_span_t) + 1);
    int status = -1;
    if (slots && counts && spans) {
        nn_thunk_blocks(slots, counts, n);
        for (i = 0; i < n; i++) {
            spans[i].start = (uintptr_t)slots[i];
            spans[i].end = spans[i].start + (uintptr_t)counts[i] * sizeof(nn_thunk_t);
            g.allocated_count[NN_STATS_THUNKS] += counts[i];
        }
        status = region_build(&g.thunks, spans, n, sizeof(nn_thunk_t));
        spans = NULL;
    }
    free(slots);
    free(counts);
    free(spans);
    g.allocated_bytes[NN_STATS_THUNKS] = g.allocated_count[NN_STATS_THUNKS] * sizeof(nn_thunk_t);
    return status;
}

static int index_pages(void)
{
    uint32_t n = nn_env_pages(NULL, NULL, 0), i;
    char **data = (char **)malloc((size_t)n * sizeof(char *) + 1);
    uint32_t *used = (uint32_t *)malloc((size_t)n * sizeof(uint32_t) + 1);
    nn_span_t *spans = (nn_span_t *)malloc((size_t)n * sizeof(nn_span_t) + 1);
    int status = -1;
    if (data && used && spans) {
        nn_env_pages(data, used, n);
        for (i = 0; i < n; i++) {
            spans[i].start = (uintptr_t)data[i];
            spans[i].end = spans[i].start + used[i];
        }
        status = region_build(&g.pages, spans, n, NN_ENV_ALIGNMENT);
        spans = NULL;
    }
    free(data);
    free(used);
    free(spans);
    return status;
}

/* Index the list headers and group them by the items array they share. */
static int index_lists(void)
{
    uint32_t count, capacity, i, k;
    nn_list_t *const *tracked = nn_list_tracked(&count, &capacity);
    nn_header_end_t *ends;
    g.overhead_bytes += (uint64_t)capacity * sizeof(nn_list_t *);
    g.allocated_count[NN_STATS_LISTS] = count;
    g.allocated_bytes[NN_STATS_LISTS] = (uint64_t)count * sizeof(nn_list_t);
    if (object_alloc(&g.lists, count) != 0) return -1;
    for (i = 0; i < count; i++) g.lists.objects[i] = (uintptr_t)tracked[i];
    object_sort(&g.lists);

    ends = (nn_header_end_t *)malloc((size_t)count * sizeof(nn_header_end_t) + 1);
    g.items = (nn_items_t *)malloc((size_t)count * sizeof(nn_items_t) + 1);
    g.list_items = (uint32_t *)malloc((size_t)count * sizeof(uint32_t) + 1);
    if (!ends || !g.items || !g.list_items) {
        free(ends);
        return -1;
    }
    for (i = 0; i < count; i++) {
        const nn_list_t *list = (const nn_list_t *)g.lists.objects[i];
        ends[i].end = (uintptr_t)list->items + (uintptr_t)list->count * sizeof(void *);
        ends[i].length = list->count;
        ends[i].pos = i;
    }
    /* By end, then longest first: each run is one array, led by the
     * header that spans all of it. */
    qsort(ends, count, sizeof(nn_header_end_t), cmp_header_end);
    k = 0;
    for (i = 0; i < count; i++) {
        if (i == 0 || ends[i].end != ends[i - 1].end) {
            g.items[k].end = ends[i].end;
            g.items[k].scanned = ends[i].end;
            g.items[k].length = ends[i].length;
            g.items_bytes += (uint64_t)ends[i].length * sizeof(void *);
            k++;
        }
        g.list_items[ends[i].pos] = k - 1;
    }
    g.item_count = k;
    g.allocated_bytes[NN_STATS_LISTS] += g.items_bytes;
    free(ends);
    return 0;
}

static int index_sets(void)
{
    uint32_t count, capacity, i;
    nn_attrset_t *const *tracked = nn_attrset_tracked(&count, &capacity);
    g.overhead_bytes += (uint64_t)capacity * sizeof(nn_attrset_t *);
    g.allocated_count[NN_STATS_ATTRSETS] = count;
    if (object_alloc(&g.sets, count) != 0) return -1;
    for (i = 0; i < count; i++) {
        g.allocated_bytes[NN_STATS_ATTRSETS] += nn_attrset_bytes(tracked[i]);
        g.sets.objects[i] = (uintptr_t)tracked[i];
    }
    object_sort(&g.sets);
    return 0;
}

static int index_lambdas(void)
{
    uint32_t count, capacity, i;
    nn_lambda_t *const *tracked = nn_lambda_tracked(&count, &capacity);
    g.overhead_bytes += (uint64_t)capacity * sizeof(nn_lambda_t *);
    g.allocated_count[NN_STATS_LAMBDAS] = count;
    if (object_alloc(&g.lambdas, count) != 0) return -1;
    for (i = 0; i < count; i++) {
        g.allocated_bytes[NN_STATS_LAMBDAS] += nn_lambda_bytes(tracked[i]);
        g.lambdas.objects[i] = (uintptr_t)tracked[i];
    }
    object_sort(&g.lambdas);
    return 0;
}

static int index_primop_apps(void)
{
    uint32_t count, capacity, i;
    nn_primop_app_t *const *tracked = nn_primop_app_tracked(&count, &capacity);
    g.overhead_bytes += (uint64_t)capacity * sizeof(nn_primop_app_t *);
    g.allocated_count[NN_STATS_PRIMOP_APPS] = count;
    if (object_alloc(&g.primop_apps, count) != 0) return -1;
    for (i = 0; i < count; i++) {
        g.allocated_bytes[NN_STATS_PRIMOP_APPS] += nn_primop_app_bytes(tracked[i]);
        g.primop_apps.objects[i] = (uintptr_t)tracked[i];
    }
    object_sort(&g.primop_apps);
    return 0;
}

static int index_ctxstrs(void)
{
    uint32_t count, capacity, i;
    nn_ctxstr_t *const *tracked = nn_ctxstr_tracked(&count, &capacity);
    g.overhead_bytes += (uint64_t)capacity * sizeof(nn_ctxstr_t *);
    g.allocated_count[NN_STATS_CTXSTRS] = count;
    if (object_alloc(&g.ctxstrs, count) != 0) return -1;
    for (i = 0; i < count; i++) {
        g.allocated_bytes[NN_STATS_CTXSTRS] += nn_ctxstr_bytes(tracked[i]);
        g.ctxstrs.objects[i] = (uintptr_t)tracked[i];
    }
    object_sort(&g.ctxstrs);
    return 0;
}

/* Index an arena's strings, a bit per NN_STR_ALIGNMENT bytes its blocks
 * have handed out, and add those bytes to *used unless it is NULL.
 * Returns 0 or -1. */
static int index_str_arena(nn_region_index_t *r, const nn_str_arena_t *arena, uint64_t *used)
{
    uint32_t n = nn_str_arena_blocks(arena, NULL, NULL, 0), i;
    const char **starts = (const char **)malloc((size_t)n * sizeof(char *) + 1);
    uint64_t *block_used = (uint64_t *)malloc((size_t)n * sizeof(uint64_t) + 1);
    nn_span_t *spans = (nn_span_t *)malloc((size_t)n * sizeof(nn_span_t) + 1);
    int status = -1;
    if (starts && block_used && spans) {
        nn_str_arena_blocks(arena, starts, block_used, n);
        for (i = 0; i < n; i++) {
            spans[i].start = (uintptr_t)starts[i];
            spans[i].end = spans[i].start + (uintptr_t)block_used[i];
            if (used) *used += block_used[i];
        }
        status = region_build(r, spans, n, NN_STR_ALIGNMENT);
        spans = NULL;
    }
    free(starts);
    free(block_used);
    free(spans);
    return status;
}

/* The value arena's strings, counted, and the code's, placed only. */
static int index_strings(void)
{
    const nn_str_arena_t *values = nn_str_values();
    uint64_t used = 0;
    if (index_str_arena(&g.strings, values, &used) != 0) return -1;
    if (index_str_arena(&g.code_strings, nn_bc_strings(), NULL) != 0) return -1;
    g.allocated_count[NN_STATS_STRINGS] = values->count;
    g.allocated_bytes[NN_STATS_STRINGS] = used;
    g.overhead_bytes += nn_str_arena_reserved_bytes(values) - used;
    return 0;
}

/* --- Session --- */

int nn_stats_begin(void)
{
    uint64_t text, reserved;
    if (g.open) nn_stats_end();
    memset(&g, 0, sizeof(g));
    g.open = 1;

    if (index_thunks() != 0 || index_pages() != 0 || index_sets() != 0
        || index_lists() != 0 || index_lambdas() != 0 || index_primop_apps() != 0
        || index_ctxstrs() != 0
        || index_strings() != 0) {
        nn_stats_end();
        return -1;
    }

    /* The page bytes handed out are the envs, their with-scope arrays and
     * the list items arrays, every one of which nn_list_new takes from the
     * pages. */
    NN_ASSERT(g.items_bytes <= nn_env_bytes_allocated(),
              "nn_stats_begin: list items arrays outside the env pages");
    g.allocated_count[NN_STATS_ENVS] = counted_envs();
    g.allocated_bytes[NN_STATS_ENVS] = nn_env_bytes_allocated() - g.items_bytes;

    nn_symbol_footprint(&text, &reserved);
    g.symbol_count = nn_symbol_count();
    g.symbol_bits = bitmap_new((uint64_t)g.symbol_count + 1);
    if (!g.symbol_bits) {
        nn_stats_end();
        return -1;
    }
    g.allocated_count[NN_STATS_SYMBOLS] = g.symbol_count;
    g.allocated_bytes[NN_STATS_SYMBOLS] = text;

    g.overhead_bytes += nn_thunk_reserved_bytes() - g.allocated_bytes[NN_STATS_THUNKS]
                      + nn_env_reserved_bytes() - nn_env_bytes_allocated()
                      + reserved - text;
    g.code_bytes = nn_bc_reserved_bytes();
    return 0;
}

void nn_stats_end(void)
{
    region_free(&g.thunks);
    region_free(&g.pages);
    object_free(&g.sets);
    object_free(&g.lists);
    object_free(&g.lambdas);
    object_free(&g.primop_apps);
    object_free(&g.ctxstrs);
    region_free(&g.strings);
    region_free(&g.code_strings);
    free(g.items);
    free(g.list_items);
    free(g.symbol_bits);
    free(g.work);
    memset(&g, 0, sizeof(g));
}

/* --- What is allocated --- */

void nn_stats_allocated(uint64_t *counts, uint64_t *bytes)
{
    memcpy(counts, g.allocated_count, sizeof(g.allocated_count));
    memcpy(bytes, g.allocated_bytes, sizeof(g.allocated_bytes));
}

uint64_t nn_stats_overhead_bytes(void) { return g.overhead_bytes; }
uint64_t nn_stats_code_bytes(void)     { return g.code_bytes; }

/* --- Reaching objects --- */

static void work_push(uint8_t kind, const void *object)
{
    if (g.work_count == g.work_cap) {
        size_t cap = g.work_cap ? g.work_cap * 2 : NN_STATS_WORK_INITIAL;
        nn_work_t *grown = (nn_work_t *)realloc(g.work, cap * sizeof(nn_work_t));
        if (!grown) {
            g.failed = 1;
            return;
        }
        g.work = grown;
        g.work_cap = cap;
    }
    g.work[g.work_count].object = object;
    g.work[g.work_count].kind = kind;
    g.work_count++;
}

/* An object reached for the first time: its kind's figures and the
 * mark's own. */
static void count_reached(int kind, uint64_t bytes)
{
    g.reached_count[kind]++;
    g.reached_bytes[kind] += bytes;
    g.visited_objects++;
    g.visited_bytes += bytes;
}

/* The null, true, false and empty list cells, the builtins' cells and
 * the empty env are statics, not allocations: reached and neither counted nor entered,
 * as they hold no pointer. */
static int is_static(int kind, const void *object)
{
    if (kind == NN_STATS_THUNKS) return nn_thunk_is_static((const nn_thunk_t *)object);
    if (kind == NN_STATS_ENVS) return object == nn_env_empty();
    /* So is the empty set (nn_attrset_empty). */
    if (kind == NN_STATS_ATTRSETS) return object == nn_attrset_empty();
    return 0;
}

/* Reach an object: mark it and push it when it is new. */
static void reach(int kind, const void *object)
{
    uintptr_t p = (uintptr_t)object;
    uint64_t bit;
    uint32_t pos;
    if (!object || is_static(kind, object)) return;
    switch (kind) {
    case NN_STATS_THUNKS:
        if (!region_find(&g.thunks, p, &bit)) break;
        if (bit_set(g.thunks.bits, bit)) {
            count_reached(kind, sizeof(nn_thunk_t));
            work_push((uint8_t)kind, object);
        }
        return;
    case NN_STATS_ENVS:
        if (!region_find(&g.pages, p, &bit)) break;
        if (bit_set(g.pages.bits, bit)) {
            count_reached(kind, nn_env_bytes((const nn_env_t *)object));
            work_push((uint8_t)kind, object);
        }
        return;
    case NN_STATS_ATTRSETS:
        if (!object_find(&g.sets, p, &pos)) break;
        if (bit_set(g.sets.bits, pos)) {
            count_reached(kind, nn_attrset_bytes((const nn_attrset_t *)object));
            work_push((uint8_t)kind, object);
        }
        return;
    case NN_STATS_LISTS:
        if (!object_find(&g.lists, p, &pos)) break;
        if (bit_set(g.lists.bits, pos)) {
            count_reached(kind, sizeof(nn_list_t));
            work_push((uint8_t)kind, object);
        }
        return;
    case NN_STATS_LAMBDAS:
        if (!object_find(&g.lambdas, p, &pos)) break;
        if (bit_set(g.lambdas.bits, pos)) {
            count_reached(kind, nn_lambda_bytes((const nn_lambda_t *)object));
            work_push((uint8_t)kind, object);
        }
        return;
    case NN_STATS_PRIMOP_APPS:
        if (!object_find(&g.primop_apps, p, &pos)) break;
        if (bit_set(g.primop_apps.bits, pos)) {
            count_reached(kind, nn_primop_app_bytes((const nn_primop_app_t *)object));
            work_push((uint8_t)kind, object);
        }
        return;
    case NN_STATS_CTXSTRS:
        if (!object_find(&g.ctxstrs, p, &pos)) break;
        if (bit_set(g.ctxstrs.bits, pos)) {
            count_reached(kind, nn_ctxstr_bytes((const nn_ctxstr_t *)object));
            work_push((uint8_t)kind, object);
        }
        return;
    default:
        break;
    }
    g.unplaced++;
}

/* Reach a symbol.  0 is no symbol (an unnamed formal set). */
static void reach_symbol(uint32_t sym)
{
    if (sym == NN_SYMBOL_INVALID) return;
    if (sym > g.symbol_count) {
        g.unplaced++;
        return;
    }
    if (bit_set(g.symbol_bits, sym)) {
        g.reached_count[NN_STATS_SYMBOLS]++;
        g.reached_bytes[NN_STATS_SYMBOLS] += (uint64_t)nn_symbol_len(sym) + 1;
    }
}

/* Reach a string.  NULL is no string (a context element without an
 * output).  The empty string is a static, reached and not counted, and a
 * string of the code's is placed and not counted, as code is not marked. */
static void reach_string(const nn_str_t *s)
{
    uintptr_t p = (uintptr_t)s;
    uint64_t bit;
    if (!s || s == nn_str_empty()) return;
    if (region_find(&g.strings, p, &bit)) {
        if (bit_set(g.strings.bits, bit)) {
            g.reached_count[NN_STATS_STRINGS]++;
            g.reached_bytes[NN_STATS_STRINGS] += nn_str_object_bytes(s);
        }
        return;
    }
    if (region_find(&g.code_strings, p, &bit)) return;
    g.unplaced++;
}

/* Reach an env's with-scope array of `n` entries, which several envs may
 * share; 1 when it is new and its entries are to be followed. */
static int reach_array(void *const *array, uint32_t n)
{
    uint64_t bit;
    uint64_t bytes = (uint64_t)n * sizeof(void *);
    if (!array || n == 0) return 0;
    if (!region_find(&g.pages, (uintptr_t)array, &bit)) {
        g.unplaced++;
        return 0;
    }
    if (!bit_set(g.pages.bits, bit)) return 0;
    g.reached_bytes[NN_STATS_ENVS] += bytes;
    g.visited_objects++;
    g.visited_bytes += bytes;
    return 1;
}

/* --- Entering objects --- */

static void enter_thunk(const nn_thunk_t *t)
{
    uint8_t state = nn_thunk_state(t);
    if (state == NN_THUNK_PENDING || state == NN_THUNK_BLACKHOLE) {
        /* A blackhole keeps its pending env (nn_thunk_mark_pending puts it
         * back to work), so it holds the env as a pending thunk does. */
        reach(NN_STATS_ENVS, nn_thunk_payload(t));
        return;
    }
    if (state != NN_THUNK_COMPUTED) {
        g.unplaced++;
        return;
    }
    switch (nn_thunk_value_tag(t)) {
    case NN_VALUE_INT:
    case NN_VALUE_FLOAT:
    case NN_VALUE_BOOL:
    case NN_VALUE_NULL:
        break;
    case NN_VALUE_STR:
        reach_string((const nn_str_t *)nn_thunk_get_str(t));
        break;
    case NN_VALUE_PATH:
        reach_string((const nn_str_t *)nn_thunk_get_path(t));
        break;
    case NN_VALUE_LIST:
        reach(NN_STATS_LISTS, nn_thunk_get_list(t));
        break;
    case NN_VALUE_ATTRS:
        reach(NN_STATS_ATTRSETS, nn_thunk_get_attrs(t));
        break;
    case NN_VALUE_CTXSTR:
        reach(NN_STATS_CTXSTRS, nn_thunk_get_ctxstr(t));
        break;
    case NN_VALUE_LAMBDA:
        reach(NN_STATS_LAMBDAS, nn_thunk_get_lambda(t));
        break;
    case NN_VALUE_PRIMOP:
        break;
    case NN_VALUE_PRIMOP_APP:
        reach(NN_STATS_PRIMOP_APPS, nn_thunk_get_primop_app(t));
        break;
    default:
        g.unplaced++;
        break;
    }
}

/* An env's slots are part of it, counted in its bytes when it is reached;
 * its with-scope array is an allocation of its own. */
static void enter_env(const nn_env_t *env)
{
    uint32_t i;
    const nn_env_scopes_t *scopes = nn_env_scopes(env);
    uint32_t slot_count = nn_env_slot_count(env);
    for (i = 0; i < slot_count; i++) reach(NN_STATS_THUNKS, env->slots[i]);
    reach(NN_STATS_ENVS, nn_env_parent(env));
    if (!scopes) return;
    reach(NN_STATS_ATTRSETS, scopes->lazy_scope);
    if (reach_array(scopes->with_scopes, scopes->with_count)) {
        for (i = 0; i < scopes->with_count; i++) {
            uintptr_t entry = (uintptr_t)scopes->with_scopes[i];
            if (entry & NN_ENV_LAZY_WITH)
                reach(NN_STATS_THUNKS, (const void *)(entry & ~NN_ENV_LAZY_WITH));
            else
                reach(NN_STATS_ATTRSETS, (const void *)entry);
        }
    }
}

static void enter_attrset(const nn_attrset_t *set)
{
    uint32_t n = nn_attrset_size(set), i;
    const nn_symbol_t *keys = nn_attrset_keys_ptr(set);
    for (i = 0; i < n; i++) {
        reach_symbol(keys[i]);
        reach(NN_STATS_THUNKS, nn_attrset_get_value(set, i));
    }
}

static void enter_list(const nn_list_t *list)
{
    uint32_t pos, i, below;
    nn_items_t *items;
    uintptr_t start = (uintptr_t)list->items;
    if (!object_find(&g.lists, (uintptr_t)list, &pos)) return;
    items = &g.items[g.list_items[pos]];
    if (items->scanned == items->end) {
        uint64_t bytes = (uint64_t)items->length * sizeof(void *);
        g.reached_bytes[NN_STATS_LISTS] += bytes;
        g.visited_objects++;
        g.visited_bytes += bytes;
    }
    if (start >= items->scanned) return;
    below = (uint32_t)((items->scanned - start) / sizeof(void *));
    for (i = 0; i < below; i++) reach(NN_STATS_THUNKS, list->items[i]);
    items->scanned = start;
}

static void enter_lambda(const nn_lambda_t *lam)
{
    uint32_t i;
    uint32_t formals = nn_lambda_formal_count(lam);
    reach(NN_STATS_ENVS, lam->env);
    reach_symbol(lam->name_sym);
    reach_symbol(lam->lambda_name_sym);
    for (i = 0; i < formals && lam->entries; i++) reach_symbol(lam->entries[i].name_sym);
}

static void enter_primop_app(const nn_primop_app_t *app)
{
    uint16_t i;
    for (i = 0; i < app->taken; i++) reach(NN_STATS_THUNKS, app->args[i]);
}

static void enter_ctxstr(const nn_ctxstr_t *s)
{
    uint32_t i;
    reach_string(s->text);
    for (i = 0; i < s->ctx_count; i++) {
        reach_string(s->ctx[i].sp_hash);
        reach_string(s->ctx[i].sp_name);
        reach_string(s->ctx[i].output);
    }
}

/* --- The mark --- */

void nn_stats_root_thunk(const void *thunk)   { if (g.open) reach(NN_STATS_THUNKS, thunk); }
void nn_stats_root_env(const void *env)       { if (g.open) reach(NN_STATS_ENVS, env); }
void nn_stats_root_attrset(const void *set)   { if (g.open) reach(NN_STATS_ATTRSETS, set); }
void nn_stats_root_list(const void *list)     { if (g.open) reach(NN_STATS_LISTS, list); }
void nn_stats_root_string(const void *str)    { if (g.open) reach_string((const nn_str_t *)str); }
void nn_stats_root_ctxstr(const void *ctxstr) { if (g.open) reach(NN_STATS_CTXSTRS, ctxstr); }

void nn_stats_root_registered(void)
{
    uint32_t i, n;
    if (!g.open) return;
    for (i = 0, n = nn_roots_thunk_count(); i < n; i++) reach(NN_STATS_THUNKS, nn_roots_thunk(i));
    for (i = 0, n = nn_roots_env_count(); i < n; i++) reach(NN_STATS_ENVS, nn_roots_env(i));
}

int nn_stats_mark(void)
{
    if (!g.open) return -1;
    while (g.work_count > 0) {
        nn_work_t item = g.work[--g.work_count];
        switch (item.kind) {
        case NN_STATS_THUNKS:   enter_thunk((const nn_thunk_t *)item.object); break;
        case NN_STATS_ENVS:     enter_env((const nn_env_t *)item.object); break;
        case NN_STATS_ATTRSETS: enter_attrset((const nn_attrset_t *)item.object); break;
        case NN_STATS_LISTS:    enter_list((const nn_list_t *)item.object); break;
        case NN_STATS_LAMBDAS:  enter_lambda((const nn_lambda_t *)item.object); break;
        case NN_STATS_PRIMOP_APPS: enter_primop_app((const nn_primop_app_t *)item.object); break;
        case NN_STATS_CTXSTRS:  enter_ctxstr((const nn_ctxstr_t *)item.object); break;
        default: g.unplaced++; break;
        }
    }
    return g.failed ? -1 : 0;
}

void nn_stats_reached(uint64_t *counts, uint64_t *bytes)
{
    memcpy(counts, g.reached_count, sizeof(g.reached_count));
    memcpy(bytes, g.reached_bytes, sizeof(g.reached_bytes));
}

void nn_stats_visited(uint64_t *objects, uint64_t *bytes, uint64_t *unplaced)
{
    *objects = g.visited_objects;
    *bytes = g.visited_bytes;
    *unplaced = g.unplaced;
}

int nn_stats_reached_set_sizes(uint32_t *sizes, uint64_t *counts, uint32_t max,
                               uint32_t *distinct)
{
    uint32_t reached = 0, i;
    uint32_t *all = (uint32_t *)malloc((size_t)g.sets.count * sizeof(uint32_t) + 1);
    *distinct = 0;
    if (!all) return -1;
    for (i = 0; i < g.sets.count; i++) {
        if (bit_get(g.sets.bits, i))
            all[reached++] = nn_attrset_size((const nn_attrset_t *)g.sets.objects[i]);
    }
    qsort(all, reached, sizeof(uint32_t), cmp_u32);
    for (i = 0; i < reached; i++) {
        if (i == 0 || all[i] != all[i - 1]) {
            if (*distinct < max) {
                sizes[*distinct] = all[i];
                counts[*distinct] = 0;
            }
            (*distinct)++;
        }
        if (*distinct <= max) counts[*distinct - 1]++;
    }
    free(all);
    return 0;
}
