/*
 * nn_env.c - C-native evaluation environments.
 *
 * Arena-allocated envs (nn_env.h), with-scope arrays, list items arrays
 * and deferred applications' cells.  Pages are NN_ENV_PAGE_SIZE bytes, or
 * a request's own size when it is larger, allocated on demand as a linked
 * list.  Each allocation bumps a pointer within the current page.
 *
 * All memory is freed in bulk via nn_env_destroy().
 */

#include "nn_env.h"
#include "nn_assert.h"
#include "nn_region.h"
#include "nn_stats.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* --- Internal types --- */

struct nn_env_page {
    struct nn_env_page *next;     /* NULL for newest page */
    uint32_t            used;     /* one past the last byte ever handed out */
    uint32_t            capacity; /* usable bytes in this page */
    /* Flexible array member: page data follows */
    char                data[];
};

/* --- Alignment --- */

/* C99 has no alignof: a type's alignment is the offset it takes after a
 * char in a struct.  Every allocation starts at an NN_ENV_ALIGNMENT
 * boundary of a page's data, which malloc aligns for any type.  An env's
 * strictest member is a slot, a void *, so the env, its slots, and the
 * parent and scopes that follow the slots a whole number of pointers on
 * are aligned when the boundary is a multiple of a pointer's alignment
 * and the parent and scopes need no more than a pointer does. */
struct nn_env_align_pointer { char c; void *p; };
struct nn_env_align_env_pointer { char c; nn_env_t *p; };
struct nn_env_align_scopes { char c; nn_env_scopes_t s; };
struct nn_env_align_word { char c; uintptr_t w; };

typedef char nn_env_page_data_check_[
    (offsetof(struct nn_env_page, data) % NN_ENV_ALIGNMENT == 0) ? 1 : -1];
typedef char nn_env_pointer_align_check_[
    (NN_ENV_ALIGNMENT % offsetof(struct nn_env_align_pointer, p) == 0) ? 1 : -1];
typedef char nn_env_env_pointer_align_check_[
    (offsetof(struct nn_env_align_env_pointer, p) <= offsetof(struct nn_env_align_pointer, p)) ? 1 : -1];
typedef char nn_env_scopes_align_check_[
    (offsetof(struct nn_env_align_scopes, s) <= offsetof(struct nn_env_align_pointer, p)) ? 1 : -1];

/* A short env keeps its slot count in the low bits of its parent's
 * address, so every env's address has them clear: one from the pages,
 * at an NN_ENV_ALIGNMENT boundary, and the static empty env, aligned as
 * its word is. */
typedef char nn_env_short_page_check_[(NN_ENV_ALIGNMENT > NN_ENV_SHORT_MASK) ? 1 : -1];
typedef char nn_env_short_static_check_[
    (offsetof(struct nn_env_align_word, w) > NN_ENV_SHORT_MASK) ? 1 : -1];
typedef char nn_env_short_max_check_[(NN_ENV_SHORT_MAX == NN_ENV_SHORT_MASK) ? 1 : -1];

/* --- Global state --- */

/* The first page and the newest, for iteration and for linking a new
 * one, and the page envs are bumped from, with its room: from the cursor
 * to below the limit, in bytes.  A page is a room of its own when it is
 * new; while the region collector runs, a room can also be lines of a
 * page it freed (nn_region_refill). */
static struct nn_env_page *g_first_page = NULL;
static struct nn_env_page *g_last_page = NULL;
static struct nn_env_page *g_current_page = NULL;
static uint32_t g_cursor = 0;
static uint32_t g_limit = 0;
static uint64_t g_app_count = 0;
static uint64_t g_app_bytes = 0;
static uint64_t g_bytes_allocated = 0;

/* The empty env: a zeroed static that nothing writes, valid in and
 * outside every init .. destroy window. */
static nn_env_t g_empty_env;

/* --- Internal helpers --- */

static struct nn_env_page *
alloc_page(uint32_t capacity)
{
    struct nn_env_page *page = (struct nn_env_page *)malloc(
        sizeof(struct nn_env_page) + (size_t)capacity);
    if (!page) return NULL;
    page->next = NULL;
    page->used = 0;
    page->capacity = capacity;
    /* Every page made becomes the one allocation bumps from. */
    if (nn_region_mode) nn_region_block(NN_REGION_ENV_PAGE, page->data, capacity, 1);
    return page;
}

static uint32_t
align_up(uint32_t n, uint32_t alignment)
{
    return (n + alignment - 1) & ~(alignment - 1);
}

/* Bump from the room the collector names next, with space for `bytes`,
 * or from a new page.  A request larger than a page gets a page of its
 * own. */
static int
refill(uint32_t bytes)
{
    const char *data;
    uint64_t start, limit;
    struct nn_env_page *page;
    if (nn_region_mode == NN_REGION_COLLECT && bytes <= NN_ENV_PAGE_SIZE) {
        /* The room ends here even if no page follows (nn_region_refill). */
        g_limit = g_cursor;
        if (nn_region_refill(NN_REGION_ENV_PAGE, g_cursor, bytes, &data, &start, &limit) == 0) {
            g_current_page = (struct nn_env_page *)(void *)(data - offsetof(struct nn_env_page, data));
            g_cursor = (uint32_t)start;
            g_limit = (uint32_t)limit;
            return 0;
        }
    }
    page = alloc_page(bytes > NN_ENV_PAGE_SIZE ? bytes : NN_ENV_PAGE_SIZE);
    if (!page) return -1;
    g_last_page->next = page;
    g_last_page = page;
    g_current_page = page;
    g_cursor = 0;
    g_limit = page->capacity;
    return 0;
}

/* Allocate raw bytes from the page-based bump allocator.
 * Returns aligned, zero-initialized memory, or NULL on overflow/OOM. */
static void *
nn_env_alloc_raw(uint32_t bytes)
{
    /* align_up would wrap for sizes near UINT32_MAX; such a request can
     * only be corrupt input, so fail it instead of wrapping small. */
    if (bytes > UINT32_MAX - (NN_ENV_ALIGNMENT - 1)) return NULL;
    bytes = align_up(bytes, NN_ENV_ALIGNMENT);

    /* No pages before nn_env_init or after nn_env_destroy.  Opening a
     * first page here instead would hand out memory the next
     * nn_env_init frees, under every pointer built from it. */
    if (!g_current_page) return NULL;

    /* cursor <= limit always holds, so limit - cursor cannot underflow;
     * the additive form (cursor + bytes > limit) overflows uint32_t for
     * near-4GB requests and would pass the check, then memset past the
     * end of a 256 KB page. */
    if (bytes > g_limit - g_cursor && refill(bytes) != 0) return NULL;

    void *result = g_current_page->data + g_cursor;
    memset(result, 0, bytes);
    g_cursor += bytes;
    if (g_cursor > g_current_page->used) g_current_page->used = g_cursor;
    g_bytes_allocated += bytes;
    return result;
}

/* An array of `count` pointers, or NULL for none or for one past the
 * allocator's uint32 byte limit. */
static void **
nn_env_alloc_pointers(uint32_t count)
{
    if (count == 0) return NULL;
    uint64_t bytes64 = (uint64_t)count * sizeof(void *);
    if (bytes64 > UINT32_MAX) return NULL;
    return (void **)nn_env_alloc_raw((uint32_t)bytes64);
}

/* --- The word (nn_env.h) --- */

/* Whether an env is short: its word is its parent and slot count. */
static int
is_short(const nn_env_t *env)
{
    return (env->word & NN_ENV_SHORT_MASK) != 0;
}

static uint32_t
slot_count_of(const nn_env_t *env)
{
    return is_short(env) ? (uint32_t)(env->word & NN_ENV_SHORT_MASK)
                         : (uint32_t)(env->word >> NN_ENV_COUNT_SHIFT);
}

/* The fields a header says follow the slots; none for a short env. */
static uintptr_t
fields_of(const nn_env_t *env)
{
    return is_short(env) ? 0 : env->word & (NN_ENV_PARENT | NN_ENV_SCOPES);
}

/* The bytes an env of this shape takes: its word, its slots and the
 * fields its header names (nn_env.h), none for a short env. */
static uint64_t
env_shape_bytes(uint32_t slot_count, uintptr_t fields)
{
    return (uint64_t)sizeof(nn_env_t)
         + (uint64_t)slot_count * sizeof(void *)
         + ((fields & NN_ENV_PARENT) ? sizeof(nn_env_t *) : 0)
         + ((fields & NN_ENV_SCOPES) ? sizeof(nn_env_scopes_t) : 0);
}

/* The first byte past an env's slots, where a header's parent and scopes
 * go. */
static char *
tail_of(nn_env_t *env)
{
    return (char *)(void *)(env->slots + slot_count_of(env));
}

static const char *
const_tail_of(const nn_env_t *env)
{
    return (const char *)(const void *)(env->slots + slot_count_of(env));
}

/* Where the scopes of a header with NN_ENV_SCOPES start: after its parent
 * when it has one. */
static size_t
scopes_offset(uintptr_t fields)
{
    return (fields & NN_ENV_PARENT) ? sizeof(nn_env_t *) : 0;
}

static nn_env_t *
parent_of(const nn_env_t *env)
{
    if (is_short(env)) return (nn_env_t *)(env->word & ~NN_ENV_SHORT_MASK);
    if (!(env->word & NN_ENV_PARENT)) return NULL;
    return *(nn_env_t *const *)(const void *)const_tail_of(env);
}

/* An env of `slot_count` NULL slots under `parent` (or none), with the
 * scopes when `scopes` is non-NULL: short when nn_env.h says it is, and
 * otherwise a header and the fields it names.  It counts once under
 * `site` while the census counts sites (nn_stats.h), with its slots and
 * its bytes, which are a whole number of NN_ENV_ALIGNMENT units. */
static nn_env_t *
env_alloc(uint32_t slot_count, nn_env_t *parent, const nn_env_scopes_t *scopes,
          uint32_t site)
{
    int short_env = slot_count >= 1 && slot_count <= NN_ENV_SHORT_MAX && !scopes;
    uintptr_t fields = short_env ? 0
                     : (parent ? NN_ENV_PARENT : 0) | (scopes ? NN_ENV_SCOPES : 0);
    uint64_t bytes = env_shape_bytes(slot_count, fields);
    /* A parent's low bits would read back as a slot count.  No env's
     * address has them set, so a parent that does is corrupt state. */
    NN_ASSERT(((uintptr_t)parent & NN_ENV_SHORT_MASK) == 0,
              "env_alloc: parent address with NN_ENV_SHORT_MASK bits set");
    if (bytes > UINT32_MAX) return NULL;
    nn_env_t *env = (nn_env_t *)nn_env_alloc_raw((uint32_t)bytes);
    if (!env) return NULL;
    if (nn_stats_counting) nn_stats_count_env(site, slot_count, bytes);
    if (short_env) {
        env->word = (uintptr_t)parent | (uintptr_t)slot_count;
        return env;
    }
    env->word = ((uintptr_t)slot_count << NN_ENV_COUNT_SHIFT) | fields;
    if (parent) *(nn_env_t **)(void *)tail_of(env) = parent;
    if (scopes)
        *(nn_env_scopes_t *)(void *)(tail_of(env) + scopes_offset(fields)) = *scopes;
    return env;
}

/* --- Lifecycle --- */

void
nn_env_init(void)
{
    g_first_page = alloc_page(NN_ENV_PAGE_SIZE);
    if (!g_first_page) { fprintf(stderr, "nn_env_init: page alloc failed\n"); abort(); }
    g_last_page = g_first_page;
    g_current_page = g_first_page;
    g_cursor = 0;
    g_limit = g_first_page->capacity;
    g_app_count = 0;
    g_app_bytes = 0;
    g_bytes_allocated = 0;
}

int
nn_env_live(void)
{
    return g_first_page != NULL;
}

void
nn_env_destroy(void)
{
    struct nn_env_page *page = g_first_page;
    while (page) {
        struct nn_env_page *next = page->next;
        free(page);
        page = next;
    }
    g_first_page = NULL;
    g_last_page = NULL;
    g_current_page = NULL;
    g_cursor = 0;
    g_limit = 0;
    g_app_count = 0;
    g_app_bytes = 0;
    g_bytes_allocated = 0;
}

/* --- List items --- */

void **
nn_env_alloc_items(uint32_t count)
{
    return nn_env_alloc_pointers(count);
}

/* --- Deferred applications' cells --- */

void **
nn_env_alloc_app_cells(uint32_t count)
{
    void **cells = nn_env_alloc_pointers(count);
    if (cells) {
        g_app_count++;
        g_app_bytes += (uint64_t)count * sizeof(void *);
    }
    return cells;
}

uint64_t
nn_env_app_count(void)
{
    return g_app_count;
}

uint64_t
nn_env_app_bytes(void)
{
    return g_app_bytes;
}

/* --- With-scopes arrays --- */

/* An array of `count` with-scope entries for an env at `site`, all NULL,
 * or NULL for none, for one past the allocator's uint32 byte limit or on
 * OOM. */
static void **
nn_env_alloc_with_scopes(uint32_t count, uint32_t site)
{
    void **withs = nn_env_alloc_pointers(count);
    /* nn_env_alloc_pointers refused every count whose bytes pass
     * UINT32_MAX, and a whole number of pointers is a whole number of
     * NN_ENV_ALIGNMENT units. */
    if (withs && nn_stats_counting)
        nn_stats_count_env_bytes(site, (uint64_t)count * sizeof(void *));
    return withs;
}

/* --- Env constructors --- */

nn_env_t *
nn_env_empty(void)
{
    return &g_empty_env;
}

nn_env_t *
nn_env_new(uint32_t slot_count, nn_env_t *parent, uint32_t site)
{
    return env_alloc(slot_count, parent, NULL, site);
}

nn_env_t *
nn_env_new_scoped(uint32_t slot_count, nn_env_t *parent,
                  void *name_table,
                  void **with_scopes, uint32_t with_count,
                  uint32_t site)
{
    nn_env_scopes_t scopes;
    /* A count without its array would send nn_env_push_with's copy and
     * every with lookup through NULL. */
    NN_ASSERT(with_count == 0 || with_scopes != NULL,
              "nn_env_new_scoped: a with-scope count without an array");
    scopes.name_table = name_table;
    scopes.with_scopes = with_scopes;
    scopes.with_count = with_count;
    return env_alloc(slot_count, parent, &scopes, site);
}

nn_env_t *
nn_env_push_with(nn_env_t *base, void *scope, uint32_t site)
{
    const nn_env_t *owner = nn_env_with_owner(base);
    uint32_t outer = owner ? nn_env_with_count(owner) : 0;

    /* Every with-scopes array is one nn_env_alloc_with_scopes made, of
     * fewer than UINT32_MAX entries, so the increment does not wrap. */
    NN_ASSERT(outer < UINT32_MAX,
              "nn_env_push_with: with-scope count at UINT32_MAX");
    uint32_t new_count = outer + 1;
    void **new_withs = nn_env_alloc_with_scopes(new_count, site);
    if (!new_withs) return NULL;

    /* New scope at index 0 (innermost), then the ones base sees. */
    new_withs[0] = scope;
    if (outer > 0) {
        memcpy(new_withs + 1, nn_env_with_scopes(owner),
               (size_t)outer * sizeof(void *));
    }

    uint32_t slot_count = slot_count_of(base);
    nn_env_t *env = nn_env_new_scoped(slot_count, parent_of(base),
                                      nn_env_name_table(base),
                                      new_withs, new_count, site);
    if (!env) return NULL;
    if (slot_count > 0) {
        memcpy(env->slots, base->slots, (size_t)slot_count * sizeof(void *));
    }
    return env;
}

void
nn_env_resolve_with(void **with_scopes, uint32_t idx, void *set)
{
    with_scopes[idx] = set;
    if (nn_region_mode) nn_region_record_with(&with_scopes[idx]);
}

/* --- Accessors --- */

void **
nn_env_slots(nn_env_t *env)
{
    return env->slots;
}

uint32_t
nn_env_slot_count(const nn_env_t *env)
{
    return slot_count_of(env);
}

nn_env_t *
nn_env_parent(const nn_env_t *env)
{
    return parent_of(env);
}

const nn_env_scopes_t *
nn_env_scopes(const nn_env_t *env)
{
    uintptr_t fields = fields_of(env);
    if (!(fields & NN_ENV_SCOPES)) return NULL;
    return (const nn_env_scopes_t *)(const void *)
        (const_tail_of(env) + scopes_offset(fields));
}

void *
nn_env_name_table(const nn_env_t *env)
{
    const nn_env_scopes_t *scopes = nn_env_scopes(env);
    return scopes ? scopes->name_table : NULL;
}

void **
nn_env_with_scopes(const nn_env_t *env)
{
    const nn_env_scopes_t *scopes = nn_env_scopes(env);
    return scopes ? scopes->with_scopes : NULL;
}

uint32_t
nn_env_with_count(const nn_env_t *env)
{
    const nn_env_scopes_t *scopes = nn_env_scopes(env);
    return scopes ? scopes->with_count : 0;
}

uint32_t
nn_env_bytes(const nn_env_t *env)
{
    /* env_alloc refused every shape past UINT32_MAX bytes. */
    return (uint32_t)env_shape_bytes(slot_count_of(env), fields_of(env));
}

/* --- Lookup --- */

void *
nn_env_lookup_resolved(const nn_env_t *env, uint32_t level, uint32_t idx)
{
    if (env == NULL) {
        return NULL;
    }
    while (level > 0) {
        const nn_env_t *parent = parent_of(env);
        if (parent == NULL) {
            return NULL;
        }
        env = parent;
        level--;
    }
    if (idx >= slot_count_of(env)) {
        return NULL;
    }
    return env->slots[idx];
}

nn_env_t *
nn_env_with_owner(nn_env_t *env)
{
    while (env && nn_env_with_count(env) == 0) {
        env = parent_of(env);
    }
    return env;
}

/* --- Diagnostics --- */

uint64_t
nn_env_bytes_allocated(void)
{
    return g_bytes_allocated;
}

/* --- Census --- */

uint32_t
nn_env_pages(char **data, uint32_t *used, uint32_t max)
{
    uint32_t n = 0;
    struct nn_env_page *page;
    for (page = g_first_page; page; page = page->next) {
        if (n < max) {
            data[n] = page->data;
            used[n] = page->used;
        }
        n++;
    }
    return n;
}

uint64_t
nn_env_cursor(void)
{
    return g_cursor;
}

uint64_t
nn_env_reserved_bytes(void)
{
    uint64_t bytes = 0;
    struct nn_env_page *page;
    for (page = g_first_page; page; page = page->next) {
        bytes += sizeof(struct nn_env_page) + (uint64_t)page->capacity;
    }
    return bytes;
}
