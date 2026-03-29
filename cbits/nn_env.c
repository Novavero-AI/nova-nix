/*
 * nn_env.c — C-native evaluation environments.
 *
 * Arena-allocated nn_env_t structs and backing arrays.  Pages are
 * 256 KB each, allocated on demand as a linked list.  Each allocation
 * bumps a pointer within the current page.
 *
 * All memory is freed in bulk via nn_env_destroy().
 */

#include "nn_env.h"
#include "nn_assert.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* --- Constants --- */

#define NN_ENV_PAGE_SIZE (256u * 1024u)  /* 256 KB per page */
#define NN_ENV_ALIGN     8u              /* 8-byte alignment */

/* --- Internal types --- */

struct nn_env_page {
    struct nn_env_page *next;     /* NULL for newest page */
    uint32_t            used;     /* bytes used in this page */
    uint32_t            capacity; /* usable bytes in this page */
    /* Flexible array member: page data follows */
    char                data[];
};

/* --- Global state --- */

static struct nn_env_page *g_first_page = NULL;
static struct nn_env_page *g_current_page = NULL;
static uint64_t g_total_bytes = 0;

/* Global empty env — initialized in nn_env_init, valid until destroy. */
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
    return page;
}

static uint32_t
align_up(uint32_t n, uint32_t alignment)
{
    return (n + alignment - 1) & ~(alignment - 1);
}

/* Allocate raw bytes from the page-based bump allocator.
 * Returns aligned, zero-initialized memory. */
static void *
nn_env_alloc_raw(uint32_t bytes)
{
    bytes = align_up(bytes, NN_ENV_ALIGN);

    if (!g_current_page || g_current_page->used + bytes > g_current_page->capacity) {
        uint32_t page_cap = bytes > NN_ENV_PAGE_SIZE ? bytes : NN_ENV_PAGE_SIZE;
        struct nn_env_page *page = alloc_page(page_cap);
        if (!page) return NULL;
        if (g_current_page) {
            g_current_page->next = page;
        } else {
            g_first_page = page;
        }
        g_current_page = page;
    }

    void *result = g_current_page->data + g_current_page->used;
    memset(result, 0, bytes);
    g_current_page->used += bytes;
    g_total_bytes += bytes;
    return result;
}

/* --- Lifecycle --- */

void
nn_env_init(void)
{
    if (g_first_page) {
        nn_env_destroy();
    }

    g_first_page = alloc_page(NN_ENV_PAGE_SIZE);
    if (!g_first_page) { fprintf(stderr, "nn_env_init: page alloc failed\n"); abort(); }
    g_current_page = g_first_page;
    g_total_bytes = 0;

    /* Initialize global empty env */
    memset(&g_empty_env, 0, sizeof(g_empty_env));
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
    g_current_page = NULL;
    g_total_bytes = 0;
}

/* --- Slot allocation --- */

void **
nn_env_alloc_slots(uint32_t count)
{
    if (count == 0) return NULL;
    uint64_t bytes64 = (uint64_t)count * sizeof(void *);
    if (bytes64 > UINT32_MAX) return NULL;
    uint32_t bytes = (uint32_t)bytes64;
    return (void **)nn_env_alloc_raw(bytes);
}

/* --- Env constructors --- */

nn_env_t *
nn_env_empty(void)
{
    return &g_empty_env;
}

nn_env_t *
nn_env_new(void **slots, uint32_t slot_count,
           void *lazy_scope, nn_env_t *parent,
           void **with_scopes, uint16_t with_count)
{
    nn_env_t *env = (nn_env_t *)nn_env_alloc_raw((uint32_t)sizeof(nn_env_t));
    if (!env) return NULL;
    env->slots = slots;
    env->slot_count = slot_count;
    env->lazy_scope = lazy_scope;
    env->parent = parent;
    env->with_scopes = with_scopes;
    env->with_count = with_count;
    return env;
}

nn_env_t *
nn_env_from_slots(void **slots, uint32_t slot_count,
                  nn_env_t *parent)
{
    nn_env_t *env = (nn_env_t *)nn_env_alloc_raw((uint32_t)sizeof(nn_env_t));
    if (!env) return NULL;
    env->slots = slots;
    env->slot_count = slot_count;
    env->lazy_scope = NULL;
    env->parent = parent;
    env->with_scopes = parent ? parent->with_scopes : NULL;
    env->with_count = parent ? parent->with_count : 0;
    return env;
}

nn_env_t *
nn_env_push_with(nn_env_t *base, void *scope)
{
    if (base->with_count >= UINT16_MAX) return (nn_env_t *)base;
    uint16_t new_count = base->with_count + 1;
    void **new_withs = (void **)nn_env_alloc_raw(
        (uint32_t)new_count * (uint32_t)sizeof(void *));
    if (!new_withs) return NULL;

    /* New scope at index 0 (innermost) */
    new_withs[0] = scope;
    /* Copy existing with-scopes after it */
    if (base->with_count > 0 && base->with_scopes) {
        memcpy(new_withs + 1, base->with_scopes,
               (size_t)base->with_count * sizeof(void *));
    }

    nn_env_t *env = (nn_env_t *)nn_env_alloc_raw((uint32_t)sizeof(nn_env_t));
    if (!env) return NULL;
    env->slots = base->slots;
    env->slot_count = base->slot_count;
    env->lazy_scope = base->lazy_scope;
    env->parent = base->parent;
    env->with_scopes = new_withs;
    env->with_count = new_count;
    return env;
}

nn_env_t *
nn_env_new_minimal(void **slots, uint32_t slot_count)
{
    nn_env_t *env = (nn_env_t *)nn_env_alloc_raw((uint32_t)sizeof(nn_env_t));
    if (!env) return NULL;
    env->slots = slots;
    env->slot_count = slot_count;
    /* lazy_scope, parent, with_scopes, with_count already zero from alloc_raw */
    return env;
}

/* --- Accessors --- */

void **
nn_env_slots(const nn_env_t *env)
{
    return env->slots;
}

uint32_t
nn_env_slot_count(const nn_env_t *env)
{
    return env->slot_count;
}

void *
nn_env_lazy_scope(const nn_env_t *env)
{
    return env->lazy_scope;
}

nn_env_t *
nn_env_parent(const nn_env_t *env)
{
    return env->parent;
}

void **
nn_env_with_scopes(const nn_env_t *env)
{
    return env->with_scopes;
}

uint16_t
nn_env_with_count(const nn_env_t *env)
{
    return env->with_count;
}

/* --- Lookup --- */

void *
nn_env_lookup_resolved(const nn_env_t *env, int level, int idx)
{
    NN_ASSERT(env != NULL, "nn_env_lookup_resolved: NULL env");
    while (level > 0) {
        env = env->parent;
        level--;
    }
    NN_ASSERT(idx >= 0 && (uint32_t)idx < env->slot_count, "nn_env_lookup_resolved: idx out of bounds");
    return env->slots[idx];
}

void *
nn_env_root_scope(const nn_env_t *env)
{
    while (env->parent) {
        env = env->parent;
    }
    return env->lazy_scope;
}

/* --- With-scopes array allocation --- */

void **
nn_env_alloc_with_scopes(uint16_t count)
{
    if (count == 0) return NULL;
    uint64_t bytes64 = (uint64_t)count * sizeof(void *);
    if (bytes64 > UINT32_MAX) return NULL;
    uint32_t bytes = (uint32_t)bytes64;
    return (void **)nn_env_alloc_raw(bytes);
}

/* --- Diagnostics --- */

uint64_t
nn_env_bytes_allocated(void)
{
    return g_total_bytes;
}
