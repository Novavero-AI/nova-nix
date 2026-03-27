/*
 * nn_env.c — Page-based bump allocator for environment slot arrays.
 *
 * Pages are 256 KB each, allocated on demand as a linked list.
 * Each allocation bumps a pointer within the current page.  When
 * the current page is full, a new page is allocated.
 *
 * For oversized requests (> page capacity), a dedicated page is
 * allocated with exactly the needed capacity.
 *
 * All memory is freed in bulk via nn_env_destroy().
 */

#include "nn_env.h"

#include <stdlib.h>
#include <string.h>

/* --- Constants --- */

#define NN_ENV_PAGE_SIZE (256u * 1024u)  /* 256 KB per page */
#define NN_ENV_PTR_SIZE  sizeof(void*)
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

/* --- Lifecycle --- */

void
nn_env_init(void)
{
    /* Destroy existing allocator if re-initializing */
    if (g_first_page) {
        nn_env_destroy();
    }

    g_first_page = alloc_page(NN_ENV_PAGE_SIZE);
    g_current_page = g_first_page;
    g_total_bytes = 0;
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

/* --- Allocation --- */

void **
nn_env_alloc_slots(uint32_t count)
{
    if (count == 0) return NULL;

    uint32_t bytes = align_up(count * (uint32_t)NN_ENV_PTR_SIZE, NN_ENV_ALIGN);

    /* Current page full — allocate a new one */
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

    void **result = (void **)(g_current_page->data + g_current_page->used);
    memset(result, 0, bytes);  /* zero-initialize (NULL pointers) */
    g_current_page->used += bytes;
    g_total_bytes += bytes;
    return result;
}

/* --- Diagnostics --- */

uint64_t
nn_env_bytes_allocated(void)
{
    return g_total_bytes;
}
