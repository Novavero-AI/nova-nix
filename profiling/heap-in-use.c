/*
 * heap-in-use.c - The bytes the C allocator has handed out and not taken
 * back, for the attrset-memory benchmark, chunk headers and alignment
 * included: glibc's mallinfo2 over every arena and the chunks it maps
 * directly, or macOS's default malloc zone.  Elsewhere the answer is
 * HEAP_IN_USE_UNKNOWN.
 */

#include <stdint.h>
#include <stdlib.h>

#if defined(__GLIBC__) && (__GLIBC__ > 2 || (__GLIBC__ == 2 && __GLIBC_MINOR__ >= 33))
#include <malloc.h>
#define HEAP_IN_USE_GLIBC 1
#elif defined(__APPLE__)
#include <malloc/malloc.h>
#define HEAP_IN_USE_DARWIN 1
#endif

/* The answer where the C library has no figure to give. */
#define HEAP_IN_USE_UNKNOWN UINT64_MAX

uint64_t heap_in_use(void);

uint64_t heap_in_use(void)
{
#if defined(HEAP_IN_USE_GLIBC)
    struct mallinfo2 info = mallinfo2();
    return (uint64_t)info.uordblks + (uint64_t)info.hblkhd;
#elif defined(HEAP_IN_USE_DARWIN)
    malloc_statistics_t stats;
    malloc_zone_statistics(NULL, &stats);
    return (uint64_t)stats.size_in_use;
#else
    return HEAP_IN_USE_UNKNOWN;
#endif
}
