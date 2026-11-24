/*
 * TinyCC's ws2ipdef.h, and IPV6_V6ONLY as MinGW-w64's headers define it,
 * which busybox-w32 sets.
 */
#include_next <ws2ipdef.h>

#ifndef IPV6_V6ONLY
#define IPV6_V6ONLY 27
#endif
