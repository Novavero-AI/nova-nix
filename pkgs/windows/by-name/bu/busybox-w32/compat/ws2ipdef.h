/*
 * TinyCC's ws2ipdef.h, and IPV6_V6ONLY
 * (mingw-w64-headers/include/ws2ipdef.h:88), which busybox-w32's
 * libbb/xconnect.c sets.
 */
#include_next <ws2ipdef.h>

#ifndef IPV6_V6ONLY
#define IPV6_V6ONLY 27
#endif
