/*
 * TinyCC's limits.h, and SSIZE_MAX as MinGW-w64's defines it for 32-bit
 * Windows (mingw-w64-headers/crt/limits.h:82-88): TinyCC's defines it only
 * under _POSIX_, and as _POSIX_SSIZE_MAX.
 */
#include_next <limits.h>

#ifndef SSIZE_MAX
#define SSIZE_MAX INT_MAX
#endif
