/*
 * TinyCC's limits.h, and SSIZE_MAX as MinGW-w64's headers define it for
 * 32-bit Windows: TinyCC's defines it only under _POSIX_, and as
 * _POSIX_SSIZE_MAX.
 */
#include_next <limits.h>

#ifndef SSIZE_MAX
#define SSIZE_MAX INT_MAX
#endif
