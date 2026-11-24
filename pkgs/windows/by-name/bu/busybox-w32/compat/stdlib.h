/*
 * TinyCC's stdlib.h, and getenv_s, which MinGW-w64's headers declare and
 * TinyCC's secure API header lacks.  msvcrt.dll exports it; MinGW-w64's
 * getopt, which the package compiles, calls it.
 */
#include_next <stdlib.h>

#ifndef BUSYBOX_W32_COMPAT_STDLIB_H
#define BUSYBOX_W32_COMPAT_STDLIB_H

_CRTIMP errno_t __cdecl getenv_s(size_t *_ReturnSize, char *_DstBuf, size_t _DstSize, const char *_VarName);

#endif
