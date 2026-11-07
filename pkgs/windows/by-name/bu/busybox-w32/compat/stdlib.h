/*
 * TinyCC's stdlib.h, and getenv_s, which MinGW-w64's declares
 * (mingw-w64-headers/crt/sec_api/stdlib_s.h:25) and TinyCC's secure API
 * header lacks.  msvcrt.dll exports it; mingw-w64-crt's getopt.c calls it.
 */
#include_next <stdlib.h>

#ifndef BUSYBOX_W32_COMPAT_STDLIB_H
#define BUSYBOX_W32_COMPAT_STDLIB_H

_CRTIMP errno_t __cdecl getenv_s(size_t *_ReturnSize, char *_DstBuf, size_t _DstSize, const char *_VarName);

#endif
