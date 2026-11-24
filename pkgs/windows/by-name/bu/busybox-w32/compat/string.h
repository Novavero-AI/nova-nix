/*
 * TinyCC's string.h, and what MinGW-w64's declares that it does not:
 * mempcpy and strtok_r, which the package compiles from MinGW-w64's
 * runtime, and strnlen, which msvcrt.dll exports and TinyCC's string.h has
 * under #if 0.
 */
#include_next <string.h>

#ifndef BUSYBOX_W32_COMPAT_STRING_H
#define BUSYBOX_W32_COMPAT_STRING_H

void *__cdecl mempcpy(void *_Dst, const void *_Src, size_t _Size);
size_t __cdecl strnlen(const char *_Str, size_t _MaxCount);
char *strtok_r(char *_Str, const char *_Delim, char **__last);

#endif
