/*
 * TinyCC's string.h, and what MinGW-w64's declares that it does not
 * (mingw-w64-headers/crt/string.h:52, 65 and 107): mempcpy and strtok_r,
 * which mingw-w64-crt's mempcpy.c and strtok_r.c define, and strnlen,
 * which msvcrt.dll exports and TinyCC's string.h has under #if 0.
 */
#include_next <string.h>

#ifndef BUSYBOX_W32_COMPAT_STRING_H
#define BUSYBOX_W32_COMPAT_STRING_H

void *__cdecl mempcpy(void *_Dst, const void *_Src, size_t _Size);
size_t __cdecl strnlen(const char *_Str, size_t _MaxCount);
char *strtok_r(char *_Str, const char *_Delim, char **__last);

#endif
