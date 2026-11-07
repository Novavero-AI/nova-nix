/*
 * MinGW-w64's libgen.h (mingw-w64-headers/crt/libgen.h): TinyCC has none.
 * busybox-w32's win32/dirname.c defines both.
 */
#ifndef _LIBGEN_H_
#define _LIBGEN_H_

char *__cdecl basename(char *);
char *__cdecl dirname(char *);

#endif
