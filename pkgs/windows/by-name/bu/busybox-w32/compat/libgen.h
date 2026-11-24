/*
 * basename and dirname as MinGW-w64's headers declare them: TinyCC has no
 * libgen.h.  busybox-w32 defines both itself.
 */
#ifndef _LIBGEN_H_
#define _LIBGEN_H_

char *__cdecl basename(char *);
char *__cdecl dirname(char *);

#endif
