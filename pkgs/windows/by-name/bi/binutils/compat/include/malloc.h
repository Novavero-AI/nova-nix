/*
 * TinyCC's malloc.h, and alloca as a macro, which MinGW-w64's makes it for
 * every compiler (mingw-w64-headers/crt/malloc.h:235-242) and TinyCC's for
 * GCC alone.  tcc declares alloca before it reads any header, as a function
 * returning void * (include/tccdefs.h:351), and libiberty's cp-demangle.c
 * declares it as one returning char * unless it is a macro.  The macro
 * names tcc's alloca, as MinGW-w64's names GCC's builtin; the name
 * MinGW-w64 gives other compilers, _alloca, is a macro in TinyCC's malloc.h
 * that expands back to alloca.
 */
#include_next <malloc.h>

#ifndef alloca
#define alloca(x) alloca((x))
#endif
