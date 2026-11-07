/*
 * TinyCC's unistd.h, and what MinGW-w64's adds to it
 * (mingw-w64-headers/crt/unistd.h:6-71): process.h and getopt.h, usleep,
 * and ftruncate.  busybox compiles with _FILE_OFFSET_BITS=64, under which
 * MinGW-w64 renames ftruncate to ftruncate64, which takes a 64-bit length,
 * with an asm label; a macro renames it here.  TinyCC's off_t stays 32
 * bits whatever _FILE_OFFSET_BITS says, so the length is _off64_t.
 */
#include_next <unistd.h>

#ifndef _UNISTD_H
#define _UNISTD_H
#define __UNISTD_H_SOURCED__ 1

#include <process.h>
#include <getopt.h>
#include <sys/types.h>

int __cdecl usleep(useconds_t);
int ftruncate64(int, _off64_t);
#define ftruncate ftruncate64

#undef __UNISTD_H_SOURCED__
#endif
