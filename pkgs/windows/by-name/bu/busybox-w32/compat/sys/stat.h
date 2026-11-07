/*
 * TinyCC's sys/stat.h, with _fstati64 calling msvcrt.dll's _fstati64.
 * TinyCC's maps it to _fstat32i64, the name Visual C++ 2005's runtime gives
 * the same call, which msvcrt.dll does not export; MinGW-w64's maps it to
 * _fstati64 itself for msvcrt.  The struct, _stat32i64 by either name, has
 * the layout msvcrt.dll's _stati64 has.
 */
#include_next <sys/stat.h>

#ifndef BUSYBOX_W32_COMPAT_SYS_STAT_H
#define BUSYBOX_W32_COMPAT_SYS_STAT_H

#undef _fstati64
_CRTIMP int __cdecl _fstati64(int _FileDes, struct _stat32i64 *_Stat);

#endif
