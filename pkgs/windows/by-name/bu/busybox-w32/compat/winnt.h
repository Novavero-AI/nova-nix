/*
 * TinyCC's winnt.h, and what MinGW-w64's declares that busybox-w32 reads
 * and TinyCC's predates: IO_REPARSE_TAG_SYMLINK
 * (mingw-w64-headers/include/winnt.h:6431), the NTSTATUS type
 * (include/ntdef.h:348), and the __MSABI_LONG macro their constants are
 * written with (crt/_mingw_mac.h:219).
 */
#include_next <winnt.h>

#ifndef BUSYBOX_W32_COMPAT_WINNT_H
#define BUSYBOX_W32_COMPAT_WINNT_H

#ifndef __MSABI_LONG
#define __MSABI_LONG(x) x ## l
#endif
#ifndef IO_REPARSE_TAG_SYMLINK
#define IO_REPARSE_TAG_SYMLINK (__MSABI_LONG(0xA000000C))
#endif
typedef LONG NTSTATUS, *PNTSTATUS;

#endif
