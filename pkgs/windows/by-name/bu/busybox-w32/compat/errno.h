/*
 * TinyCC's errno.h, and those of the POSIX values MinGW-w64's adds past
 * msvcrt's (mingw-w64-headers/crt/errno.h:74-221), numbered as Visual
 * C++ 10's errno.h numbers them, that busybox-w32 sets and tests.
 */
#include_next <errno.h>

#ifndef BUSYBOX_W32_COMPAT_ERRNO_H
#define BUSYBOX_W32_COMPAT_ERRNO_H

#ifndef ENOTSUP
#define ENOTSUP 129
#endif
#ifndef EAFNOSUPPORT
#define EAFNOSUPPORT 102
#endif
#ifndef ECONNABORTED
#define ECONNABORTED 106
#endif
#ifndef ECONNRESET
#define ECONNRESET 108
#endif
#ifndef ENETRESET
#define ENETRESET 117
#endif
#ifndef ENOTSOCK
#define ENOTSOCK 128
#endif
#ifndef ENOTCONN
#define ENOTCONN 126
#endif
#ifndef EWOULDBLOCK
#define EWOULDBLOCK 140
#endif
#ifndef ENODATA
#define ENODATA 120
#endif
#ifndef ETIMEDOUT
#define ETIMEDOUT 138
#endif
#ifndef ELOOP
#define ELOOP 114
#endif
#ifndef EOVERFLOW
#define EOVERFLOW 132
#endif

#endif
