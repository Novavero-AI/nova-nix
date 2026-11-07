/*
 * TinyCC's winsock2.h, read as MinGW-w64's reads to a program that
 * targets Windows XP, as busybox-w32 does (_WIN32_WINNT 0x502,
 * include/platform.h): without struct pollfd and the POLL flags, which
 * MinGW-w64's declares only from Vista on
 * (mingw-w64-headers/include/winsock2.h:1122, 1179-1196), TinyCC's
 * whatever the target, and busybox-w32's win32/poll.h for itself.  The
 * flags as they stood are kept across TinyCC's header, and its struct
 * takes another name.  And WSAMSG (include/winsock2.h:866-873), which
 * mswsock.h's WSARecvMsg takes.
 */
#ifndef BUSYBOX_W32_COMPAT_WINSOCK2_H
#define BUSYBOX_W32_COMPAT_WINSOCK2_H

#pragma push_macro("POLLRDNORM")
#pragma push_macro("POLLRDBAND")
#pragma push_macro("POLLIN")
#pragma push_macro("POLLPRI")
#pragma push_macro("POLLWRNORM")
#pragma push_macro("POLLOUT")
#pragma push_macro("POLLWRBAND")
#pragma push_macro("POLLERR")
#pragma push_macro("POLLHUP")
#pragma push_macro("POLLNVAL")
#undef POLLRDNORM
#undef POLLRDBAND
#undef POLLIN
#undef POLLPRI
#undef POLLWRNORM
#undef POLLOUT
#undef POLLWRBAND
#undef POLLERR
#undef POLLHUP
#undef POLLNVAL
#define pollfd tinycc_winsock2_pollfd

#include_next <winsock2.h>

#undef pollfd
#pragma pop_macro("POLLRDNORM")
#pragma pop_macro("POLLRDBAND")
#pragma pop_macro("POLLIN")
#pragma pop_macro("POLLPRI")
#pragma pop_macro("POLLWRNORM")
#pragma pop_macro("POLLOUT")
#pragma pop_macro("POLLWRBAND")
#pragma pop_macro("POLLERR")
#pragma pop_macro("POLLHUP")
#pragma pop_macro("POLLNVAL")

typedef struct _WSAMSG {
  LPSOCKADDR name;
  INT namelen;
  LPWSABUF lpBuffers;
  DWORD dwBufferCount;
  WSABUF Control;
  DWORD dwFlags;
} WSAMSG, *PWSAMSG, *LPWSAMSG;

#endif
