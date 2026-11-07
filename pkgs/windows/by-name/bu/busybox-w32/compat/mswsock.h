/*
 * The control-message macros and WSARecvMsg's type from MinGW-w64's
 * mswsock.h (mingw-w64-headers/include/mswsock.h:175-190, 207-212),
 * which busybox-w32's include/mingw_ip.h maps the POSIX cmsg API onto:
 * TinyCC has no mswsock.h.
 */
#ifndef BUSYBOX_W32_COMPAT_MSWSOCK_H
#define BUSYBOX_W32_COMPAT_MSWSOCK_H

#include <winsock2.h>

typedef struct _WSACMSGHDR {
  SIZE_T cmsg_len;
  INT cmsg_level;
  INT cmsg_type;
} WSACMSGHDR, *PWSACMSGHDR, *LPWSACMSGHDR;

#define WSA_CMSGHDR_ALIGN(length) (((length) + TYPE_ALIGNMENT(WSACMSGHDR)-1) & (~(TYPE_ALIGNMENT(WSACMSGHDR)-1)))
#define WSA_CMSGDATA_ALIGN(length) (((length) + MAX_NATURAL_ALIGNMENT-1) & (~(MAX_NATURAL_ALIGNMENT-1)))
#define WSA_CMSG_FIRSTHDR(msg) (((msg)->Control.len >= sizeof(WSACMSGHDR)) ? (LPWSACMSGHDR)(msg)->Control.buf : (LPWSACMSGHDR)NULL)
#define WSA_CMSG_NXTHDR(msg,cmsg) ((!(cmsg)) ? WSA_CMSG_FIRSTHDR(msg) : ((((u_char *)(cmsg) + WSA_CMSGHDR_ALIGN((cmsg)->cmsg_len) + sizeof(WSACMSGHDR)) > (u_char *)((msg)->Control.buf) + (msg)->Control.len) ? (LPWSACMSGHDR)NULL : (LPWSACMSGHDR)((u_char *)(cmsg) + WSA_CMSGHDR_ALIGN((cmsg)->cmsg_len))))
#define WSA_CMSG_DATA(cmsg) ((u_char *)(cmsg) + WSA_CMSGDATA_ALIGN(sizeof(WSACMSGHDR)))
#define WSA_CMSG_SPACE(length) (WSA_CMSGDATA_ALIGN(sizeof(WSACMSGHDR) + WSA_CMSGHDR_ALIGN(length)))

typedef INT (WINAPI *LPFN_WSARECVMSG)(SOCKET s, LPWSAMSG lpMsg, LPDWORD lpdwNumberOfBytesRecvd,
  LPWSAOVERLAPPED lpOverlapped, LPWSAOVERLAPPED_COMPLETION_ROUTINE lpCompletionRoutine);
#define WSAID_WSARECVMSG {0xf689d7c8,0x6f1f,0x436b,{0x8a,0x53,0xe5,0x4f,0xe3,0x51,0xc3,0x22}}

#endif
