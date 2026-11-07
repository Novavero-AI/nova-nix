/*
 * MinGW-w64's sdkddkver.h, which TinyCC's headers lack: the _WIN32_WINNT
 * value of each Windows release, and the default when none is chosen
 * (mingw-w64-headers/include/sdkddkver.h:14-26 and 171-173).  gnulib's
 * stat-w32.c compares _WIN32_WINNT with _WIN32_WINNT_VISTA to decide
 * whether it may call GetFinalPathNameByHandle directly, and includes this
 * header where configure finds it; with the name undefined, the comparison
 * is against 0, and it calls a function TinyCC's headers do not declare.
 */
#ifndef _INC_SDKDDKVER
#define _INC_SDKDDKVER

#define _WIN32_WINNT_NT4 0x0400
#define _WIN32_WINNT_WIN2K 0x0500
#define _WIN32_WINNT_WINXP 0x0501
#define _WIN32_WINNT_WS03 0x0502
#define _WIN32_WINNT_WIN6 0x0600
#define _WIN32_WINNT_VISTA 0x0600
#define _WIN32_WINNT_WS08 0x0600
#define _WIN32_WINNT_LONGHORN 0x0600
#define _WIN32_WINNT_WIN7 0x0601
#define _WIN32_WINNT_WIN8 0x0602
#define _WIN32_WINNT_WINBLUE 0x0603
#define _WIN32_WINNT_WINTHRESHOLD 0x0A00
#define _WIN32_WINNT_WIN10 0x0A00

#if !defined(_WIN32_WINNT) && !defined(_CHICAGO_)
#define _WIN32_WINNT _WIN32_WINNT_WS03
#endif

#endif
