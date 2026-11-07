/*
 * What busybox-w32's miscutils/drop.c takes from MinGW-w64's winsafer.h
 * (mingw-w64-headers/include/winsafer.h:16-27, 127, 130): TinyCC has no
 * winsafer.h.  advapi32.dll exports the calls.
 */
#ifndef BUSYBOX_W32_COMPAT_WINSAFER_H
#define BUSYBOX_W32_COMPAT_WINSAFER_H

#include <windows.h>

DECLARE_HANDLE(SAFER_LEVEL_HANDLE);

#define SAFER_SCOPEID_USER 2
#define SAFER_LEVELID_NORMALUSER 0x20000
#define SAFER_LEVEL_OPEN 1

BOOL WINAPI SaferCreateLevel(DWORD dwScopeId, DWORD dwLevelId, DWORD OpenFlags, SAFER_LEVEL_HANDLE *pLevelHandle, LPVOID lpReserved);
BOOL WINAPI SaferComputeTokenFromLevel(SAFER_LEVEL_HANDLE LevelHandle, HANDLE InAccessToken, PHANDLE OutAccessToken, DWORD dwFlags, LPVOID lpReserved);

#endif
