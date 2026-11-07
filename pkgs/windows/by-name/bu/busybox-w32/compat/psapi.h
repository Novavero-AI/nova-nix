/*
 * The types busybox-w32's win32/mingw.c passes the psapi.dll calls it
 * loads at run time, from MinGW-w64's psapi.h
 * (mingw-w64-headers/include/psapi.h:139-167): TinyCC has no psapi.h.
 */
#ifndef BUSYBOX_W32_COMPAT_PSAPI_H
#define BUSYBOX_W32_COMPAT_PSAPI_H

#include <windows.h>

typedef struct _PERFORMANCE_INFORMATION {
  DWORD cb;
  SIZE_T CommitTotal;
  SIZE_T CommitLimit;
  SIZE_T CommitPeak;
  SIZE_T PhysicalTotal;
  SIZE_T PhysicalAvailable;
  SIZE_T SystemCache;
  SIZE_T KernelTotal;
  SIZE_T KernelPaged;
  SIZE_T KernelNonpaged;
  SIZE_T PageSize;
  DWORD HandleCount;
  DWORD ProcessCount;
  DWORD ThreadCount;
} PERFORMANCE_INFORMATION, *PPERFORMANCE_INFORMATION;

typedef struct _ENUM_PAGE_FILE_INFORMATION {
  DWORD cb;
  DWORD Reserved;
  SIZE_T TotalSize;
  SIZE_T TotalInUse;
  SIZE_T PeakUsage;
} ENUM_PAGE_FILE_INFORMATION, *PENUM_PAGE_FILE_INFORMATION;

typedef BOOL (CALLBACK *PENUM_PAGE_FILE_CALLBACKA)(LPVOID pContext, PENUM_PAGE_FILE_INFORMATION pPageFileInfo, LPCSTR lpFilename);
#define PENUM_PAGE_FILE_CALLBACK PENUM_PAGE_FILE_CALLBACKA

#endif
