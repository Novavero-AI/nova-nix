/*
 * The process walk busybox-w32's win32/process.c takes from MinGW-w64's
 * tlhelp32.h (mingw-w64-headers/include/tlhelp32.h:15-18, 80-96): TinyCC
 * has no tlhelp32.h.  kernel32.dll exports the calls.
 */
#ifndef BUSYBOX_W32_COMPAT_TLHELP32_H
#define BUSYBOX_W32_COMPAT_TLHELP32_H

#include <windows.h>

#define TH32CS_SNAPPROCESS 0x00000002

HANDLE WINAPI CreateToolhelp32Snapshot(DWORD dwFlags, DWORD th32ProcessID);

typedef struct tagPROCESSENTRY32 {
  DWORD dwSize;
  DWORD cntUsage;
  DWORD th32ProcessID;
  ULONG_PTR th32DefaultHeapID;
  DWORD th32ModuleID;
  DWORD cntThreads;
  DWORD th32ParentProcessID;
  LONG pcPriClassBase;
  DWORD dwFlags;
  CHAR szExeFile[MAX_PATH];
} PROCESSENTRY32, *PPROCESSENTRY32, *LPPROCESSENTRY32;

BOOL WINAPI Process32First(HANDLE hSnapshot, LPPROCESSENTRY32 lppe);
BOOL WINAPI Process32Next(HANDLE hSnapshot, LPPROCESSENTRY32 lppe);

#endif
