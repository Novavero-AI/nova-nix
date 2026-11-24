/*
 * TinyCC's shellapi.h, and the ShellExecuteEx structure, by its ANSI name
 * and the one MinGW-w64's headers give it without UNICODE, and the flags
 * busybox-w32 uses, as MinGW-w64's headers define them.
 * ShellExecuteExA itself is loaded from shell32.dll at run time.
 */
#include_next <shellapi.h>

#ifndef BUSYBOX_W32_COMPAT_SHELLAPI_H
#define BUSYBOX_W32_COMPAT_SHELLAPI_H

#define SEE_MASK_NOCLOSEPROCESS 0x40
#define SEE_MASK_NO_CONSOLE 0x8000

typedef struct _SHELLEXECUTEINFOA {
  DWORD cbSize;
  ULONG fMask;
  HWND hwnd;
  LPCSTR lpVerb;
  LPCSTR lpFile;
  LPCSTR lpParameters;
  LPCSTR lpDirectory;
  int nShow;
  HINSTANCE hInstApp;
  void *lpIDList;
  LPCSTR lpClass;
  HKEY hkeyClass;
  DWORD dwHotKey;
  union {
    HANDLE hIcon;
    HANDLE hMonitor;
  };
  HANDLE hProcess;
} SHELLEXECUTEINFOA, *LPSHELLEXECUTEINFOA;

typedef SHELLEXECUTEINFOA SHELLEXECUTEINFO;

#endif
