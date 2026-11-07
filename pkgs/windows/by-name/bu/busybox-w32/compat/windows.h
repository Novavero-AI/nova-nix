/*
 * TinyCC's windows.h, and what MinGW-w64's brings in that busybox-w32
 * reads and TinyCC's lacks.  MinGW-w64's windows.h includes shellapi.h
 * unless WIN32_LEAN_AND_MEAN is defined
 * (mingw-w64-headers/include/windows.h:80-89), where TinyCC's never does.
 * The rest are items TinyCC's winbase.h and winnt.h predate:
 * VOLUME_NAME_DOS and FILE_NAME_NORMALIZED (include/winbase.h:461-466);
 * TokenElevation and TokenIntegrityLevel, past TokenOrigin, the last value
 * of TinyCC's TOKEN_INFORMATION_CLASS, with the structures they return
 * (include/winnt.h:4132-4157, 4260-4266); and the file system controls of
 * MinGW-w64's winioctl.h, which TinyCC has no counterpart of
 * (include/winioctl.h:86-182, 1501-1510, 1909-1912).
 */
#include_next <windows.h>

#ifndef BUSYBOX_W32_COMPAT_WINDOWS_H
#define BUSYBOX_W32_COMPAT_WINDOWS_H

#include <shellapi.h>

#define VOLUME_NAME_DOS 0x0
#define FILE_NAME_NORMALIZED 0x0

#define TokenElevation ((TOKEN_INFORMATION_CLASS) 20)
#define TokenIntegrityLevel ((TOKEN_INFORMATION_CLASS) 25)

typedef struct _TOKEN_ELEVATION {
  DWORD TokenIsElevated;
} TOKEN_ELEVATION, *PTOKEN_ELEVATION;

typedef struct _TOKEN_MANDATORY_LABEL {
  SID_AND_ATTRIBUTES Label;
} TOKEN_MANDATORY_LABEL, *PTOKEN_MANDATORY_LABEL;

#define FILE_DEVICE_FILE_SYSTEM 0x00000009
#define CTL_CODE(DeviceType,Function,Method,Access) (((DeviceType) << 16) | ((Access) << 14) | ((Function) << 2) | (Method))
#define METHOD_BUFFERED 0
#define FILE_ANY_ACCESS 0
#define FILE_SPECIAL_ACCESS (FILE_ANY_ACCESS)
#define FSCTL_SET_REPARSE_POINT CTL_CODE(FILE_DEVICE_FILE_SYSTEM,41,METHOD_BUFFERED,FILE_SPECIAL_ACCESS)
#define FSCTL_GET_REPARSE_POINT CTL_CODE(FILE_DEVICE_FILE_SYSTEM,42,METHOD_BUFFERED,FILE_ANY_ACCESS)
#define FSCTL_SET_SPARSE CTL_CODE(FILE_DEVICE_FILE_SYSTEM,49,METHOD_BUFFERED,FILE_SPECIAL_ACCESS)
#define FSCTL_SET_ZERO_DATA CTL_CODE(FILE_DEVICE_FILE_SYSTEM,50,METHOD_BUFFERED,FILE_WRITE_DATA)

typedef struct _FILE_ZERO_DATA_INFORMATION {
  LARGE_INTEGER FileOffset;
  LARGE_INTEGER BeyondFinalZero;
} FILE_ZERO_DATA_INFORMATION, *PFILE_ZERO_DATA_INFORMATION;

#endif
