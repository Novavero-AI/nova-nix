/*
 * REPARSE_DATA_BUFFER from MinGW-w64's ntdef.h
 * (mingw-w64-headers/include/ntdef.h:988-1019), which busybox-w32's
 * win32/mingw.c reads symbolic links and junctions with: TinyCC has no
 * ntdef.h.
 */
#ifndef BUSYBOX_W32_COMPAT_NTDEF_H
#define BUSYBOX_W32_COMPAT_NTDEF_H

#include <windows.h>

typedef struct _REPARSE_DATA_BUFFER {
  ULONG ReparseTag;
  USHORT ReparseDataLength;
  USHORT Reserved;
  union {
    struct {
      USHORT SubstituteNameOffset;
      USHORT SubstituteNameLength;
      USHORT PrintNameOffset;
      USHORT PrintNameLength;
      ULONG Flags;
      WCHAR PathBuffer[1];
    } SymbolicLinkReparseBuffer;
    struct {
      USHORT SubstituteNameOffset;
      USHORT SubstituteNameLength;
      USHORT PrintNameOffset;
      USHORT PrintNameLength;
      WCHAR PathBuffer[1];
    } MountPointReparseBuffer;
    struct {
      UCHAR DataBuffer[1];
    } GenericReparseBuffer;
  };
} REPARSE_DATA_BUFFER, *PREPARSE_DATA_BUFFER;

#define REPARSE_DATA_BUFFER_HEADER_SIZE FIELD_OFFSET(REPARSE_DATA_BUFFER, GenericReparseBuffer)

#endif
