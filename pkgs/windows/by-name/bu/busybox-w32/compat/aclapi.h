/*
 * GetSecurityInfo, which busybox-w32 calls, and its object type, as
 * MinGW-w64's headers declare them: TinyCC's headers have neither.
 * advapi32.dll exports it.
 */
#ifndef BUSYBOX_W32_COMPAT_ACLAPI_H
#define BUSYBOX_W32_COMPAT_ACLAPI_H

#include <windows.h>

typedef enum _SE_OBJECT_TYPE {
  SE_UNKNOWN_OBJECT_TYPE = 0,
  SE_FILE_OBJECT,
  SE_SERVICE,
  SE_PRINTER,
  SE_REGISTRY_KEY,
  SE_LMSHARE,
  SE_KERNEL_OBJECT,
  SE_WINDOW_OBJECT,
  SE_DS_OBJECT,
  SE_DS_OBJECT_ALL,
  SE_PROVIDER_DEFINED_OBJECT,
  SE_WMIGUID_OBJECT,
  SE_REGISTRY_WOW64_32KEY
} SE_OBJECT_TYPE;

DWORD WINAPI GetSecurityInfo(HANDLE handle, SE_OBJECT_TYPE ObjectType, SECURITY_INFORMATION SecurityInfo,
  PSID *ppsidOwner, PSID *ppsidGroup, PACL *ppDacl, PACL *ppSacl, PSECURITY_DESCRIPTOR *ppSecurityDescriptor);

#endif
