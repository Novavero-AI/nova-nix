#include "nn_winfs.h"

#ifdef _WIN32

#include <windows.h>
#include <sddl.h>
#include <winternl.h>

/* FileCaseSensitiveInformation and its flag are absent from older SDK and
 * MinGW headers; both values are fixed ABI, defined locally. */
enum { NN_FILE_CASE_SENSITIVE_INFORMATION = 71 };

#ifndef FILE_CS_FLAG_CASE_SENSITIVE_DIR
#define FILE_CS_FLAG_CASE_SENSITIVE_DIR 0x00000001
#endif

typedef struct nn_file_case_sensitive_info {
  ULONG flags;
} nn_file_case_sensitive_info;

typedef NTSTATUS(NTAPI *nn_nt_set_information_file)(HANDLE, PIO_STATUS_BLOCK,
                                                    PVOID, ULONG, ULONG);

int nn_dir_set_case_sensitive(const wchar_t *path) {
  HMODULE ntdll;
  nn_nt_set_information_file set_information_file;
  HANDLE dir;
  nn_file_case_sensitive_info info;
  IO_STATUS_BLOCK iosb;
  NTSTATUS status;

  /* NtSetInformationFile is the documented route to the per-directory
   * flag (fsutil and WSL use it); resolved dynamically because it lives
   * in ntdll, which is always loaded but not in the link line. */
  ntdll = GetModuleHandleW(L"ntdll.dll");
  if (ntdll == NULL) {
    return 0;
  }
  set_information_file =
      (nn_nt_set_information_file)GetProcAddress(ntdll, "NtSetInformationFile");
  if (set_information_file == NULL) {
    return 0;
  }

  dir = CreateFileW(path, FILE_WRITE_ATTRIBUTES,
                    FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
                    NULL, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS, NULL);
  if (dir == INVALID_HANDLE_VALUE) {
    return 0;
  }

  info.flags = FILE_CS_FLAG_CASE_SENSITIVE_DIR;
  status = set_information_file(dir, &iosb, &info, sizeof info,
                                NN_FILE_CASE_SENSITIVE_INFORMATION);
  CloseHandle(dir);
  return status >= 0 ? 1 : 0;
}

unsigned long nn_token_user_sid(wchar_t **sid, const char **failed) {
  HANDLE token;
  /* A TOKEN_USER is followed in the buffer by the SID it points at, which
   * is at most SECURITY_MAX_SID_SIZE bytes, so one call fills it.  The
   * union aligns the buffer for the structure. */
  union {
    TOKEN_USER user;
    BYTE bytes[sizeof(TOKEN_USER) + SECURITY_MAX_SID_SIZE];
  } buffer;
  DWORD size;
  DWORD error = ERROR_SUCCESS;

  *sid = NULL;
  *failed = NULL;
  if (!OpenProcessToken(GetCurrentProcess(), TOKEN_QUERY, &token)) {
    *failed = "OpenProcessToken";
    return GetLastError();
  }
  if (!GetTokenInformation(token, TokenUser, &buffer, sizeof buffer, &size)) {
    *failed = "GetTokenInformation";
    error = GetLastError();
  } else if (!ConvertSidToStringSidW(buffer.user.User.Sid, sid)) {
    *failed = "ConvertSidToStringSidW";
    error = GetLastError();
    *sid = NULL;
  }
  CloseHandle(token);
  return error;
}

unsigned long nn_dir_create_sddl(const wchar_t *path, const wchar_t *sddl) {
  PSECURITY_DESCRIPTOR descriptor = NULL;
  SECURITY_ATTRIBUTES attributes;
  DWORD error = ERROR_SUCCESS;

  if (!ConvertStringSecurityDescriptorToSecurityDescriptorW(
          sddl, SDDL_REVISION_1, &descriptor, NULL)) {
    return GetLastError();
  }
  attributes.nLength = sizeof attributes;
  attributes.lpSecurityDescriptor = descriptor;
  attributes.bInheritHandle = FALSE;
  if (!CreateDirectoryW(path, &attributes)) {
    error = GetLastError();
  }
  LocalFree(descriptor);
  return error;
}

#else /* !_WIN32 */

/* Per-directory case sensitivity is an NTFS feature; APFS decides at
 * volume creation and Linux filesystems are case-sensitive by nature. */
int nn_dir_set_case_sensitive(const wchar_t *path) {
  (void)path;
  return 0;
}

#endif
