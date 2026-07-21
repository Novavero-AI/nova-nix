#include "nn_winfs.h"

#ifdef _WIN32

#include <windows.h>
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

#else /* !_WIN32 */

/* Per-directory case sensitivity is an NTFS feature; APFS decides at
 * volume creation and Linux filesystems are case-sensitive by nature. */
int nn_dir_set_case_sensitive(const wchar_t *path) {
  (void)path;
  return 0;
}

#endif
