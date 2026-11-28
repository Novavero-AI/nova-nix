#include "nn_winfs.h"

#ifdef _WIN32

#include <windows.h>
#include <aclapi.h>
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

unsigned long nn_entry_security(const wchar_t *path, void **descriptor,
                                unsigned long *length, const char **failed) {
  HANDLE entry;
  PSECURITY_DESCRIPTOR read = NULL;
  DWORD error;

  *descriptor = NULL;
  *length = 0;
  *failed = NULL;
  /* READ_CONTROL is the one right reading an owner and a DACL takes.
   * FILE_FLAG_BACKUP_SEMANTICS lets a directory be opened at all, and
   * FILE_FLAG_OPEN_REPARSE_POINT opens a link itself, not what it leads
   * to. */
  entry = CreateFileW(path, READ_CONTROL,
                      FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
                      NULL, OPEN_EXISTING,
                      FILE_FLAG_BACKUP_SEMANTICS | FILE_FLAG_OPEN_REPARSE_POINT,
                      NULL);
  if (entry == INVALID_HANDLE_VALUE) {
    *failed = "CreateFileW";
    return GetLastError();
  }
  error =
      GetSecurityInfo(entry, SE_FILE_OBJECT,
                      OWNER_SECURITY_INFORMATION | DACL_SECURITY_INFORMATION,
                      NULL, NULL, NULL, NULL, &read);
  CloseHandle(entry);
  if (error != ERROR_SUCCESS) {
    *failed = "GetSecurityInfo";
    return error;
  }
  *descriptor = read;
  *length = GetSecurityDescriptorLength(read);
  return ERROR_SUCCESS;
}

/* A local account's name, qualified by the computer's NetBIOS name so
 * the lookup searches this machine's accounts alone and never a domain's,
 * fits in this many characters with room to spare: a NetBIOS name is at
 * most 15 and a local account's name at most 256. */
enum { NN_QUALIFIED_NAME_CHARS = 300 };

unsigned long nn_local_group_sid(const wchar_t *name, wchar_t **sid,
                                 const char **failed) {
  wchar_t qualified[NN_QUALIFIED_NAME_CHARS];
  DWORD computerChars = MAX_COMPUTERNAME_LENGTH + 1;
  union {
    SID sid;
    BYTE bytes[SECURITY_MAX_SID_SIZE];
  } found;
  DWORD sidSize = sizeof found;
  wchar_t domain[MAX_COMPUTERNAME_LENGTH + 1];
  DWORD domainChars = MAX_COMPUTERNAME_LENGTH + 1;
  SID_NAME_USE use;
  size_t nameChars = wcslen(name);
  DWORD error;

  *sid = NULL;
  *failed = NULL;
  if (!GetComputerNameW(qualified, &computerChars)) {
    *failed = "GetComputerNameW";
    return GetLastError();
  }
  if (computerChars + 1 + nameChars + 1 > NN_QUALIFIED_NAME_CHARS) {
    *failed = "nn_local_group_sid";
    return ERROR_INVALID_PARAMETER;
  }
  qualified[computerChars] = L'\\';
  wmemcpy(qualified + computerChars + 1, name, nameChars + 1);
  if (!LookupAccountNameW(NULL, qualified, &found.sid, &sidSize, domain,
                          &domainChars, &use)) {
    error = GetLastError();
    if (error == ERROR_NONE_MAPPED) {
      return ERROR_SUCCESS;
    }
    *failed = "LookupAccountNameW";
    return error;
  }
  if (use != SidTypeAlias) {
    return ERROR_SUCCESS;
  }
  if (!ConvertSidToStringSidW(&found.sid, sid)) {
    *failed = "ConvertSidToStringSidW";
    *sid = NULL;
    return GetLastError();
  }
  return ERROR_SUCCESS;
}

unsigned long nn_descriptor_from_sddl(const wchar_t *sddl, void **descriptor) {
  PSECURITY_DESCRIPTOR made = NULL;

  *descriptor = NULL;
  if (!ConvertStringSecurityDescriptorToSecurityDescriptorW(
          sddl, SDDL_REVISION_1, &made, NULL)) {
    return GetLastError();
  }
  *descriptor = made;
  return ERROR_SUCCESS;
}

unsigned long nn_entry_set_owner_dacl(const wchar_t *path, void *descriptor,
                                      const char **failed) {
  PSID owner = NULL;
  PACL dacl = NULL;
  BOOL present = FALSE;
  BOOL defaulted = FALSE;
  HANDLE entry;
  DWORD error;

  *failed = NULL;
  if (!GetSecurityDescriptorOwner(descriptor, &owner, &defaulted)) {
    *failed = "GetSecurityDescriptorOwner";
    return GetLastError();
  }
  if (!GetSecurityDescriptorDacl(descriptor, &present, &dacl, &defaulted)) {
    *failed = "GetSecurityDescriptorDacl";
    return GetLastError();
  }
  /* WRITE_OWNER and WRITE_DAC are the rights the two writes take, and
   * READ_CONTROL lets the write read what it keeps.  The flags open a
   * directory, and a link itself rather than what it leads to. */
  entry = CreateFileW(path, READ_CONTROL | WRITE_DAC | WRITE_OWNER,
                      FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
                      NULL, OPEN_EXISTING,
                      FILE_FLAG_BACKUP_SEMANTICS | FILE_FLAG_OPEN_REPARSE_POINT,
                      NULL);
  if (entry == INVALID_HANDLE_VALUE) {
    *failed = "CreateFileW";
    return GetLastError();
  }
  error = SetSecurityInfo(entry, SE_FILE_OBJECT,
                          OWNER_SECURITY_INFORMATION |
                              DACL_SECURITY_INFORMATION |
                              PROTECTED_DACL_SECURITY_INFORMATION,
                          owner, NULL, dacl, NULL);
  CloseHandle(entry);
  if (error != ERROR_SUCCESS) {
    *failed = "SetSecurityInfo";
  }
  return error;
}

unsigned long nn_job_die_on_unhandled_exception(void *job, const char **failed) {
  JOBOBJECT_EXTENDED_LIMIT_INFORMATION limits;

  *failed = NULL;
  /* The limits are set as a whole, so the job's own, the process
   * library's KILL_ON_JOB_CLOSE among them, are read first and kept. */
  if (!QueryInformationJobObject(job, JobObjectExtendedLimitInformation,
                                 &limits, sizeof limits, NULL)) {
    *failed = "QueryInformationJobObject";
    return GetLastError();
  }
  limits.BasicLimitInformation.LimitFlags |=
      JOB_OBJECT_LIMIT_DIE_ON_UNHANDLED_EXCEPTION;
  if (!SetInformationJobObject(job, JobObjectExtendedLimitInformation,
                               &limits, sizeof limits)) {
    *failed = "SetInformationJobObject";
    return GetLastError();
  }
  return ERROR_SUCCESS;
}

#else /* !_WIN32 */

/* Per-directory case sensitivity is an NTFS feature; APFS decides at
 * volume creation and Linux filesystems are case-sensitive by nature. */
int nn_dir_set_case_sensitive(const wchar_t *path) {
  (void)path;
  return 0;
}

#endif
