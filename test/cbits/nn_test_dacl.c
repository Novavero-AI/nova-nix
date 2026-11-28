/* A directory's DACL read back through the Win32 API, for the store
 * access tests (test/DirDacl.hs): whether it is protected and each of its
 * entries.  The test suite builds it on Windows alone, the one platform
 * whose directories carry a DACL. */

#include <windows.h>
#include <aclapi.h>
#include <sddl.h>

/* The security descriptor of the entry at path, holding its DACL, written
 * to *descriptor for the caller to release with LocalFree.  Returns 0 or
 * the Win32 error code. */
unsigned long nn_test_dacl_read(const wchar_t *path, void **descriptor) {
  PACL dacl = NULL;
  PSECURITY_DESCRIPTOR read = NULL;
  DWORD error;

  error = GetNamedSecurityInfoW(path, SE_FILE_OBJECT, DACL_SECURITY_INFORMATION,
                                NULL, NULL, &dacl, NULL, &read);
  *descriptor = read;
  return error;
}

/* 1 when the descriptor's DACL is protected, taking no entry from the
 * parent, 0 when it is not, and -1 when its control bits cannot be
 * read. */
int nn_test_dacl_protected(void *descriptor) {
  SECURITY_DESCRIPTOR_CONTROL control;
  DWORD revision;

  if (!GetSecurityDescriptorControl(descriptor, &control, &revision)) {
    return -1;
  }
  return (control & SE_DACL_PROTECTED) != 0 ? 1 : 0;
}

/* How many entries the descriptor's DACL holds, or -1 when it has none
 * to count: no DACL, or a NULL one, which grants every account every
 * right. */
long nn_test_dacl_count(void *descriptor) {
  BOOL present;
  BOOL defaulted;
  PACL dacl = NULL;
  ACL_SIZE_INFORMATION size;

  if (!GetSecurityDescriptorDacl(descriptor, &present, &dacl, &defaulted) ||
      !present || dacl == NULL) {
    return -1;
  }
  if (!GetAclInformation(dacl, &size, sizeof size, AclSizeInformation)) {
    return -1;
  }
  return (long)size.AceCount;
}

/* The entry at index of the descriptor's DACL: fields gets its type, its
 * flags and its access mask, and *sid its SID's string form, which the
 * caller releases with LocalFree.  An allow and a deny entry share one
 * layout; an entry of any other type gets a mask of 0 and no SID (NULL).
 * Returns 0 or the Win32 error code. */
unsigned long nn_test_dacl_entry(void *descriptor, unsigned long index,
                                 unsigned long fields[3], wchar_t **sid) {
  BOOL present;
  BOOL defaulted;
  PACL dacl = NULL;
  LPVOID entry;
  ACE_HEADER *header;
  ACCESS_ALLOWED_ACE *allowed;

  *sid = NULL;
  if (!GetSecurityDescriptorDacl(descriptor, &present, &dacl, &defaulted) ||
      !GetAce(dacl, index, &entry)) {
    return GetLastError();
  }
  header = (ACE_HEADER *)entry;
  fields[0] = header->AceType;
  fields[1] = header->AceFlags;
  fields[2] = 0;
  if (header->AceType != ACCESS_ALLOWED_ACE_TYPE &&
      header->AceType != ACCESS_DENIED_ACE_TYPE) {
    return ERROR_SUCCESS;
  }
  allowed = (ACCESS_ALLOWED_ACE *)header;
  fields[2] = allowed->Mask;
  if (!ConvertSidToStringSidW((PSID)&allowed->SidStart, sid)) {
    *sid = NULL;
    return GetLastError();
  }
  return ERROR_SUCCESS;
}

/* The owner of the entry at path, in the string form
 * ConvertSidToStringSidW writes, to *sid for the caller to release with
 * LocalFree.  Returns 0 or the Win32 error code. */
unsigned long nn_test_owner_read(const wchar_t *path, wchar_t **sid) {
  PSID owner = NULL;
  PSECURITY_DESCRIPTOR read = NULL;
  DWORD error;

  *sid = NULL;
  error = GetNamedSecurityInfoW(path, SE_FILE_OBJECT, OWNER_SECURITY_INFORMATION,
                                &owner, NULL, NULL, NULL, &read);
  if (error != ERROR_SUCCESS) {
    return error;
  }
  if (owner == NULL) {
    error = ERROR_INVALID_SECURITY_DESCR;
  } else if (!ConvertSidToStringSidW(owner, sid)) {
    error = GetLastError();
    *sid = NULL;
  }
  LocalFree(read);
  return error;
}
