/* Platform filesystem capabilities the store layer uses on Windows.
 * The cabal c-sources entry is unconditional and the C99 strict job
 * lints every C file under cbits on Linux, so every other platform
 * compiles a constant 0 stub of the case-sensitivity call; the Haskell
 * side picks the platform's answer at compile time and never calls it.
 * The access-control calls are declared and defined on Windows alone,
 * the only platform that calls them. */
#ifndef NN_WINFS_H
#define NN_WINFS_H

#include <wchar.h>

/* Make an EMPTY directory's namespace case-sensitive (the NTFS
 * per-directory flag, the same mechanism WSL uses to host Linux trees).
 * Returns 1 on success, 0 when refused or unsupported - a non-NTFS
 * volume, policy, a non-empty directory, or a non-Windows build - and
 * the caller falls back to the case-hack.  The check guards external
 * input (NAR trees choose which directories need it), so the failure
 * path is a live branch, never an assert. */
int nn_dir_set_case_sensitive(const wchar_t *path);

#ifdef _WIN32

/* The SID of the user this process runs as, its token's user, in the
 * string form ConvertSidToStringSidW writes ("S-1-5-21-..."), written to
 * *sid for the caller to release with LocalFree.  Returns 0, or the
 * Win32 error code of the call that failed, with *failed naming that
 * call (a static string) and *sid NULL. */
unsigned long nn_token_user_sid(wchar_t **sid, const char **failed);

/* Create the directory at path with the security descriptor the SDDL
 * string describes, in the one CreateDirectoryW call that makes it, so
 * the directory never exists with the access control list its parent
 * would pass down.  Returns 0, or the Win32 error code (a DWORD) of the
 * step that failed: ERROR_ALREADY_EXISTS when anything already has the
 * name, a file as well as a directory. */
unsigned long nn_dir_create_sddl(const wchar_t *path, const wchar_t *sddl);

/* The owner and DACL of the entry at path itself, a link read as itself
 * and not followed, as the self-relative security descriptor
 * GetSecurityInfo returns, written to *descriptor for the caller to
 * release with LocalFree, with its length in bytes in *length.  Returns
 * 0, or the Win32 error code of the call that failed, with *failed naming
 * that call (a static string) and *descriptor NULL: ERROR_FILE_NOT_FOUND
 * or ERROR_PATH_NOT_FOUND from CreateFileW when nothing has the name. */
unsigned long nn_entry_security(const wchar_t *path, void **descriptor,
                                unsigned long *length, const char **failed);

#endif

#endif
