/* Platform filesystem capabilities the store layer probes at runtime.
 * Windows-only functionality; other platforms compile the constant-failure
 * stubs so callers fall back without conditional compilation. */
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

#endif
