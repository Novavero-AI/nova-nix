/* Platform filesystem capabilities the store layer probes at runtime.
 * macOS-only functionality.  Every other platform compiles the constant
 * -1 stub because the cabal c-sources entry is unconditional and the
 * C99 strict job lints every C file under cbits on Linux; the Haskell
 * side picks the platform's answer at compile time and never calls the
 * stub. */
#ifndef NN_DARWINFS_H
#define NN_DARWINFS_H

/* Whether the filesystem holding PATH (which must exist) compares names
 * case-sensitively: pathconf(_PC_CASE_SENSITIVE), a Darwin extension of
 * the POSIX call.  Returns 1 when sensitive, 0 when folding, and -1 when
 * there is no answer - the path does not exist, the filesystem does not
 * implement the query, or a non-macOS build - and the caller falls back
 * to the platform default. */
int nn_path_case_sensitive(const char *path);

#endif
