/* w32compat.h -- the POSIX interfaces bash needs and MinGW-w64 on msvcrt
   lacks, implemented in this directory's C files over the Windows API.

   The replacement headers beside this one (signal.h, unistd.h, fcntl.h,
   sys/stat.h, ...) include the MinGW header of the same name and then this
   one, so bash, and configure's tests of it, see one POSIX-shaped system.
   Functions msvcrt has under the POSIX name but with other behaviour are
   renamed to their replacements by function-like macros, which leave
   `struct stat' and the like alone. */

#ifndef BASH_W32COMPAT_H
#define BASH_W32COMPAT_H

#include <sys/types.h>
#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Windows has no user or group ids.  Every file and the process share
   this one, so a file's owner permission bits are the ones that apply. */
#define W32_ID 4095

/* The descriptors msvcrt can hold: 64 blocks of 32. */
#define W32_MAX_FDS 2048

/* Descriptors and files. */
extern int w32_open (const char *, int, ...);
extern int w32_close (int);
extern int w32_dup (int);
extern int w32_dup2 (int, int);
extern int w32_pipe (int[2]);
extern int w32_access (const char *, int);
extern char *w32_getcwd (char *, size_t);
extern int w32_cloexec (int);
extern int w32_append (int);
extern int getdtablesize (void);

/* Processes. */
extern pid_t fork (void);
extern pid_t w32_spawnve (const char *, char *const *, char *const *,
			  const unsigned char *, int);
extern char *w32_executable_path (void);
extern pid_t getppid (void);
extern pid_t getpgrp (void);
extern int setpgid (pid_t, pid_t);
extern uid_t getuid (void);
extern uid_t geteuid (void);
extern gid_t getgid (void);
extern gid_t getegid (void);
extern int setuid (uid_t);
extern int setgid (gid_t);
extern int getgroups (int, gid_t *);

/* Memory, for bash's malloc. */
extern void *sbrk (ptrdiff_t);
extern int getpagesize (void);

/* What the shell's main calls first: binary descriptors throughout. */
extern void w32_init (void);

#ifdef __cplusplus
}
#endif

#endif /* BASH_W32COMPAT_H */
