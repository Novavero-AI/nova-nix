/* sys/stat.h -- msvcrt's, with stat answering as POSIX does for a file a
   program can run.  msvcrt marks a file executable by its extension alone;
   w32_stat also marks a regular file that begins with #! or MZ, and gives
   every file the process's id as its owner, so its owner bits are the ones
   that apply (io.c says how).  There are no symbolic links, so lstat is
   stat.  mkdir takes POSIX's mode, which Windows has no bits for. */

#include_next <sys/stat.h>

#ifndef BASH_W32_SYS_STAT_H
#define BASH_W32_SYS_STAT_H

/* msvcrt declares its one-argument mkdir here, before the macro below
   names the two-argument one. */
#include <io.h>
#include <direct.h>
#include <w32compat.h>

extern int w32_stat (const char *, struct stat *);
extern int w32_fstat (int, struct stat *);

/* There are no set-id or sticky bits, and no FIFOs: mkfifo fails with
   ENOSYS, and process substitution reads /dev/fd instead. */
#define S_ISUID	04000
#define S_ISGID	02000
#define S_ISVTX	01000
extern int mkfifo (const char *, mode_t);
extern int fchmod (int, mode_t);

extern int w32_mkdir (const char *, mode_t);

#define stat(p, b)	w32_stat (p, b)
#define lstat(p, b)	w32_stat (p, b)
#define fstat(fd, b)	w32_fstat (fd, b)
#define mkdir(p, m)	w32_mkdir (p, m)

#endif /* BASH_W32_SYS_STAT_H */
