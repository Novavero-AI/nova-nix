/* unistd.h -- MinGW-w64's, with the process and descriptor functions bash
   uses.  Those msvcrt has under the POSIX name but with other behaviour
   (dup2's return value, close-on-exec, descriptors not handles kept apart,
   backslashes in getcwd's answer) are renamed to the replacements in
   io.c. */

#include_next <unistd.h>

#ifndef BASH_W32_UNISTD_H
#define BASH_W32_UNISTD_H

#include <io.h>
#include <direct.h>
#include <process.h>
#include <w32compat.h>

/* The interfaces here are POSIX's, as bash's wait status, terminal and
   signal code expects of a system that says so. */
#define _POSIX_VERSION 200112L

/* mingw-w64's alarm, which bash uses for read -t and TMOUT, schedules
   nothing. */
extern unsigned int alarm (unsigned int);
extern int gethostname (char *, size_t);
extern char *ttyname (int);

#define close(fd)	w32_close (fd)
#define dup(fd)		w32_dup (fd)
#define dup2(from, to)	w32_dup2 (from, to)
#define pipe(fds)	w32_pipe (fds)
#define access(p, m)	w32_access (p, m)
#define getcwd(b, s)	w32_getcwd (b, s)

#endif /* BASH_W32_UNISTD_H */
