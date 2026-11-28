/* sys/wait.h -- POSIX's, for the processes w32_spawnve starts.

   A status is encoded as on Linux: an exit code in the second byte, or a
   signal number in the low seven bits.  waitpid maps a Windows exit code to
   one the way busybox-w32 does (proc.c says how). */

#ifndef BASH_W32_SYS_WAIT_H
#define BASH_W32_SYS_WAIT_H

#include <sys/types.h>

#define WNOHANG		1
#define WUNTRACED	2

#define WIFEXITED(s)	(((s) & 0x7f) == 0)
#define WEXITSTATUS(s)	(((s) >> 8) & 0xff)
#define WIFSIGNALED(s)	(((s) & 0x7f) != 0 && ((s) & 0x7f) != 0x7f)
#define WTERMSIG(s)	((s) & 0x7f)
#define WIFSTOPPED(s)	(((s) & 0xff) == 0x7f)
#define WSTOPSIG(s)	WEXITSTATUS (s)
#define WIFCONTINUED(s)	0
#define WCOREDUMP(s)	0

extern pid_t waitpid (pid_t, int *, int);
extern pid_t wait (int *);

#endif /* BASH_W32_SYS_WAIT_H */
