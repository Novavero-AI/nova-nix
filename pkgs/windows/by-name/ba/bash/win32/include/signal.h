/* signal.h -- msvcrt's, with the rest of the POSIX signal interface.

   msvcrt raises SIGINT, SIGILL, SIGFPE, SIGSEGV, SIGTERM, SIGBREAK and
   SIGABRT.  The signals added here are numbered as Linux numbers them
   wherever that number is free, are delivered only by the shell itself
   (kill and raise of its own pid), and so let bash's tables name them.
   msvcrt already uses 21 and 22, Linux's SIGTTIN and SIGTTOU, so the job
   control signals are left undefined, as bash expects on a system without
   job control. */

#include_next <signal.h>

#ifndef BASH_W32_SIGNAL_H
#define BASH_W32_SIGNAL_H

#include <sys/types.h>

#define SIGHUP	1
#define SIGQUIT	3
#define SIGTRAP	5
#define SIGKILL	9
#define SIGUSR1	10
#define SIGUSR2	12
#define SIGPIPE	13
#define SIGALRM	14
#define SIGCHLD	17

#ifndef _POSIX
typedef _sigset_t sigset_t;
#endif

struct sigaction
{
  void (*sa_handler) (int);
  sigset_t sa_mask;
  int sa_flags;
};

#define SA_RESTART	0x1

#define SIG_BLOCK	0
#define SIG_UNBLOCK	1
#define SIG_SETMASK	2

extern int sigaction (int, const struct sigaction *, struct sigaction *);
extern int sigprocmask (int, const sigset_t *, sigset_t *);
extern int sigsuspend (const sigset_t *);
extern int sigpending (sigset_t *);
extern int sigemptyset (sigset_t *);
extern int sigfillset (sigset_t *);
extern int sigaddset (sigset_t *, int);
extern int sigdelset (sigset_t *, int);
extern int sigismember (const sigset_t *, int);
extern int kill (pid_t, int);

#include <w32compat.h>

#endif /* BASH_W32_SIGNAL_H */
