/* signals.c -- POSIX signal handling for bash on MinGW-w64 and msvcrt.

   msvcrt delivers its own signals (Ctrl+C as SIGINT, and SIGTERM, SIGBREAK,
   SIGABRT, SIGFPE, SIGILL and SIGSEGV) to one handler each, and resets it
   to the default before it calls it.  The handlers bash installs are kept
   here, msvcrt calls a trampoline that reinstalls itself and runs them,
   and the process's mask holds back what bash blocks until it unblocks it.
   The other signals reach the shell only from itself, by kill or raise of
   its own pid.  A signal whose action is the default ends the process with
   the signal in its exit code's top byte, which waitpid reads back as that
   signal, as busybox-w32 does. */

#include <errno.h>
#include <stdlib.h>
/* Leaves out winsock, whose gethostname takes an int length. */
#define WIN32_LEAN_AND_MEAN
#include <windows.h>

#include <signal.h>
#include <string.h>
#include <unistd.h>

/* Where a signal sits in a process's exit code (proc.c reads it back). */
#define EXIT_SIGNAL_SHIFT 24

typedef void (*handler_t) (int);

static handler_t handlers[NSIG];
static sigset_t blocked, pending;

static int
valid_signal (int sig)
{
  return sig > 0 && sig < NSIG;
}

static sigset_t
bit (int sig)
{
  return (sigset_t) 1 << sig;
}

/* Whether msvcrt raises SIG itself. */
static int
crt_signal (int sig)
{
  switch (sig)
    {
    case SIGINT:
    case SIGILL:
    case SIGFPE:
    case SIGSEGV:
    case SIGTERM:
    case SIGBREAK:
    case SIGABRT:
      return 1;
    default:
      return 0;
    }
}

static void
deliver (int sig)
{
  handler_t h;

  if (blocked & bit (sig))
    {
      pending |= bit (sig);
      return;
    }
  h = handlers[sig];
  if (h == SIG_IGN)
    return;
  if (h == SIG_DFL)
    {
      if (sig == SIGCHLD)
	return;
      _exit (sig << EXIT_SIGNAL_SHIFT);
    }
  h (sig);
}

static void
trampoline (int sig)
{
  signal (sig, trampoline);
  deliver (sig);
}

/* Deliver what is pending and no longer blocked. */
static void
deliver_pending (void)
{
  int sig;

  for (sig = 1; sig < NSIG; sig++)
    if ((pending & bit (sig)) && !(blocked & bit (sig)))
      {
	pending &= ~bit (sig);
	deliver (sig);
      }
}

int
sigaction (int sig, const struct sigaction *act, struct sigaction *old)
{
  if (!valid_signal (sig) || (act && (sig == SIGKILL) && act->sa_handler != SIG_DFL))
    {
      errno = EINVAL;
      return -1;
    }
  if (old)
    {
      old->sa_handler = handlers[sig];
      old->sa_mask = 0;
      old->sa_flags = 0;
    }
  if (act)
    {
      handlers[sig] = act->sa_handler;
      if (crt_signal (sig))
	signal (sig, (act->sa_handler == SIG_DFL || act->sa_handler == SIG_IGN)
		? act->sa_handler : trampoline);
    }
  return 0;
}

int
sigprocmask (int how, const sigset_t *set, sigset_t *old)
{
  if (old)
    *old = blocked;
  if (set)
    switch (how)
      {
      case SIG_BLOCK:
	blocked |= *set;
	break;
      case SIG_UNBLOCK:
	blocked &= ~*set;
	break;
      case SIG_SETMASK:
	blocked = *set;
	break;
      default:
	errno = EINVAL;
	return -1;
      }
  blocked &= ~bit (SIGKILL);
  deliver_pending ();
  return 0;
}

/* The mask is MASK while what is pending under it is delivered.  No
   signal from outside the shell but msvcrt's arrives while it waits, so it
   waits no longer than that. */
int
sigsuspend (const sigset_t *mask)
{
  sigset_t saved;

  saved = blocked;
  blocked = *mask & ~bit (SIGKILL);
  deliver_pending ();
  blocked = saved;
  errno = EINTR;
  return -1;
}

int
sigpending (sigset_t *set)
{
  *set = pending;
  return 0;
}

int
sigemptyset (sigset_t *set)
{
  *set = 0;
  return 0;
}

int
sigfillset (sigset_t *set)
{
  *set = ~(sigset_t) 0;
  return 0;
}

int
sigaddset (sigset_t *set, int sig)
{
  if (!valid_signal (sig))
    {
      errno = EINVAL;
      return -1;
    }
  *set |= bit (sig);
  return 0;
}

int
sigdelset (sigset_t *set, int sig)
{
  if (!valid_signal (sig))
    {
      errno = EINVAL;
      return -1;
    }
  *set &= ~bit (sig);
  return 0;
}

int
sigismember (const sigset_t *set, int sig)
{
  if (!valid_signal (sig))
    {
      errno = EINVAL;
      return -1;
    }
  return (*set & bit (sig)) != 0;
}

/* What each signal is, as glibc describes it. */
char *
strsignal (int sig)
{
  switch (sig)
    {
    case SIGHUP: return (char *) "Hangup";
    case SIGINT: return (char *) "Interrupt";
    case SIGQUIT: return (char *) "Quit";
    case SIGILL: return (char *) "Illegal instruction";
    case SIGTRAP: return (char *) "Trace/breakpoint trap";
    case SIGFPE: return (char *) "Floating point exception";
    case SIGKILL: return (char *) "Killed";
    case SIGUSR1: return (char *) "User defined signal 1";
    case SIGSEGV: return (char *) "Segmentation fault";
    case SIGUSR2: return (char *) "User defined signal 2";
    case SIGPIPE: return (char *) "Broken pipe";
    case SIGALRM: return (char *) "Alarm clock";
    case SIGTERM: return (char *) "Terminated";
    case SIGCHLD: return (char *) "Child exited";
    case SIGBREAK: return (char *) "Break";
    case SIGABRT: return (char *) "Aborted";
    default: return (char *) "Unknown signal";
    }
}

/* The shell's own pid gets the signal at once.  Another process can only
   be asked whether it exists, with signal 0. */
int
kill (pid_t pid, int sig)
{
  HANDLE process;

  if (sig != 0 && !valid_signal (sig))
    {
      errno = EINVAL;
      return -1;
    }
  if (pid == 0 || pid == (pid_t) GetCurrentProcessId ())
    {
      if (sig != 0)
	deliver (sig);
      return 0;
    }
  if (sig != 0)
    {
      errno = ENOSYS;
      return -1;
    }
  process = OpenProcess (SYNCHRONIZE, FALSE, (DWORD) pid);
  if (process == 0)
    {
      errno = ESRCH;
      return -1;
    }
  CloseHandle (process);
  return 0;
}
