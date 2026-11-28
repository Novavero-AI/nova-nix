/* proc.c -- processes for bash on MinGW-w64 and msvcrt.

   Windows has no fork.  A program bash runs is started with
   CreateProcess by w32_spawnve, from the shell itself once it has made
   the program's pipes and redirections its own descriptors, and the
   program inherits exactly the descriptors it would on POSIX: those that
   do not close on exec and that the caller does not exclude.  They reach
   it as msvcrt passes descriptors to a child, in STARTUPINFO's
   lpReserved2, and as its standard handles, so a program built on any C
   runtime finds its standard input and output.  The handles it inherits
   are listed, so no other handle of the shell's reaches it.

   Each process started is kept here by its pid until waitpid reaps it. */

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
/* Leaves out winsock, whose gethostname takes an int length. */
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <tlhelp32.h>

#include <fcntl.h>
#include <pwd.h>
#include <signal.h>
#include <sys/stat.h>
#include <sys/times.h>
#include <sys/wait.h>
#include <unistd.h>

extern int w32_append (int);

/* msvcrt's descriptor flags in lpReserved2. */
#define CRT_FOPEN 0x01
#define CRT_FAPPEND 0x20
#define CRT_FPIPE 0x08
#define CRT_FDEV 0x40
#define CRT_FTEXT 0x80

/* WaitForMultipleObjects waits on at most this many processes at once,
   so that is how many unreaped children the shell may have. */
#define MAX_CHILDREN MAXIMUM_WAIT_OBJECTS

/* Windows exit codes that mean a signal, as busybox-w32 maps them.  A
   process killed as by a signal exits with the signal in its top byte. */
#define EXIT_ACCESS_VIOLATION 0xC0000005u
#define EXIT_CONTROL_C 0xC000013Au
#define EXIT_SIGNAL_SHIFT 24

/* The most a Windows command line holds, in characters. */
#define MAX_COMMAND_LINE 32767

static struct child
{
  pid_t pid;
  HANDLE process;
} children[MAX_CHILDREN];
static int nchildren;

/* The user and system time of the children reaped so far, in 100 ns. */
static ULONGLONG child_user_time, child_system_time;

pid_t
fork (void)
{
  errno = ENOSYS;
  return -1;
}

/* ---------------------------------------------------------------------- */
/* Command lines and environments                                         */
/* ---------------------------------------------------------------------- */

/* A growable byte buffer. */
struct buffer
{
  char *bytes;
  size_t len, size;
};

static int
put (struct buffer *b, const char *s, size_t n)
{
  char *grown;
  size_t want;

  if (b->len + n + 1 > b->size)
    {
      want = (b->size ? b->size * 2 : 256);
      while (want < b->len + n + 1)
	want *= 2;
      grown = realloc (b->bytes, want);
      if (grown == 0)
	return -1;
      b->bytes = grown;
      b->size = want;
    }
  memcpy (b->bytes + b->len, s, n);
  b->len += n;
  b->bytes[b->len] = '\0';
  return 0;
}

static int
put_char (struct buffer *b, char c)
{
  return put (b, &c, 1);
}

/* ARG as the C runtime's argument parser reads it back: quoted when it
   is empty or holds a space, tab or quote, each quote escaped, and the
   backslashes before a quote or the closing quote doubled. */
static int
put_argument (struct buffer *b, const char *arg)
{
  const char *p;
  size_t slashes, i;

  if (*arg && strpbrk (arg, " \t\n\v\"") == 0)
    return put (b, arg, strlen (arg));
  if (put_char (b, '"') < 0)
    return -1;
  slashes = 0;
  for (p = arg; *p; p++)
    {
      if (*p == '\\')
	{
	  slashes++;
	  continue;
	}
      if (*p == '"')
	for (i = 0; i < slashes + 1; i++)
	  if (put_char (b, '\\') < 0)
	    return -1;
      for (i = 0; i < slashes; i++)
	if (put_char (b, '\\') < 0)
	  return -1;
      slashes = 0;
      if (put_char (b, *p) < 0)
	return -1;
    }
  for (i = 0; i < slashes * 2; i++)
    if (put_char (b, '\\') < 0)
      return -1;
  return put_char (b, '"');
}

/* PATH's value as a Windows program reads it, its elements joined with
   ';'.  An element ends at ';', or at ':' unless the ':' ends a drive
   letter at the element's start, as busybox-w32 splits PATH. */
static int
put_path_value (struct buffer *b, const char *value)
{
  const char *p;

  p = value;
  while (*p)
    {
      if (*p != ':' && *p != ';')
	{
	  if (((*p >= 'A' && *p <= 'Z') || (*p >= 'a' && *p <= 'z')) && p[1] == ':')
	    {
	      if (put (b, p, 2) < 0)
		return -1;
	      p += 2;
	    }
	  while (*p && *p != ':' && *p != ';')
	    if (put_char (b, *p++) < 0)
	      return -1;
	}
      if (*p)
	{
	  if (put_char (b, ';') < 0)
	    return -1;
	  p++;
	}
    }
  return 0;
}

/* ENVP as an environment block: each string with its NUL, PATH's value
   respelled for Windows, and a NUL after the last. */
static int
put_environment (struct buffer *b, char *const *envp)
{
  for (; envp && *envp; envp++)
    {
      if (strncmp (*envp, "PATH=", 5) == 0)
	{
	  if (put (b, "PATH=", 5) < 0 || put_path_value (b, *envp + 5) < 0
	      || put (b, "", 1) < 0)
	    return -1;
	}
      else if (put (b, *envp, strlen (*envp) + 1) < 0)
	return -1;
    }
  return put (b, "", 1);
}

/* ---------------------------------------------------------------------- */
/* Spawning                                                               */
/* ---------------------------------------------------------------------- */

static int
errno_from_error (DWORD error)
{
  switch (error)
    {
    case ERROR_FILE_NOT_FOUND:
    case ERROR_PATH_NOT_FOUND:
    case ERROR_INVALID_NAME:
    case ERROR_BAD_PATHNAME:
      return ENOENT;
    case ERROR_ACCESS_DENIED:
    case ERROR_SHARING_VIOLATION:
      return EACCES;
    case ERROR_BAD_EXE_FORMAT:
    case ERROR_EXE_MACHINE_TYPE_MISMATCH:
      return ENOEXEC;
    case ERROR_NOT_ENOUGH_MEMORY:
    case ERROR_OUTOFMEMORY:
      return ENOMEM;
    case ERROR_FILENAME_EXCED_RANGE:
      return E2BIG;
    default:
      return EINVAL;
    }
}

/* PATH with backslashes, and .exe added where PATH names no file and has
   no extension of its own, as an interpreter line names a program. */
static char *
application_name (const char *path)
{
  char *name, *p, *base;
  size_t len;

  len = strlen (path);
  name = malloc (len + 5);
  if (name == 0)
    return 0;
  memcpy (name, path, len + 1);
  for (p = name; *p; p++)
    if (*p == '/')
      *p = '\\';
  if (GetFileAttributesA (name) == INVALID_FILE_ATTRIBUTES)
    {
      base = strrchr (name, '\\');
      if (strchr (base ? base : name, '.') == 0)
	strcat (name, ".exe");
    }
  return name;
}

static void
close_handles (HANDLE *handles, int n)
{
  int i;

  for (i = 0; i < n; i++)
    if (handles[i] != INVALID_HANDLE_VALUE)
      CloseHandle (handles[i]);
}

/* Start PATH with ARGV and ENVP, giving it the shell's descriptors but
   those that close on exec and those EXCLUDE marks, a byte for each of
   the first NEXCLUDE descriptors, as bash's fd_bitmap holds them.
   Returns its pid, or -1 with errno set. */
pid_t
w32_spawnve (const char *path, char *const *argv, char *const *envp,
	     const unsigned char *exclude, int nexclude)
{
  struct buffer command = { 0, 0, 0 }, environment = { 0, 0, 0 };
  HANDLE handles[W32_MAX_FDS], inherited[W32_MAX_FDS];
  unsigned char flags[W32_MAX_FDS];
  int nfds, ninherited, fd, mode, saved;
  char *app, *reserved;
  intptr_t os;
  DWORD type;
  STARTUPINFOEXA si;
  PROCESS_INFORMATION pi;
  SIZE_T attrsize;
  char *const *a;
  pid_t pid;
  BOOL ok;

  if (nchildren == MAX_CHILDREN)
    {
      errno = EAGAIN;
      return -1;
    }

  for (a = argv; a && *a; a++)
    if ((a != argv && put_char (&command, ' ') < 0) || put_argument (&command, *a) < 0)
      goto nomem;
  if (command.len >= MAX_COMMAND_LINE)
    {
      free (command.bytes);
      errno = E2BIG;
      return -1;
    }
  if (put_environment (&environment, envp) < 0)
    goto nomem;
  app = application_name (path);
  if (app == 0)
    goto nomem;

  /* What has been written through stdio reaches the descriptors before
     the program writes to them. */
  fflush (0);

  nfds = 0;
  ninherited = 0;
  for (fd = 0; fd < W32_MAX_FDS; fd++)
    {
      handles[fd] = INVALID_HANDLE_VALUE;
      flags[fd] = 0;
      os = _get_osfhandle (fd);
      if (os == -1 || w32_cloexec (fd) || (fd < nexclude && exclude[fd]))
	continue;
      if (!DuplicateHandle (GetCurrentProcess (), (HANDLE) os, GetCurrentProcess (),
			    &handles[fd], 0, TRUE, DUPLICATE_SAME_ACCESS))
	{
	  handles[fd] = INVALID_HANDLE_VALUE;
	  continue;
	}
      inherited[ninherited++] = handles[fd];
      type = GetFileType ((HANDLE) os) & ~FILE_TYPE_REMOTE;
      mode = _setmode (fd, O_BINARY);
      if (mode != -1)
	_setmode (fd, mode);
      flags[fd] = CRT_FOPEN
	| (type == FILE_TYPE_PIPE ? CRT_FPIPE : 0)
	| (type == FILE_TYPE_CHAR ? CRT_FDEV : 0)
	| (w32_append (fd) ? CRT_FAPPEND : 0)
	| (mode == O_TEXT ? CRT_FTEXT : 0);
      nfds = fd + 1;
    }

  /* lpReserved2: the count, a flag byte for each descriptor, then each
     descriptor's handle, unaligned. */
  reserved = malloc (sizeof (int) + nfds * (1 + sizeof (HANDLE)));
  if (reserved == 0)
    {
      close_handles (inherited, ninherited);
      free (app);
      goto nomem;
    }
  memcpy (reserved, &nfds, sizeof (int));
  memcpy (reserved + sizeof (int), flags, nfds);
  memcpy (reserved + sizeof (int) + nfds, handles, nfds * sizeof (HANDLE));

  /* The handle list that keeps every other handle from the program needs
     the extended form of STARTUPINFO; with no handle to inherit, the
     program inherits none and the plain form does. */
  memset (&si, 0, sizeof (si));
  si.StartupInfo.cb = ninherited > 0 ? sizeof (si) : sizeof (si.StartupInfo);
  si.StartupInfo.dwFlags = STARTF_USESTDHANDLES;
  si.StartupInfo.hStdInput = nfds > 0 ? handles[0] : INVALID_HANDLE_VALUE;
  si.StartupInfo.hStdOutput = nfds > 1 ? handles[1] : INVALID_HANDLE_VALUE;
  si.StartupInfo.hStdError = nfds > 2 ? handles[2] : INVALID_HANDLE_VALUE;
  si.StartupInfo.cbReserved2 = (WORD) (sizeof (int) + nfds * (1 + sizeof (HANDLE)));
  si.StartupInfo.lpReserved2 = (LPBYTE) reserved;

  si.lpAttributeList = 0;
  if (ninherited > 0)
    {
      attrsize = 0;
      InitializeProcThreadAttributeList (0, 1, 0, &attrsize);
      si.lpAttributeList = malloc (attrsize);
      if (si.lpAttributeList == 0
	  || !InitializeProcThreadAttributeList (si.lpAttributeList, 1, 0, &attrsize)
	  || !UpdateProcThreadAttribute (si.lpAttributeList, 0, PROC_THREAD_ATTRIBUTE_HANDLE_LIST,
					 inherited, ninherited * sizeof (HANDLE), 0, 0))
	{
	  saved = errno_from_error (GetLastError ());
	  free (si.lpAttributeList);
	  close_handles (inherited, ninherited);
	  free (reserved);
	  free (app);
	  free (command.bytes);
	  free (environment.bytes);
	  errno = saved;
	  return -1;
	}
    }

  ok = CreateProcessA (app, command.bytes, 0, 0, ninherited > 0,
		       ninherited > 0 ? EXTENDED_STARTUPINFO_PRESENT : 0,
		       environment.bytes, 0, &si.StartupInfo, &pi);
  saved = ok ? 0 : errno_from_error (GetLastError ());

  if (si.lpAttributeList)
    {
      DeleteProcThreadAttributeList (si.lpAttributeList);
      free (si.lpAttributeList);
    }
  close_handles (inherited, ninherited);
  free (reserved);
  free (app);
  free (command.bytes);
  free (environment.bytes);

  if (!ok)
    {
      errno = saved;
      return -1;
    }
  CloseHandle (pi.hThread);
  pid = (pid_t) pi.dwProcessId;
  children[nchildren].pid = pid;
  children[nchildren].process = pi.hProcess;
  nchildren++;
  return pid;

nomem:
  free (command.bytes);
  free (environment.bytes);
  errno = ENOMEM;
  return -1;
}

/* ---------------------------------------------------------------------- */
/* Waiting                                                                */
/* ---------------------------------------------------------------------- */

/* A Windows exit code as a wait status, as busybox-w32 maps it: an access
   violation as SIGSEGV, Ctrl+C as SIGINT, a signal in the top byte as that
   signal, and otherwise the low byte, or 255 where the low byte is 0 and
   the code is not. */
static int
exit_status (DWORD code)
{
  DWORD sig;
  int status;

  if (code == EXIT_ACCESS_VIOLATION)
    return SIGSEGV;
  if (code == EXIT_CONTROL_C)
    return SIGINT;
  sig = code >> EXIT_SIGNAL_SHIFT;
  if (sig != 0 && code == sig << EXIT_SIGNAL_SHIFT && sig < NSIG)
    return (int) sig;
  status = code & 0xff;
  if (code != 0 && status == 0)
    status = 255;
  return status << 8;
}

static ULONGLONG
filetime (FILETIME t)
{
  return ((ULONGLONG) t.dwHighDateTime << 32) | t.dwLowDateTime;
}

/* Reap the child at INDEX, which has exited. */
static pid_t
reap (int index, int *status)
{
  FILETIME created, exited, kernel, user;
  DWORD code;
  pid_t pid;

  pid = children[index].pid;
  if (!GetExitCodeProcess (children[index].process, &code))
    code = 255;
  if (GetProcessTimes (children[index].process, &created, &exited, &kernel, &user))
    {
      child_user_time += filetime (user);
      child_system_time += filetime (kernel);
    }
  CloseHandle (children[index].process);
  children[index] = children[--nchildren];
  if (status)
    *status = exit_status (code);
  return pid;
}

pid_t
waitpid (pid_t pid, int *status, int options)
{
  HANDLE handles[MAX_CHILDREN];
  DWORD timeout, r;
  int i;

  timeout = (options & WNOHANG) ? 0 : INFINITE;
  if (pid > 0)
    {
      for (i = 0; i < nchildren; i++)
	if (children[i].pid == pid)
	  break;
      if (i == nchildren)
	{
	  errno = ECHILD;
	  return -1;
	}
      r = WaitForSingleObject (children[i].process, timeout);
      if (r == WAIT_TIMEOUT)
	return 0;
      if (r != WAIT_OBJECT_0)
	{
	  errno = EINVAL;
	  return -1;
	}
      return reap (i, status);
    }

  /* Any child: the shell has no process groups. */
  if (nchildren == 0)
    {
      errno = ECHILD;
      return -1;
    }
  for (i = 0; i < nchildren; i++)
    handles[i] = children[i].process;
  r = WaitForMultipleObjects (nchildren, handles, FALSE, timeout);
  if (r == WAIT_TIMEOUT)
    return 0;
  if (r >= WAIT_OBJECT_0 + (DWORD) nchildren)
    {
      errno = EINVAL;
      return -1;
    }
  return reap (r - WAIT_OBJECT_0, status);
}

pid_t
wait (int *status)
{
  return waitpid (-1, status, 0);
}

/* ---------------------------------------------------------------------- */
/* Ids and the password database                                          */
/* ---------------------------------------------------------------------- */

static char executable_path[MAX_PATH * 4];

/* The running program's path, with forward slashes. */
char *
w32_executable_path (void)
{
  DWORD len;
  char *p;

  if (executable_path[0] == '\0')
    {
      len = GetModuleFileNameA (0, executable_path, sizeof (executable_path));
      if (len == 0 || len == sizeof (executable_path))
	{
	  executable_path[0] = '\0';
	  return (char *) "bash";
	}
      for (p = executable_path; *p; p++)
	if (*p == '\\')
	  *p = '/';
    }
  return executable_path;
}

pid_t
getppid (void)
{
  PROCESSENTRY32 entry;
  HANDLE snapshot;
  DWORD self;
  pid_t parent;

  self = GetCurrentProcessId ();
  parent = 1;
  snapshot = CreateToolhelp32Snapshot (TH32CS_SNAPPROCESS, 0);
  if (snapshot == INVALID_HANDLE_VALUE)
    return parent;
  entry.dwSize = sizeof (entry);
  if (Process32First (snapshot, &entry))
    do
      if (entry.th32ProcessID == self)
	{
	  parent = (pid_t) entry.th32ParentProcessID;
	  break;
	}
    while (Process32Next (snapshot, &entry));
  CloseHandle (snapshot);
  return parent;
}

pid_t
getpgrp (void)
{
  return (pid_t) GetCurrentProcessId ();
}

int
setpgid (pid_t pid, pid_t pgid)
{
  (void) pid;
  (void) pgid;
  errno = ENOSYS;
  return -1;
}

uid_t getuid (void) { return W32_ID; }
uid_t geteuid (void) { return W32_ID; }
gid_t getgid (void) { return W32_ID; }
gid_t getegid (void) { return W32_ID; }

int
setuid (uid_t id)
{
  if (id == W32_ID)
    return 0;
  errno = EPERM;
  return -1;
}

int
setgid (gid_t id)
{
  if (id == W32_ID)
    return 0;
  errno = EPERM;
  return -1;
}

int
getgroups (int size, gid_t *list)
{
  if (size == 0)
    return 1;
  if (size < 1)
    {
      errno = EINVAL;
      return -1;
    }
  list[0] = W32_ID;
  return 1;
}

static struct passwd user;
static char user_name[256 + 1];
static char user_home[MAX_PATH * 4];
static char user_shell[MAX_PATH * 4];
static int pwent_done;

/* A copy of VALUE in BUF of SIZE bytes, with forward slashes. */
static char *
slashed (char *buf, size_t size, const char *value)
{
  char *p;

  strncpy (buf, value, size - 1);
  buf[size - 1] = '\0';
  for (p = buf; *p; p++)
    if (*p == '\\')
      *p = '/';
  return buf;
}

static struct passwd *
current_user (void)
{
  DWORD len;
  const char *home;

  if (user.pw_name)
    return &user;
  len = sizeof (user_name);
  if (!GetUserNameA (user_name, &len))
    strcpy (user_name, "user");
  home = getenv ("HOME");
  if (home == 0)
    home = getenv ("USERPROFILE");
  slashed (user_home, sizeof (user_home), home ? home : "C:/");
  slashed (user_shell, sizeof (user_shell), w32_executable_path ());
  user.pw_name = user_name;
  user.pw_passwd = (char *) "*";
  user.pw_uid = W32_ID;
  user.pw_gid = W32_ID;
  user.pw_gecos = user_name;
  user.pw_dir = user_home;
  user.pw_shell = user_shell;
  return &user;
}

struct passwd *
getpwuid (uid_t id)
{
  return id == W32_ID ? current_user () : 0;
}

struct passwd *
getpwnam (const char *name)
{
  struct passwd *pw;

  pw = current_user ();
  return strcasecmp (name, pw->pw_name) == 0 ? pw : 0;
}

struct passwd *
getpwent (void)
{
  if (pwent_done)
    return 0;
  pwent_done = 1;
  return current_user ();
}

void setpwent (void) { pwent_done = 0; }
void endpwent (void) { pwent_done = 0; }

/* ---------------------------------------------------------------------- */
/* Times and memory                                                       */
/* ---------------------------------------------------------------------- */

/* 100 ns intervals in a clock() tick. */
#define FILETIME_PER_TICK (10000000 / CLOCKS_PER_SEC)

clock_t
times (struct tms *t)
{
  FILETIME created, exited, kernel, user_time;

  if (GetProcessTimes (GetCurrentProcess (), &created, &exited, &kernel, &user_time))
    {
      t->tms_utime = (clock_t) (filetime (user_time) / FILETIME_PER_TICK);
      t->tms_stime = (clock_t) (filetime (kernel) / FILETIME_PER_TICK);
    }
  else
    t->tms_utime = t->tms_stime = 0;
  t->tms_cutime = (clock_t) (child_user_time / FILETIME_PER_TICK);
  t->tms_cstime = (clock_t) (child_system_time / FILETIME_PER_TICK);
  return clock ();
}

int
getpagesize (void)
{
  SYSTEM_INFO info;

  GetSystemInfo (&info);
  return (int) info.dwPageSize;
}

/* bash's malloc takes its memory from sbrk.  The break moves through a
   region reserved on the first call, whose pages are committed as it
   grows and released as it shrinks. */
#define HEAP_RESERVE_BYTES ((SIZE_T) 4 << 30)

static char *heap_base, *heap_break, *heap_committed;

void *
sbrk (ptrdiff_t increment)
{
  char *old, *want, *commit_to;
  SIZE_T page;

  if (heap_base == 0)
    {
      heap_base = VirtualAlloc (0, HEAP_RESERVE_BYTES, MEM_RESERVE, PAGE_NOACCESS);
      if (heap_base == 0)
	{
	  errno = ENOMEM;
	  return (void *) -1;
	}
      heap_break = heap_committed = heap_base;
    }
  old = heap_break;
  if ((increment > 0 && (SIZE_T) increment > (SIZE_T) (heap_base + HEAP_RESERVE_BYTES - heap_break))
      || (increment < 0 && (SIZE_T) -increment > (SIZE_T) (heap_break - heap_base)))
    {
      errno = ENOMEM;
      return (void *) -1;
    }
  want = heap_break + increment;
  page = (SIZE_T) getpagesize ();
  commit_to = heap_base + (((SIZE_T) (want - heap_base) + page - 1) / page) * page;
  if (commit_to > heap_committed)
    {
      if (VirtualAlloc (heap_committed, commit_to - heap_committed, MEM_COMMIT, PAGE_READWRITE) == 0)
	{
	  errno = ENOMEM;
	  return (void *) -1;
	}
    }
  else if (commit_to < heap_committed)
    VirtualFree (commit_to, heap_committed - commit_to, MEM_DECOMMIT);
  heap_committed = commit_to;
  heap_break = want;
  return old;
}
