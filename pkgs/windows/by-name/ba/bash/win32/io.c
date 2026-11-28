/* io.c -- descriptors and files for bash on MinGW-w64 and msvcrt.

   msvcrt keeps a descriptor's open flags and its Windows handle, but not
   the two things POSIX adds that a spawned program depends on: whether the
   descriptor closes on exec, and whether writes append.  The table here
   keeps both for every descriptor bash opens or duplicates, so that
   w32_spawnve can hand a program exactly the descriptors it would inherit
   on POSIX, each appending as it did here.

   Every file opens in binary mode: bash reads and writes bytes, and a
   script's output is the same on Windows as elsewhere. */

#include <errno.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
/* Leaves out winsock, whose gethostname takes an int length. */
#define WIN32_LEAN_AND_MEAN
#include <windows.h>

#include <fcntl.h>
#include <sys/stat.h>
#include <termios.h>
#include <unistd.h>

#undef close
#undef dup
#undef dup2
#undef pipe
#undef access
#undef getcwd
#undef open
#undef stat
#undef lstat
#undef fstat

/* The size msvcrt gives a pipe's buffer when bash makes one. */
#define PIPE_BUFFER_BYTES 65536

/* How many bytes of a file say whether it is a program: "#!" or "MZ". */
#define MAGIC_BYTES 2

/* A descriptor's flags beyond msvcrt's own. */
#define FD_FLAG_CLOEXEC 0x1
#define FD_FLAG_APPEND 0x2

static unsigned char fd_flags[W32_MAX_FDS];

static int
valid_fd (int fd)
{
  return fd >= 0 && fd < W32_MAX_FDS;
}

/* Whether FD closes when a program is spawned. */
int
w32_cloexec (int fd)
{
  return valid_fd (fd) && (fd_flags[fd] & FD_FLAG_CLOEXEC);
}

/* Whether writes to FD append. */
int
w32_append (int fd)
{
  return valid_fd (fd) && (fd_flags[fd] & FD_FLAG_APPEND);
}

/* The names POSIX gives devices, as Windows names them, or 0 for a name
   that is not one.  /dev/stdin, /dev/stdout, /dev/stderr and /dev/fd/N are
   descriptors, which w32_open duplicates instead. */
static const char *
device_name (const char *path)
{
  if (strcmp (path, "/dev/null") == 0)
    return "NUL";
  if (strcmp (path, "/dev/tty") == 0)
    return "CON";
  return 0;
}

/* The descriptor a /dev/fd/N, /dev/stdin, /dev/stdout or /dev/stderr
   path names, or -1 for any other path. */
static int
device_fd (const char *path)
{
  char *end;
  long n;

  if (strcmp (path, "/dev/stdin") == 0)
    return 0;
  if (strcmp (path, "/dev/stdout") == 0)
    return 1;
  if (strcmp (path, "/dev/stderr") == 0)
    return 2;
  if (strncmp (path, "/dev/fd/", 8) != 0 || path[8] < '0' || path[8] > '9')
    return -1;
  errno = 0;
  n = strtol (path + 8, &end, 10);
  if (*end != '\0' || errno != 0 || n >= W32_MAX_FDS)
    return -1;
  return (int) n;
}

int
w32_open (const char *path, int flags, ...)
{
  va_list ap;
  int mode, fd;
  const char *device;

  mode = 0;
  if (flags & O_CREAT)
    {
      va_start (ap, flags);
      mode = va_arg (ap, int);
      va_end (ap);
    }

  fd = device_fd (path);
  if (fd >= 0)
    return w32_dup (fd);

  device = device_name (path);
  fd = _open (device ? device : path,
	      (flags & ~O_NONBLOCK & ~O_TEXT) | O_BINARY | O_NOINHERIT, mode);
  if (fd >= 0 && valid_fd (fd))
    fd_flags[fd] = (flags & O_APPEND) ? FD_FLAG_APPEND : 0;
  return fd;
}

int
w32_close (int fd)
{
  int r;

  r = _close (fd);
  if (r == 0 && valid_fd (fd))
    fd_flags[fd] = 0;
  return r;
}

/* A duplicate keeps whether writes append and, as POSIX has it, does not
   close on exec. */
int
w32_dup (int fd)
{
  int nfd;

  nfd = _dup (fd);
  if (nfd >= 0 && valid_fd (nfd))
    fd_flags[nfd] = w32_append (fd) ? FD_FLAG_APPEND : 0;
  return nfd;
}

/* msvcrt's _dup2 answers 0, where POSIX's answers the new descriptor. */
int
w32_dup2 (int from, int to)
{
  if (from == to)
    return (_get_osfhandle (from) == -1) ? -1 : to;
  if (_dup2 (from, to) != 0)
    return -1;
  if (valid_fd (to))
    fd_flags[to] = w32_append (from) ? FD_FLAG_APPEND : 0;
  return to;
}

int
w32_pipe (int fds[2])
{
  if (_pipe (fds, PIPE_BUFFER_BYTES, O_BINARY | O_NOINHERIT) != 0)
    return -1;
  if (valid_fd (fds[0]))
    fd_flags[fds[0]] = 0;
  if (valid_fd (fds[1]))
    fd_flags[fds[1]] = 0;
  return 0;
}

int
fcntl (int fd, int cmd, ...)
{
  va_list ap;
  int arg, nfd, flags;

  va_start (ap, cmd);
  arg = (cmd == F_GETFD || cmd == F_GETFL) ? 0 : va_arg (ap, int);
  va_end (ap);

  if (!valid_fd (fd) || _get_osfhandle (fd) == -1)
    {
      errno = EBADF;
      return -1;
    }

  switch (cmd)
    {
    case F_DUPFD:
      /* The lowest free descriptor at or above ARG, found by duplicating
	 until one is, as POSIX's F_DUPFD chooses it. */
      {
	int held[W32_MAX_FDS];
	int n, i;

	if (arg < 0 || arg >= W32_MAX_FDS)
	  {
	    errno = EINVAL;
	    return -1;
	  }
	n = 0;
	while ((nfd = w32_dup (fd)) >= 0 && nfd < arg)
	  held[n++] = nfd;
	for (i = 0; i < n; i++)
	  w32_close (held[i]);
	return nfd;
      }
    case F_GETFD:
      return w32_cloexec (fd) ? FD_CLOEXEC : 0;
    case F_SETFD:
      if (arg & FD_CLOEXEC)
	fd_flags[fd] |= FD_FLAG_CLOEXEC;
      else
	fd_flags[fd] &= ~FD_FLAG_CLOEXEC;
      return 0;
    case F_GETFL:
      flags = O_RDWR;
      if (w32_append (fd))
	flags |= O_APPEND;
      return flags;
    case F_SETFL:
      if (arg & O_APPEND)
	fd_flags[fd] |= FD_FLAG_APPEND;
      else
	fd_flags[fd] &= ~FD_FLAG_APPEND;
      return 0;
    default:
      errno = EINVAL;
      return -1;
    }
}

int
getdtablesize (void)
{
  return W32_MAX_FDS;
}

/* Whether the first bytes of the regular file PATH say it is a program:
   a script with an interpreter line, or a PE image. */
static int
starts_as_program (const char *path)
{
  char magic[MAGIC_BYTES];
  int fd, n;

  fd = _open (path, O_RDONLY | O_BINARY | O_NOINHERIT);
  if (fd < 0)
    return 0;
  n = _read (fd, magic, MAGIC_BYTES);
  _close (fd);
  return n == MAGIC_BYTES
    && ((magic[0] == '#' && magic[1] == '!') || (magic[0] == 'M' && magic[1] == 'Z'));
}

/* msvcrt's answer, owned by the process's id, with the owner's bits
   copied to the group's and others', and a program's execute bits. */
static void
fix_stat (const char *path, struct stat *st)
{
  st->st_uid = W32_ID;
  st->st_gid = W32_ID;
  if (path && S_ISREG (st->st_mode) && (st->st_mode & S_IXUSR) == 0
      && starts_as_program (path))
    st->st_mode |= S_IXUSR;
  st->st_mode |= (st->st_mode & S_IRWXU) >> 3;
  st->st_mode |= (st->st_mode & S_IRWXU) >> 6;
}

int
w32_stat (const char *path, struct stat *st)
{
  const char *device;

  device = device_name (path);
  if (device)
    {
      memset (st, 0, sizeof (*st));
      st->st_mode = S_IFCHR | S_IRUSR | S_IWUSR;
      fix_stat (0, st);
      return 0;
    }
  if (stat (path, st) != 0)
    return -1;
  fix_stat (path, st);
  return 0;
}

int
w32_fstat (int fd, struct stat *st)
{
  if (fstat (fd, st) != 0)
    return -1;
  fix_stat (0, st);
  return 0;
}

/* F_OK, R_OK and W_OK as msvcrt answers them; X_OK as w32_stat does. */
int
w32_access (const char *path, int mode)
{
  struct stat st;
  const char *device;

  device = device_name (path);
  if (device)
    return 0;
  if (mode & X_OK)
    {
      if (w32_stat (path, &st) != 0)
	return -1;
      if ((st.st_mode & S_IXUSR) == 0)
	{
	  errno = EACCES;
	  return -1;
	}
      mode &= ~X_OK;
    }
  return _access (path, mode);
}

/* The working directory with forward slashes, as bash's paths are spelled.
   With no buffer, the answer is allocated by the malloc bash links, so bash
   may free it. */
char *
w32_getcwd (char *buf, size_t size)
{
  char here[MAX_PATH * 4];
  size_t len;
  char *p;

  if (_getcwd (here, sizeof (here)) == 0)
    return 0;
  for (p = here; *p; p++)
    if (*p == '\\')
      *p = '/';
  len = strlen (here) + 1;
  if (buf == 0)
    {
      buf = malloc (size > len ? size : len);
      if (buf == 0)
	{
	  errno = ENOMEM;
	  return 0;
	}
    }
  else if (size < len)
    {
      errno = ERANGE;
      return 0;
    }
  memcpy (buf, here, len);
  return buf;
}

int
tcgetattr (int fd, struct termios *t)
{
  (void) fd;
  (void) t;
  errno = ENOTTY;
  return -1;
}

int
tcsetattr (int fd, int when, const struct termios *t)
{
  (void) fd;
  (void) when;
  (void) t;
  errno = ENOTTY;
  return -1;
}

void
w32_init (void)
{
  _setmode (0, O_BINARY);
  _setmode (1, O_BINARY);
  _setmode (2, O_BINARY);
  _fmode = O_BINARY;
}
