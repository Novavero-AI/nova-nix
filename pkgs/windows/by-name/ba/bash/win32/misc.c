/* misc.c -- the rest of what bash calls by its POSIX or BSD name and
   MinGW-w64 on msvcrt lacks. */

#include <errno.h>
#include <io.h>
#include <string.h>
/* Leaves out winsock, whose gethostname takes an int length. */
#define WIN32_LEAN_AND_MEAN
#include <windows.h>

#include <strings.h>
#include <sys/stat.h>
#include <unistd.h>
#include <wchar.h>

int
gethostname (char *name, size_t len)
{
  DWORD size;

  size = (DWORD) len;
  if (!GetComputerNameExA (ComputerNameDnsHostname, name, &size))
    {
      errno = (GetLastError () == ERROR_MORE_DATA) ? ENAMETOOLONG : EINVAL;
      return -1;
    }
  return 0;
}

/* A console has no device name of its own; /dev/tty is what the shell's
   open makes of one. */
char *
ttyname (int fd)
{
  if (!isatty (fd))
    {
      errno = ENOTTY;
      return 0;
    }
  return (char *) "/dev/tty";
}

/* Windows keeps no permission bits; a file without the owner's write bit
   becomes read-only, and one with it stops being so. */
int
fchmod (int fd, mode_t mode)
{
  FILE_BASIC_INFO info;
  HANDLE h;

  h = (HANDLE) _get_osfhandle (fd);
  if (h == INVALID_HANDLE_VALUE)
    {
      errno = EBADF;
      return -1;
    }
  if (!GetFileInformationByHandleEx (h, FileBasicInfo, &info, sizeof (info)))
    {
      errno = EACCES;
      return -1;
    }
  if (mode & S_IWUSR)
    info.FileAttributes &= ~FILE_ATTRIBUTE_READONLY;
  else
    info.FileAttributes |= FILE_ATTRIBUTE_READONLY;
  if (info.FileAttributes == 0)
    info.FileAttributes = FILE_ATTRIBUTE_NORMAL;
  if (!SetFileInformationByHandle (h, FileBasicInfo, &info, sizeof (info)))
    {
      errno = EACCES;
      return -1;
    }
  return 0;
}

int
mkfifo (const char *path, mode_t mode)
{
  (void) path;
  (void) mode;
  errno = ENOSYS;
  return -1;
}

void
bcopy (const void *from, void *to, size_t n)
{
  memmove (to, from, n);
}

void
bzero (void *p, size_t n)
{
  memset (p, 0, n);
}

/* The columns a wide character takes: none for NUL and the combining
   marks, two for the East Asian wide and fullwidth blocks, -1 for the
   other control characters, and one for the rest.  wchar_t is UTF-16 on
   Windows, so each half of a surrogate pair counts as one column. */
struct range
{
  unsigned int first, last;
};

static const struct range combining[] = {
  { 0x0300, 0x036F }, { 0x0483, 0x0489 }, { 0x0591, 0x05BD },
  { 0x0610, 0x061A }, { 0x064B, 0x065F }, { 0x0E31, 0x0E31 },
  { 0x0E34, 0x0E3A }, { 0x0E47, 0x0E4E }, { 0x1AB0, 0x1AFF },
  { 0x1DC0, 0x1DFF }, { 0x200B, 0x200F }, { 0x20D0, 0x20FF },
  { 0xFE00, 0xFE0F }, { 0xFE20, 0xFE2F }, { 0xFEFF, 0xFEFF },
};

static const struct range wide[] = {
  { 0x1100, 0x115F }, { 0x2E80, 0x303E }, { 0x3041, 0x33FF },
  { 0x3400, 0x4DBF }, { 0x4E00, 0x9FFF }, { 0xA000, 0xA4CF },
  { 0xAC00, 0xD7A3 }, { 0xF900, 0xFAFF }, { 0xFE30, 0xFE4F },
  { 0xFF00, 0xFF60 }, { 0xFFE0, 0xFFE6 },
};

static int
in_ranges (unsigned int c, const struct range *r, size_t n)
{
  size_t i;

  for (i = 0; i < n; i++)
    if (c >= r[i].first && c <= r[i].last)
      return 1;
  return 0;
}

int
wcwidth (wchar_t wc)
{
  unsigned int c;

  c = (unsigned int) wc;
  if (c == 0)
    return 0;
  if (c < 0x20 || (c >= 0x7F && c < 0xA0))
    return -1;
  if (in_ranges (c, combining, sizeof (combining) / sizeof (combining[0])))
    return 0;
  if (in_ranges (c, wide, sizeof (wide) / sizeof (wide[0])))
    return 2;
  return 1;
}

int
wcswidth (const wchar_t *s, size_t n)
{
  int total, w;

  total = 0;
  for (; n > 0 && *s; s++, n--)
    {
      w = wcwidth (*s);
      if (w < 0)
	return -1;
      total += w;
    }
  return total;
}
