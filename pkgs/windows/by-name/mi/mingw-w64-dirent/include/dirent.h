/*
 * MinGW-w64's dirent.h (mingw-w64-headers/crt/dirent.h:18-66 at v8.0.3),
 * for the functions libdirent.a holds, MinGW-w64's own
 * mingw-w64-crt/misc/dirent.c.  TinyCC's dirent.h declares an older struct
 * dirent, whose d_name points into the DIR, and that file fills an array:
 * compiled against TinyCC's, it writes through a pointer nothing sets.  The
 * wide-character half of the header is left out with the functions it
 * declares, which mingw-w64-crt/misc/wdirent.c defines, and so is
 * __MINGW_NOTHROW, an attribute GCC alone reads.
 */
#ifndef _DIRENT_H_
#define _DIRENT_H_

#include <_mingw.h>
#include <io.h>

#pragma pack(push,_CRT_PACKING)

struct dirent
{
  long d_ino;
  unsigned short d_reclen;
  unsigned short d_namlen;
  char d_name[260];
};

typedef struct
{
  struct _finddata_t dd_dta;
  struct dirent dd_dir;
  intptr_t dd_handle;
  int dd_stat;
  char dd_name[1];
} DIR;

DIR *__cdecl opendir (const char *);
struct dirent *__cdecl readdir (DIR *);
int __cdecl closedir (DIR *);
void __cdecl rewinddir (DIR *);
long __cdecl telldir (DIR *);
void __cdecl seekdir (DIR *, long);

#pragma pack(pop)

#endif
