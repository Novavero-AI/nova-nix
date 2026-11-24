/*
 * MinGW-w64's declarations of struct dirent, DIR and their functions, from
 * the release of the functions libdirent.a holds, which are MinGW-w64's
 * own.  TinyCC's dirent.h declares an older struct dirent, whose d_name
 * points into the DIR, and MinGW-w64's functions fill an array: compiled
 * against TinyCC's, they write through a pointer nothing sets.  The
 * wide-character half of the header is left out with the functions it
 * declares, which libdirent.a does not hold, and so is the nothrow
 * attribute MinGW-w64 marks the declarations with, which GCC alone reads.
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
