/* wchar.h -- MinGW-w64's, with the column widths of wide characters,
   which bash needs to handle multibyte text (misc.c says how they are
   counted). */

#include_next <wchar.h>

#ifndef BASH_W32_WCHAR_H
#define BASH_W32_WCHAR_H

extern int wcwidth (wchar_t);
extern int wcswidth (const wchar_t *, size_t);

#endif /* BASH_W32_WCHAR_H */
