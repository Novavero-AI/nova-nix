/* strings.h -- MinGW-w64's, with the BSD copy and clear functions bash
   still calls by name. */

#include_next <strings.h>

#ifndef BASH_W32_STRINGS_H
#define BASH_W32_STRINGS_H

#include <stddef.h>

extern void bcopy (const void *, void *, size_t);
extern void bzero (void *, size_t);

#endif /* BASH_W32_STRINGS_H */
