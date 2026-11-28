/* string.h -- msvcrt's, with strsignal, which signals.c answers with the
   descriptions glibc gives. */

#include_next <string.h>

#ifndef BASH_W32_STRING_H
#define BASH_W32_STRING_H

extern char *strsignal (int);

#endif /* BASH_W32_STRING_H */
