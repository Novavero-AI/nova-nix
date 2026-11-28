/* fcntl.h -- msvcrt's, with fcntl and the POSIX names of /dev.  open is
   renamed to w32_open, which opens every file in binary mode and maps
   /dev/null, /dev/tty and /dev/fd/N (io.c says how). */

#include_next <fcntl.h>

#ifndef BASH_W32_FCNTL_H
#define BASH_W32_FCNTL_H

#include <io.h>
#include <w32compat.h>

#define F_DUPFD		0
#define F_GETFD		1
#define F_SETFD		2
#define F_GETFL		3
#define F_SETFL		4

#define FD_CLOEXEC	1

/* A bit msvcrt's open does not use.  No descriptor here is non-blocking;
   w32_open drops the bit, and F_SETFL accepts and ignores it. */
#define O_NONBLOCK	0x100000
#define O_NDELAY	O_NONBLOCK

extern int fcntl (int, int, ...);

#define open(...)	w32_open (__VA_ARGS__)

#endif /* BASH_W32_FCNTL_H */
