/* sys/types.h -- MinGW-w64's, with the id types POSIX adds.  Windows has
   no user or group ids; w32compat.h says what stands in for them. */

#include_next <sys/types.h>

#ifndef BASH_W32_SYS_TYPES_H
#define BASH_W32_SYS_TYPES_H
typedef unsigned int uid_t;
typedef unsigned int gid_t;
#endif
