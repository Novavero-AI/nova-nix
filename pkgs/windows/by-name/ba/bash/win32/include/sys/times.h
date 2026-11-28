/* sys/times.h -- process times, in clock() ticks, from GetProcessTimes. */

#ifndef BASH_W32_SYS_TIMES_H
#define BASH_W32_SYS_TIMES_H

#include <time.h>

struct tms
{
  clock_t tms_utime;
  clock_t tms_stime;
  clock_t tms_cutime;
  clock_t tms_cstime;
};

extern clock_t times (struct tms *);

#endif /* BASH_W32_SYS_TIMES_H */
