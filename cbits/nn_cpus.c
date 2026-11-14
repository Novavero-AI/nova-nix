#include "nn_cpus.h"

#ifndef _WIN32

#include <unistd.h>

long nn_online_processors(void) { return sysconf(_SC_NPROCESSORS_ONLN); }

#else /* _WIN32 */

/* Windows has no sysconf. */
long nn_online_processors(void) { return -1; }

#endif
