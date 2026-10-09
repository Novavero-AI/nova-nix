#include "nn_darwinfs.h"

#ifdef __APPLE__

#include <unistd.h>

int nn_path_case_sensitive(const char *path) {
  long answer;

  /* pathconf returns the queried value, not a status: -1 is the error
   * return (errno set), 0 means the volume folds, and a positive value
   * means it does not. */
  answer = pathconf(path, _PC_CASE_SENSITIVE);
  if (answer < 0) {
    return -1;
  }
  return answer > 0 ? 1 : 0;
}

#else /* !__APPLE__ */

/* _PC_CASE_SENSITIVE is a Darwin extension: Linux has no pathconf name for
 * case sensitivity (its filesystems are case-sensitive by convention, the
 * same assumption upstream Nix makes there), and Windows decides per
 * directory through nn_winfs.h rather than per volume. */
int nn_path_case_sensitive(const char *path) {
  (void)path;
  return -1;
}

#endif
