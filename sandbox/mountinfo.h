/*
 * The mount points at or beneath a directory, read from the text of
 * /proc/self/mountinfo without IO: the fifth field of each line, its
 * fields split at single spaces, with the kernel's three-digit octal
 * escapes undone (a space, a tab, a newline and a backslash in a path).
 * Nix.Builder.Sandbox.Spawn's mountPointsWithin reads the text the same
 * way, which the test suite checks.  Portable C, so the test suite links
 * it too.
 */

#ifndef NOVA_NIX_SANDBOX_MOUNTINFO_H
#define NOVA_NIX_SANDBOX_MOUNTINFO_H

#include <stddef.h>

struct mountinfo_cursor {
    const char *next;
    const char *end;
};

/* What looking for the next mount point came to. */
enum mountinfo_read {
    MOUNTINFO_FOUND,
    MOUNTINFO_END,
    /* A mount point at DIR or beneath it, or one whose first bytes cannot
     * say, longer than the space given for it. */
    MOUNTINFO_TOO_LONG
};

/* Find the next mount point at DIR or beneath it, DIR being DIR_LENGTH
 * bytes, and write it, unescaped, to the CAPACITY bytes at POINT, with its
 * length in *POINT_LENGTH.  The point is bytes, not a C string: an escape
 * can make a NUL. */
enum mountinfo_read mountinfo_next_within(struct mountinfo_cursor *cursor, const char *dir, size_t dir_length,
                                          char *point, size_t capacity, size_t *point_length);

#endif
