/*
 * A key for the Linux sandbox tests to look for: planted in a session
 * keyring of the calling thread's own, which a process it then starts
 * inherits, and viewable only by a process that possesses it, so a
 * builder sees it in /proc/keys exactly when it still possesses that
 * keyring.
 */

#define _GNU_SOURCE

#include <errno.h>

#if defined(__linux__)
#include <linux/keyctl.h>
#include <sys/syscall.h>
#include <unistd.h>

/* Every permission for the key's possessor, and none for anyone else
 * (KEY_POS_ALL in the kernel's include/linux/key.h). */
#define POSSESSOR_ALL 0x3f000000UL

/* The payload the key holds, which no test reads. */
static const char payload[] = "nova-nix test";
#endif

/* Join a fresh session keyring and add a user key named DESCRIPTION to it.
 * Answers the key's serial, or -1 with errno set, ENOSYS off Linux.  Only
 * the calling OS thread joins: a keyring is part of a thread's
 * credentials. */
long nova_test_plant_possessed_key(const char *description)
{
#if defined(__linux__)
    long serial;
    if (syscall(SYS_keyctl, KEYCTL_JOIN_SESSION_KEYRING, (char *)NULL) < 0)
        return -1;
    serial = syscall(SYS_add_key, "user", description, payload, sizeof payload - 1, KEY_SPEC_SESSION_KEYRING);
    if (serial < 0)
        return -1;
    if (syscall(SYS_keyctl, KEYCTL_SETPERM, serial, POSSESSOR_ALL) < 0)
        return -1;
    return serial;
#else
    (void)description;
    errno = ENOSYS;
    return -1;
#endif
}
