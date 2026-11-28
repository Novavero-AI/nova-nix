/*
 * The end of a process the test is not the parent of, for the build user
 * tests: a process a build left behind, which a build user's kill is to
 * end, is waited for through a descriptor that refers to it, as Linux
 * gives one, rather than looked at once or polled.
 */

#define _GNU_SOURCE

#include <errno.h>

#if defined(__linux__)
#include <poll.h>
#include <sys/syscall.h>
#include <unistd.h>
#endif

/* Wait up to TIMEOUT_MS milliseconds for the process PID to end.  Answers
 * 0 once it has ended or when there is no such process, 1 when it is
 * still running at the timeout, or -1 with errno set, ENOSYS where the
 * system has no descriptor for a process. */
int nova_test_await_exit(int pid, int timeout_ms)
{
#if defined(__linux__) && defined(SYS_pidfd_open)
    struct pollfd watched;
    int ready;
    int fd = (int)syscall(SYS_pidfd_open, pid, 0);
    if (fd < 0)
        return errno == ESRCH ? 0 : -1;
    watched.fd = fd;
    watched.events = POLLIN;
    watched.revents = 0;
    do {
        ready = poll(&watched, 1, timeout_ms);
    } while (ready < 0 && errno == EINTR);
    close(fd);
    if (ready < 0)
        return -1;
    return ready == 0 ? 1 : 0;
#else
    (void)pid;
    (void)timeout_ms;
    errno = ENOSYS;
    return -1;
#endif
}
