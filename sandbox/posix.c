/*
 * The channels, start and end of a run that nova-nix-sandbox's POSIX
 * backends share (posix.h).
 */

#define _POSIX_C_SOURCE 200809L

#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <signal.h>
#include <stdlib.h>
#include <string.h>
#include <sys/resource.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

#include "backend.h"
#include "posix.h"

/* The most descriptors one pass over a descriptor listing closes; a
 * listing that names more is read again, since closing changes it. */
#define CLOSE_BATCH 1024

/* How far up descriptors are closed where neither a listing nor a finite
 * limit says where they end. */
#define CLOSE_FALLBACK_TOP 1024

/* The status channel's descriptor, or the descriptor posix_report_to
 * names in its place.  It is the process's, so it lives at file scope:
 * every failure of the setup is reported on it, a child's before its exec
 * included. */
static int status_fd = STDOUT_FILENO;

int posix_status_fd(void)
{
    return status_fd;
}

void posix_close_status(void)
{
    close(status_fd);
    status_fd = -1;
}

void posix_report_to(int fd)
{
    status_fd = fd;
}

void backend_write_status(const char *bytes, size_t length)
{
    while (length > 0) {
        ssize_t written = write(status_fd, bytes, length);
        if (written < 0) {
            if (errno == EINTR)
                continue;
            return;
        }
        bytes += written;
        length -= (size_t)written;
    }
}

int backend_read_input(char *bytes, size_t length)
{
    size_t got = 0;
    while (got < length) {
        ssize_t read_now = read(STDIN_FILENO, bytes + got, length - got);
        if (read_now < 0) {
            if (errno == EINTR)
                continue;
            return errno;
        }
        if (read_now == 0)
            return EPIPE;
        got += (size_t)read_now;
    }
    return 0;
}

int backend_prepare_run(void)
{
    int error;
    /* The status pipe moves to a descriptor the builder's exec closes, and
     * standard output becomes the log, as upstream's child gets standard
     * error on both (commonChildInit in child.cc). */
    int moved = fcntl(STDOUT_FILENO, F_DUPFD_CLOEXEC, 3);
    if (moved < 0)
        return -1;
    status_fd = moved;
    if (dup2(STDERR_FILENO, STDOUT_FILENO) < 0)
        status_fail("stdout", errno, NO_INDEX);

    if ((error = posix_reset_sigchld()) != 0)
        status_fail("sigchld", error, NO_INDEX);
    return 0;
}

int posix_reset_sigchld(void)
{
    struct sigaction action;
    memset(&action, 0, sizeof action);
    sigemptyset(&action.sa_mask);
    action.sa_handler = SIG_DFL;
    return sigaction(SIGCHLD, &action, NULL) < 0 ? errno : 0;
}

int posix_reap(pid_t child, int *wait_status)
{
    for (;;) {
        if (waitpid(child, wait_status, 0) == child)
            return 0;
        if (errno != EINTR)
            return errno;
    }
}

int posix_relay(int wait_status)
{
    if (WIFEXITED(wait_status))
        return WEXITSTATUS(wait_status);
    if (WIFSIGNALED(wait_status)) {
        int signal_number = WTERMSIG(wait_status);
        sigset_t just_this;
        struct rlimit core;
        if (getrlimit(RLIMIT_CORE, &core) == 0) {
            core.rlim_cur = 0;
            setrlimit(RLIMIT_CORE, &core);
        }
        signal(signal_number, SIG_DFL);
        sigemptyset(&just_this);
        sigaddset(&just_this, signal_number);
        sigprocmask(SIG_UNBLOCK, &just_this, NULL);
        raise(signal_number);
        return 128 + signal_number;
    }
    return EXIT_FAILURE;
}

void posix_close_descriptors(const char *listing, int keep)
{
    DIR *dir = opendir(listing);
    if (dir != NULL) {
        int own = dirfd(dir);
        int fds[CLOSE_BATCH];
        size_t count = 0;
        size_t i;
        struct dirent *entry;
        int more = 1;
        while (more) {
            count = 0;
            more = 0;
            rewinddir(dir);
            while ((entry = readdir(dir)) != NULL) {
                long fd;
                char *rest = NULL;
                if (entry->d_name[0] < '0' || entry->d_name[0] > '9')
                    continue;
                fd = strtol(entry->d_name, &rest, 10);
                if (rest == NULL || *rest != '\0' || fd < 3 || fd == own || fd == keep)
                    continue;
                if (count == sizeof fds / sizeof fds[0]) {
                    more = 1;
                    break;
                }
                fds[count++] = (int)fd;
            }
            for (i = 0; i < count; i++)
                close(fds[i]);
        }
        closedir(dir);
        return;
    }
    {
        struct rlimit limit;
        int fd;
        int top = CLOSE_FALLBACK_TOP;
        if (getrlimit(RLIMIT_NOFILE, &limit) == 0 && limit.rlim_cur != RLIM_INFINITY && limit.rlim_cur < (rlim_t)INT_MAX)
            top = (int)limit.rlim_cur;
        for (fd = 3; fd < top; fd++)
            if (fd != keep)
                close(fd);
    }
}
