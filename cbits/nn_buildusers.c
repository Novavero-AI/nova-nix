/* glibc declares setgroups, syscall, pipe2 and strsignal only past strict
 * C99. */
#define _GNU_SOURCE

#include "nn_buildusers.h"

#ifndef _WIN32

#include <errno.h>
#include <fcntl.h>
#include <grp.h>
#include <limits.h>
#include <pthread.h>
#include <signal.h>
#include <stdlib.h>
#include <string.h>
#include <sys/resource.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

#if defined(__linux__) || defined(__APPLE__)
#include <sys/syscall.h>
#endif

/* Both calls fork nova-nix's own process, which the GHC runtime runs
 * threaded, so each child makes only async-signal-safe calls before it
 * executes or exits: whatever another thread held at the fork, a lock of
 * the allocator's or of the name service's among them, stays held in the
 * child.  The process library can start a program as another user only
 * by looking that user up in its child, with getpwuid_r and initgroups;
 * here the caller looks the groups up beforehand and the child only sets
 * them.  Every signal is blocked across the fork, so none runs a handler
 * of the runtime's in the child before the child has put each back to
 * its default. */

/* How far up descriptors are closed where the descriptor limit is not
 * finite. */
#define CLOSE_FALLBACK_TOP 1024

/* What a child that failed to start its program tells its parent. */
struct spawn_failure {
  int step;
  int error;
};

/* The descriptor limit, read before the fork: the top of the range a
 * child closes. */
static int descriptor_top(void) {
  struct rlimit limit;
  if (getrlimit(RLIMIT_NOFILE, &limit) == 0 && limit.rlim_cur != RLIM_INFINITY &&
      limit.rlim_cur < (rlim_t)INT_MAX) {
    return (int)limit.rlim_cur;
  }
  return CLOSE_FALLBACK_TOP;
}

/* Close every descriptor from 3 up to TOP but KEEP, as upstream's child
 * closes them before it executes the builder: a build user is not to
 * hold a descriptor of the process that started it.  On Linux a range is
 * closed in one call where the kernel has it, whatever the limit. */
static void close_descriptors(int keep, int top) {
  int fd;
#if defined(__linux__) && defined(SYS_close_range)
  if ((keep <= 3 || syscall(SYS_close_range, 3U, (unsigned int)keep - 1U, 0U) == 0) &&
      syscall(SYS_close_range, (unsigned int)keep + 1U, ~0U, 0U) == 0) {
    return;
  }
#endif
  for (fd = 3; fd < top; fd++) {
    if (fd != keep) {
      close(fd);
    }
  }
}

/* Put every signal a handler catches back to its default, and SIGPIPE,
 * which the GHC runtime ignores and an exec would leave ignored, so the
 * program starts with the dispositions a program expects. */
static void default_signals(void) {
  struct sigaction found;
  struct sigaction default_action;
  int signal_number;
  default_action.sa_handler = SIG_DFL;
  default_action.sa_flags = 0;
  sigemptyset(&default_action.sa_mask);
  for (signal_number = 1; signal_number < NSIG; signal_number++) {
    if (signal_number == SIGKILL || signal_number == SIGSTOP) {
      continue;
    }
    if (sigaction(signal_number, NULL, &found) == 0 &&
        (signal_number == SIGPIPE || (found.sa_handler != SIG_DFL && found.sa_handler != SIG_IGN))) {
      sigaction(signal_number, &default_action, NULL);
    }
  }
}

/* Tell the parent which step failed, then end. */
static void child_fail(int report, int step) {
  struct spawn_failure failure;
  ssize_t written;
  failure.step = step;
  failure.error = errno;
  do {
    written = write(report, &failure, sizeof failure);
  } while (written < 0 && errno == EINTR);
  _exit(127);
}

static void child_run(const char *program, char *const argv[], char *const envp[], const char *cwd, int input_fd,
                      int output_fd, uid_t uid, gid_t gid, int group_count, const gid_t *groups, int report,
                      int top) {
  sigset_t none;
  struct rlimit core;

  /* No controlling terminal, and no terminal signals, as upstream's child
   * makes a session of its own. */
  if (setsid() < 0) {
    child_fail(report, NN_SPAWN_SESSION);
  }
  if (dup2(input_fd, STDIN_FILENO) < 0) {
    child_fail(report, NN_SPAWN_INPUT);
  }
  if (dup2(output_fd, STDOUT_FILENO) < 0 || dup2(output_fd, STDERR_FILENO) < 0) {
    child_fail(report, NN_SPAWN_OUTPUT);
  }
  close_descriptors(report, top);

  /* No core dumps, as upstream's child sets; the hard limit is left as it
   * is. */
  if (getrlimit(RLIMIT_CORE, &core) == 0) {
    core.rlim_cur = 0;
    setrlimit(RLIMIT_CORE, &core);
  }

  if (chdir(cwd) < 0) {
    child_fail(report, NN_SPAWN_CHDIR);
  }

  /* The supplementary groups first, then the group, then the user, which
   * gives up the right to change the other two; setuid as root sets the
   * real, effective and saved ids, each checked as upstream checks them. */
  if (setgroups(group_count, groups) < 0) {
    child_fail(report, NN_SPAWN_GROUPS);
  }
  if (setgid(gid) < 0 || getgid() != gid || getegid() != gid) {
    child_fail(report, NN_SPAWN_GID);
  }
  if (setuid(uid) < 0 || getuid() != uid || geteuid() != uid) {
    child_fail(report, NN_SPAWN_UID);
  }

  default_signals();
  sigemptyset(&none);
  sigprocmask(SIG_SETMASK, &none, NULL);
  execve(program, argv, envp);
  child_fail(report, NN_SPAWN_EXEC);
}

/* A pipe whose ends close at an exec, made so in one call where the
 * system has one, so a fork in another thread cannot inherit an end. */
static int report_pipe(int ends[2]) {
#if defined(__linux__)
  return pipe2(ends, O_CLOEXEC);
#else
  if (pipe(ends) < 0) {
    return -1;
  }
  if (fcntl(ends[0], F_SETFD, FD_CLOEXEC) < 0 || fcntl(ends[1], F_SETFD, FD_CLOEXEC) < 0) {
    int error = errno;
    close(ends[0]);
    close(ends[1]);
    errno = error;
    return -1;
  }
  return 0;
#endif
}

int nn_spawn_as_user(const char *program, char *const argv[], char *const envp[], const char *cwd, int input_fd,
                     int output_fd, unsigned int uid, unsigned int gid, int group_count, const unsigned int *groups,
                     int *pid, int *step) {
  int report[2];
  gid_t *group_ids = NULL;
  int top = descriptor_top();
  int i;
  pid_t child;
  struct spawn_failure failure;
  ssize_t got;
  int wait_status;
  sigset_t all;
  sigset_t original;

  if (uid == 0) {
    *step = NN_SPAWN_UID;
    return EINVAL;
  }

  /* The groups as setgroups takes them, copied before the fork. */
  if (group_count > 0) {
    group_ids = malloc((size_t)group_count * sizeof *group_ids);
    if (group_ids == NULL) {
      *step = NN_SPAWN_FORK;
      return ENOMEM;
    }
    for (i = 0; i < group_count; i++) {
      group_ids[i] = (gid_t)groups[i];
    }
  }

  /* The report pipe closes at the exec, so a read that finds its end
   * means the program started. */
  if (report_pipe(report) < 0) {
    int error = errno;
    free(group_ids);
    *step = NN_SPAWN_PIPE;
    return error;
  }

  sigfillset(&all);
  pthread_sigmask(SIG_SETMASK, &all, &original);
  child = fork();
  if (child == 0) {
    close(report[0]);
    child_run(program, argv, envp, cwd, input_fd, output_fd, (uid_t)uid, (gid_t)gid, group_count, group_ids,
              report[1], top);
  }
  pthread_sigmask(SIG_SETMASK, &original, NULL);
  free(group_ids);
  if (child < 0) {
    int error = errno;
    close(report[0]);
    close(report[1]);
    *step = NN_SPAWN_FORK;
    return error;
  }

  close(report[1]);
  do {
    got = read(report[0], &failure, sizeof failure);
  } while (got < 0 && errno == EINTR);
  close(report[0]);
  if (got == 0) {
    *pid = (int)child;
    return 0;
  }

  while (waitpid(child, &wait_status, 0) < 0 && errno == EINTR) {
  }
  if (got != (ssize_t)sizeof failure) {
    *step = NN_SPAWN_FORK;
    return EIO;
  }
  *step = failure.step;
  return failure.error;
}

/* Send SIGKILL to every process the caller may signal but the caller
 * itself.  Linux's kill(-1) leaves the caller out; macOS's libc kill
 * takes it in, so there the call passes the argument that leaves it out,
 * as upstream's does, through the system call the SDK marks deprecated
 * and offers no other way to. */
static int kill_all_others(void) {
#if defined(__APPLE__)
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wdeprecated-declarations"
  return syscall(SYS_kill, -1, SIGKILL, 0);
#pragma clang diagnostic pop
#else
  return kill(-1, SIGKILL);
#endif
}

int nn_kill_user(unsigned int uid, int *status) {
  pid_t child;
  int wait_status;
  pid_t waited;

  if (uid == 0) {
    return EINVAL;
  }
  child = fork();
  if (child < 0) {
    return errno;
  }
  if (child == 0) {
    if (setuid((uid_t)uid) < 0) {
      _exit(1);
    }
    /* A user with no other process is no failure. */
    while (kill_all_others() < 0) {
      if (errno == ESRCH || errno == EPERM) {
        _exit(0);
      }
      if (errno != EINTR) {
        _exit(1);
      }
    }
    _exit(0);
  }

  do {
    waited = waitpid(child, &wait_status, 0);
  } while (waited < 0 && errno == EINTR);
  if (waited < 0) {
    return errno;
  }
  if (WIFEXITED(wait_status)) {
    *status = WEXITSTATUS(wait_status);
  } else if (WIFSIGNALED(wait_status)) {
    *status = 256 + WTERMSIG(wait_status);
  } else {
    *status = 1;
  }
  return 0;
}

int nn_await_exit(int pid) {
  siginfo_t info;
  int answer;
  do {
    answer = waitid(P_PID, (id_t)pid, &info, WEXITED | WNOWAIT);
  } while (answer < 0 && errno == EINTR);
  return answer < 0 ? errno : 0;
}

const char *nn_signal_text(int signal) { return strsignal(signal); }

#else /* _WIN32 */

#include <errno.h>

int nn_spawn_as_user(const char *program, char *const argv[], char *const envp[], const char *cwd, int input_fd,
                     int output_fd, unsigned int uid, unsigned int gid, int group_count, const unsigned int *groups,
                     int *pid, int *step) {
  (void)program;
  (void)argv;
  (void)envp;
  (void)cwd;
  (void)input_fd;
  (void)output_fd;
  (void)uid;
  (void)gid;
  (void)group_count;
  (void)groups;
  (void)pid;
  *step = NN_SPAWN_FORK;
  return ENOSYS;
}

int nn_kill_user(unsigned int uid, int *status) {
  (void)uid;
  *status = 1;
  return ENOSYS;
}

int nn_await_exit(int pid) {
  (void)pid;
  return ENOSYS;
}

const char *nn_signal_text(int signal) {
  (void)signal;
  return "";
}

#endif
