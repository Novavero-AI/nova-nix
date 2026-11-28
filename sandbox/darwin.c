/*
 * nova-nix-sandbox's macOS backend (backend.h): runs the builder under the
 * sandbox profile its plan carries, which macOS's sandbox then holds the
 * builder and every process it starts to.
 *
 * The profile is applied with sandbox_init_with_parameters in a child of
 * the helper, just before that child executes the builder, as upstream
 * applies its profile in the child it forks for the builder
 * (DarwinDerivationBuilder::setUser in darwin-derivation-builder.cc at Nix
 * 2.36.0, called from UnixDerivationBuilderImpl::runChild in
 * unix-derivation-builder.cc).  nova-nix renders the profile
 * (Nix.Builder.Sandbox); the plan carries its text and the values of the
 * parameters it reads, to which the helper adds the build's mark.
 *
 * The helper stays the builder's parent: it waits for the builder, kills
 * the builder's process group when the lifeline closes or a terminating
 * signal arrives, and ends as the builder ended, unless it cannot learn
 * how the builder ended, finish the sweep below, see the build's output
 * end or watch the log, each of which fails the build.  The builder leads
 * a session of its own, and once it has ended, or the build is to end,
 * every process still in its group is killed before the builder is
 * reaped, as upstream kills the group before it waits.
 *
 * macOS has no PID namespace, and a process of the build can leave the
 * group by a session of its own.  A builder runs as the invoking user,
 * whose other processes must live, or, when the plan names one, as a build
 * user, whose every process nova-nix kills once the build ends, as
 * upstream kills its build user's.  Either way, before it ends the helper
 * stops and then kills every process of the builder's user that carries
 * the build's mark (sweep_build): a Mach service name, random and made by
 * the helper for this build alone, that the build's profile allows, beside
 * a second that it refuses.  It does so whichever way the build ends: the
 * builder's own end, the lifeline's, or any terminating signal the helper
 * can catch; only SIGKILL, which no process can catch, ends the helper
 * before it has.
 *
 * The builder writes its output to a pipe the helper holds the reading
 * end of and copies to its own standard error, the build log.  Upstream
 * reads the builder's pipe itself until it ends, which killing the build
 * user's processes makes sure of; here a process the sweep could not end
 * may still hold the builder's pipe, and nova-nix's log ends when the
 * helper does all the same.  The helper never blocks on the log: what the
 * log does not take at once is held, and the helper reads no more of the
 * builder's pipe until it has, waiting for the log through the queue that
 * also brings it signals and the lifeline's end.  Once the build's
 * processes have ended it copies at most LOG_DRAIN_BYTES_MAX more, and it
 * waits for the log to take the rest only while the build is not to end:
 * a reader that has gone away closes the log, which fails the write.
 *
 * The probe applies a profile that allows everything to the probe process
 * itself, which shows that this system's sandbox takes a profile from this
 * process; a process already in a sandbox, for one, is refused another.
 */

#include <errno.h>
#include <fcntl.h>
#include <grp.h>
#include <libproc.h>
#include <signal.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/event.h>
#include <sys/proc.h>
#include <sys/resource.h>
#include <sys/sysctl.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>

#include "backend.h"
#include "plan.h"
#include "posix.h"

/* libSystem's entry point that applies a profile with parameters to the
 * calling process.  It is exported but declared in no SDK header, whose
 * sandbox.h declares only the deprecated sandbox_init; upstream declares it
 * the same way (in darwin-derivation-builder.cc).  Answers 0, or -1 with
 * *ERRORBUF naming what the profile's interpreter refused.  FLAGS 0 takes
 * PROFILE as a profile's text, and PARAMETERS is each name followed by its
 * value, the whole NULL-terminated. */
int sandbox_init_with_parameters(const char *profile, uint64_t flags, const char *const parameters[],
                                 char **errorbuf);

/* libSystem's check of what a process's sandbox allows, exported but
 * declared in no SDK header.  Asked of an operation on a Mach service's
 * name, it answers 0 when the process may look the name up, as a process
 * in no sandbox may any, and 1 when its sandbox refuses it, as it also
 * answers for a zombie and for a PID no process has; any other answer is
 * an error.  SANDBOX_CHECK_NO_REPORT keeps a refusal out of the system
 * log. */
int sandbox_check(pid_t pid, const char *operation, int type, ...);
extern const int SANDBOX_CHECK_NO_REPORT;

/* sandbox_check's filter type for a Mach service's global name. */
#define SANDBOX_FILTER_GLOBAL_NAME 2

/* The parameters through which the profile reads the build's mark: the
 * name it allows (the token) and the name it refuses (the decoy). */
#define TOKEN_PARAMETER "_NOVA_NIX_BUILD_TOKEN"
#define DECOY_PARAMETER "_NOVA_NIX_BUILD_DECOY"
#define MARK_PARAMETER_COUNT 2

/* A mark's name: this prefix, then random bytes in hex. */
#define MARK_PREFIX "org.nova-nix.build."
#define MARK_RANDOM_BYTES 16
#define MARK_LENGTH (sizeof MARK_PREFIX - 1 + 2 * MARK_RANDOM_BYTES)

/* How many listings of the user's processes the sweep takes in all, how
 * many times one listing is tried while processes start faster than it
 * can be sized, and how long the sweep waits, each time it kills the
 * processes it stopped, for them to end. */
#define SWEEP_PASSES_MAX 64
#define LISTING_ATTEMPTS_MAX 8
#define SWEEP_EXIT_SECONDS 10

/* How many bytes of the builder's output the helper holds at once, and
 * how many it copies, at most, once the build's processes have ended. */
#define LOG_CHUNK_BYTES 65536
#define LOG_DRAIN_BYTES_MAX (64 * 1024 * 1024)

/* How long a message of the helper's to the build log may be. */
#define LOG_MESSAGE_BYTES 512

/* The most the builder's report of a failed setup step holds: one status
 * record, with room to spare. */
#define REPORT_BYTES_MAX 256

/* The backend this file is. */
const enum plan_backend backend_plan = PLAN_DARWIN;

/* The open descriptors of the process reading it. */
static const char descriptor_listing[] = "/dev/fd";

/* What the probe applies: a profile that allows everything. */
static const char probe_profile[] = "(version 1)\n(allow default)\n";

/* The signals the helper answers by ending the build, every one a process
 * can catch whose default action ends it, so that none sent to the helper
 * ends it while the build runs; and SIGCHLD, which says the builder may
 * have ended. */
static const int handled_signals[] = {SIGHUP,  SIGINT,  SIGQUIT, SIGILL,  SIGTRAP,   SIGABRT, SIGEMT,
                                      SIGFPE,  SIGBUS,  SIGSEGV, SIGSYS,  SIGPIPE,   SIGALRM, SIGTERM,
                                      SIGXCPU, SIGXFSZ, SIGVTALRM, SIGPROF, SIGUSR1, SIGUSR2, SIGCHLD};

#define HANDLED_SIGNAL_COUNT (sizeof handled_signals / sizeof handled_signals[0])

/* The lifeline's read, which only needs to see it end. */
#define LIFELINE_READ_BYTES 64

/* The names that mark one build's processes. */
struct build_mark {
    char token[MARK_LENGTH + 1];
    char decoy[MARK_LENGTH + 1];
};

/* ------------------------------------------------------------------------
 * Time
 * ------------------------------------------------------------------------ */

/* SECONDS from now on the monotonic clock, in *DEADLINE.  Answers 0, or -1
 * with errno set when the clock cannot be read. */
static int deadline_in(int seconds, struct timespec *deadline)
{
    if (clock_gettime(CLOCK_MONOTONIC, deadline) < 0)
        return -1;
    deadline->tv_sec += seconds;
    return 0;
}

/* The time left until DEADLINE on the monotonic clock in *LEFT.  Answers
 * 1 while some is left, 0 once none is, and -1 with errno set when the
 * clock cannot be read. */
static int time_left(const struct timespec *deadline, struct timespec *left)
{
    struct timespec now;
    if (clock_gettime(CLOCK_MONOTONIC, &now) < 0)
        return -1;
    if (now.tv_sec > deadline->tv_sec || (now.tv_sec == deadline->tv_sec && now.tv_nsec >= deadline->tv_nsec))
        return 0;
    left->tv_sec = deadline->tv_sec - now.tv_sec;
    left->tv_nsec = deadline->tv_nsec - now.tv_nsec;
    if (left->tv_nsec < 0) {
        left->tv_sec -= 1;
        left->tv_nsec += 1000000000L;
    }
    return 1;
}

/* ------------------------------------------------------------------------
 * The build log
 * ------------------------------------------------------------------------ */

/* What the helper watches while the build runs and after it: the queue,
 * the lifeline, and whether the build is to end, which a terminating
 * signal or the lifeline's end says. */
struct watch {
    int queue;
    /* The lifeline, or -1 once it has ended. */
    int lifeline_fd;
    int ending;
};

/* The builder's output on its way to the build log, the helper's standard
 * error, which the helper never waits to write to while the build runs. */
struct log_copy {
    /* The reading end of the builder's pipe, or -1 once it has ended. */
    int from;
    /* What was read from it and the log has not taken yet. */
    char held[LOG_CHUNK_BYTES];
    size_t start;
    size_t end;
    /* Whether the log refused a write, as when nova-nix stopped reading
     * it: whatever is read after that is dropped. */
    int gone;
    /* The errno value of a failure to watch the log, or 0. */
    int unwatched;
    /* The errno value of a failure that would leave the builder's pipe
     * unread, or the helper woken without end, which ends the build, or
     * 0. */
    int stalled;
};

/* Write what is held to the log, as far as it takes it without waiting.
 * Answers 1 once nothing is held, and 0 while some is. */
static int log_flush(struct log_copy *copy)
{
    while (copy->start < copy->end) {
        ssize_t wrote = write(STDERR_FILENO, copy->held + copy->start, copy->end - copy->start);
        if (wrote > 0) {
            copy->start += (size_t)wrote;
        } else if (wrote < 0 && errno == EINTR) {
            continue;
        } else if (wrote < 0 && errno == EAGAIN) {
            return 0;
        } else {
            copy->gone = 1;
            copy->start = copy->end;
        }
    }
    copy->start = copy->end = 0;
    return 1;
}

/* Read once from the builder's pipe into what is held, which is empty,
 * dropping it when the log has gone.  Answers how many bytes were read;
 * the pipe's end, or a failed read, closes it. */
static size_t log_fill(struct log_copy *copy)
{
    ssize_t got;
    if (copy->from < 0)
        return 0;
    got = read(copy->from, copy->held, sizeof copy->held);
    if (got > 0) {
        copy->start = 0;
        copy->end = copy->gone ? 0 : (size_t)got;
        return (size_t)got;
    }
    if (got < 0 && (errno == EINTR || errno == EAGAIN))
        return 0;
    close(copy->from);
    copy->from = -1;
    return 0;
}

/* Apply one change to QUEUE, on its own, so that its failure is known.
 * Answers 0, or an errno value. */
static int queue_change(int queue, int fd, int16_t filter, uint16_t flags)
{
    struct kevent change;
    EV_SET(&change, (uintptr_t)fd, filter, flags, 0, 0, NULL);
    return kevent(queue, &change, 1, NULL, 0, NULL) < 0 ? errno : 0;
}

/* Switch the queue between reading the builder's pipe and waiting for the
 * log to take what is held, by whether anything is held, one change at a
 * time.  Waiting on the log is switched on before reading the pipe is
 * switched off, so a failure of either leaves the pipe read: the log is
 * then given up as gone, and what is held dropped.  The pipe left unread,
 * or the wait on the log left on, which would wake the helper without
 * end, stalls the build instead, which ends it. */
static void log_watch(int queue, struct log_copy *copy)
{
    int error;
    if (copy->start < copy->end) {
        error = queue_change(queue, STDERR_FILENO, EVFILT_WRITE, EV_ENABLE);
        if (error == 0 && copy->from >= 0) {
            error = queue_change(queue, copy->from, EVFILT_READ, EV_DISABLE);
            if (error != 0 && queue_change(queue, STDERR_FILENO, EVFILT_WRITE, EV_DISABLE) != 0)
                copy->stalled = error;
        }
        if (error != 0) {
            copy->unwatched = error;
            copy->gone = 1;
            copy->start = copy->end = 0;
        }
        return;
    }
    if (copy->from >= 0 && (error = queue_change(queue, copy->from, EVFILT_READ, EV_ENABLE)) != 0)
        copy->unwatched = copy->stalled = error;
    if ((error = queue_change(queue, STDERR_FILENO, EVFILT_WRITE, EV_DISABLE)) != 0)
        copy->unwatched = copy->stalled = error;
}

/* Note what a signal or the lifeline says in EVENT: a terminating signal,
 * or the lifeline's end, ends the build. */
static void watch_note(struct watch *watch, const struct kevent *event)
{
    if (event->filter == EVFILT_READ && watch->lifeline_fd >= 0 && (int)event->ident == watch->lifeline_fd) {
        char discard[LIFELINE_READ_BYTES];
        ssize_t read_now = read(watch->lifeline_fd, discard, sizeof discard);
        if (read_now <= 0 && !(read_now < 0 && errno == EINTR)) {
            /* Closing it takes it off the queue. */
            close(watch->lifeline_fd);
            watch->lifeline_fd = -1;
            watch->ending = 1;
        }
    } else if (event->filter == EVFILT_SIGNAL && (int)event->ident != SIGCHLD) {
        watch->ending = 1;
    }
}

/* Wait for the log to take more of what is held, for as long as the build
 * is not to end: a nova-nix that stops reading closes the log on its way
 * out, which fails the write, or ends the helper, so the wait lasts no
 * longer than the reader does.  Answers 1 once the log may take more, and
 * 0 once the build is to end or the log cannot be watched. */
static int log_wait(struct watch *watch, struct log_copy *copy)
{
    log_watch(watch->queue, copy);
    while (!watch->ending && !copy->gone && copy->stalled == 0) {
        struct kevent event;
        int got = kevent(watch->queue, NULL, 0, &event, 1, NULL);
        if (got < 0 && errno == EINTR)
            continue;
        if (got < 0) {
            copy->unwatched = errno;
            return 0;
        }
        if (got > 0 && event.filter == EVFILT_WRITE && (int)event.ident == STDERR_FILENO)
            return 1;
        if (got > 0)
            watch_note(watch, &event);
    }
    return 0;
}

/* How copying the rest of the builder's output ended. */
enum drain_outcome {
    /* The pipe ended, or holds nothing more, or the log has gone. */
    DRAIN_DONE,
    /* LOG_DRAIN_BYTES_MAX were copied and the pipe still held more. */
    DRAIN_ENDLESS,
    /* The build was to end, or the log could not be watched, before the
     * log took what was left. */
    DRAIN_CUT
};

/* Copy what is held, and what is left in the builder's pipe, into the log,
 * without waiting for a writer, which a process the sweep could not end
 * may be; then close the pipe.  Waits for the log to take it for as long
 * as the build is not to end, and copies at most LOG_DRAIN_BYTES_MAX, so
 * a leftover writer that never stops cannot keep the helper. */
static enum drain_outcome log_drain(struct watch *watch, struct log_copy *copy)
{
    enum drain_outcome outcome = DRAIN_DONE;
    size_t copied = 0;
    for (;;) {
        size_t got;
        if (!log_flush(copy)) {
            if (log_wait(watch, copy))
                continue;
            if (!copy->gone)
                outcome = DRAIN_CUT;
            copy->start = copy->end = 0;
            break;
        }
        if (copy->from < 0)
            break;
        if (copied >= LOG_DRAIN_BYTES_MAX) {
            outcome = DRAIN_ENDLESS;
            break;
        }
        got = log_fill(copy);
        if (got == 0 && copy->from >= 0)
            break;
        copied += got;
    }
    if (copy->from >= 0) {
        close(copy->from);
        copy->from = -1;
    }
    return outcome;
}

/* Write a message of the helper's to the log, in printf's terms, waiting
 * for the log to take it as log_drain does, and dropping it if the log has
 * gone or the build is to end. */
static void log_message(struct watch *watch, struct log_copy *copy, const char *format, ...)
{
    char message[LOG_MESSAGE_BYTES];
    int length;
    va_list arguments;
    va_start(arguments, format);
    length = vsnprintf(message, sizeof message, format, arguments);
    va_end(arguments);
    if (length < 0 || copy->gone)
        return;
    if ((size_t)length >= sizeof message)
        length = (int)sizeof message - 1;
    memcpy(copy->held, message, (size_t)length);
    copy->start = 0;
    copy->end = (size_t)length;
    for (;;) {
        if (log_flush(copy) || !log_wait(watch, copy))
            break;
    }
    copy->start = copy->end = 0;
}

/* ------------------------------------------------------------------------
 * The builder's process
 * ------------------------------------------------------------------------ */

/* Apply the plan's profile to this process with PARAMETERS.  On a refusal
 * the interpreter's words go to the build log, in upstream's line
 * (DarwinDerivationBuilder::setUser), since a status record holds no
 * text; the process ends right after, which releases them. */
static void apply_profile(const struct plan *plan, const char *const *parameters)
{
    char *refusal = NULL;
    if (sandbox_init_with_parameters(plan->profile, 0, parameters, &refusal) != 0) {
        fprintf(stderr, "failed to configure sandbox: %s\n", refusal != NULL ? refusal : "(null)");
        status_fail("sandbox", 0, NO_INDEX);
    }
}

/* The child the helper forks, which becomes the builder: upstream's
 * runChild for a build that is not a builtin.  LOG_FD is the writing end
 * of the pipe the helper copies into the build log, and REPORT_FD that of
 * the pipe the child reports a failed setup step on, which its exec
 * closes, and which the helper relays to the status channel. */
static void child_main(const struct plan *plan, const char *const *parameters, int log_fd, int report_fd, char **argv,
                       const sigset_t *original_mask)
{
    struct rlimit core;

    posix_report_to(report_fd);

    /* No controlling terminal, and no terminal signals; the builder leads
     * the process group the helper kills (commonChildInit). */
    if (setsid() < 0)
        status_fail("setsid", errno, NO_INDEX);

    if (chdir(plan->build_dir) < 0)
        status_fail("chdir", errno, NO_INDEX);

    if (dup2(log_fd, STDOUT_FILENO) < 0 || dup2(log_fd, STDERR_FILENO) < 0)
        status_fail("output", errno, NO_INDEX);

    /* Every descriptor but the report pipe, which closes when the builder
     * starts (runChild's closeExtraFDs). */
    posix_close_descriptors(descriptor_listing, posix_status_fd());

    /* No core dumps (runChild).  Upstream also raises the hard limit to
     * infinity, which an unprivileged process may not; the hard limit is
     * left as it is. */
    if (getrlimit(RLIMIT_CORE, &core) == 0) {
        core.rlim_cur = 0;
        setrlimit(RLIMIT_CORE, &core);
    }

    /* The build user's ids, with no supplementary group, before the
     * profile, as upstream's setUser drops to the build user before it
     * applies one; upstream gives a macOS build user no supplementary
     * groups. */
    if (plan->as_build_user) {
        if (setgroups(0, NULL) < 0)
            status_fail("setgroups", errno, NO_INDEX);
        if (setgid((gid_t)plan->build_gid) < 0 || getgid() != (gid_t)plan->build_gid
            || getegid() != (gid_t)plan->build_gid)
            status_fail("setgid", errno, NO_INDEX);
        if (setuid((uid_t)plan->build_uid) < 0 || getuid() != (uid_t)plan->build_uid
            || geteuid() != (uid_t)plan->build_uid)
            status_fail("setuid", errno, NO_INDEX);
    }

    /* Last before the exec, as upstream's runChild calls setUser. */
    apply_profile(plan, parameters);

    sigprocmask(SIG_SETMASK, original_mask, NULL);
    execve(argv[0], argv, plan->environment);
    status_fail("exec", errno, NO_INDEX);
}

/* ------------------------------------------------------------------------
 * The build's mark
 * ------------------------------------------------------------------------ */

/* Write a new mark's name into NAME, which holds MARK_LENGTH bytes and its
 * terminator. */
static void make_mark_name(char *name)
{
    static const char hex[] = "0123456789abcdef";
    const size_t prefix = sizeof MARK_PREFIX - 1;
    unsigned char bytes[MARK_RANDOM_BYTES];
    size_t i;
    arc4random_buf(bytes, sizeof bytes);
    memcpy(name, MARK_PREFIX, prefix);
    for (i = 0; i < MARK_RANDOM_BYTES; i++) {
        name[prefix + 2 * i] = hex[bytes[i] >> 4];
        name[prefix + 2 * i + 1] = hex[bytes[i] & 0xf];
    }
    name[MARK_LENGTH] = '\0';
}

/* The plan's parameters followed by the mark's, NULL-terminated, in a
 * buffer for the caller to free, or NULL. */
static const char **profile_parameters(const struct plan *plan, const struct build_mark *mark)
{
    size_t from_plan = 2 * plan->parameter_count;
    const char **parameters = calloc(from_plan + 2 * MARK_PARAMETER_COUNT + 1, sizeof *parameters);
    if (parameters == NULL)
        return NULL;
    if (from_plan > 0)
        memcpy(parameters, plan->parameters, from_plan * sizeof *parameters);
    parameters[from_plan] = TOKEN_PARAMETER;
    parameters[from_plan + 1] = mark->token;
    parameters[from_plan + 2] = DECOY_PARAMETER;
    parameters[from_plan + 3] = mark->decoy;
    return parameters;
}

/* ------------------------------------------------------------------------
 * The sweep
 * ------------------------------------------------------------------------ */

/* PIDs, in a buffer that grows. */
struct pid_set {
    pid_t *pids;
    size_t count;
    size_t capacity;
};

static int pid_set_has(const struct pid_set *set, pid_t pid)
{
    size_t i;
    for (i = 0; i < set->count; i++)
        if (set->pids[i] == pid)
            return 1;
    return 0;
}

/* Make room in SET for COUNT PIDs in all.  Answers 0, or ENOMEM. */
static int pid_set_reserve(struct pid_set *set, size_t count)
{
    pid_t *grown;
    if (count <= set->capacity)
        return 0;
    grown = realloc(set->pids, count * sizeof *grown);
    if (grown == NULL)
        return ENOMEM;
    set->pids = grown;
    set->capacity = count;
    return 0;
}

/* Add PID to SET, which has room for it. */
static void pid_set_put(struct pid_set *set, pid_t pid)
{
    set->pids[set->count++] = pid;
}

/* One of the user's processes as a listing found it: whether it was still
 * running then, rather than a zombie, which starts no other process. */
struct listed_process {
    pid_t pid;
    int running;
};

/* How listing the user's processes ended. */
enum listing_outcome {
    LISTED,
    /* A call failed, with errno set. */
    LISTING_FAILED,
    /* Processes started faster than a listing could be sized for them,
     * LISTING_ATTEMPTS_MAX times running. */
    LISTING_OUTRUN
};

/* The processes whose effective user is USER, in *LISTED, a buffer for the
 * caller to free, and their count in *COUNT. */
static enum listing_outcome user_processes(uid_t user, struct listed_process **listed, size_t *count)
{
    int request[4] = {CTL_KERN, KERN_PROC, KERN_PROC_UID, 0};
    int attempt;
    request[3] = (int)user;
    for (attempt = 0; attempt < LISTING_ATTEMPTS_MAX; attempt++) {
        size_t bytes = 0;
        size_t found;
        size_t i;
        struct kinfo_proc *processes;
        struct listed_process *each;
        if (sysctl(request, 4, NULL, &bytes, NULL, 0) < 0)
            return LISTING_FAILED;
        processes = malloc(bytes > 0 ? bytes : sizeof *processes);
        if (processes == NULL)
            return LISTING_FAILED;
        if (sysctl(request, 4, processes, &bytes, NULL, 0) < 0) {
            int error = errno;
            free(processes);
            /* More processes than when it was sized: size it again. */
            if (error == ENOMEM)
                continue;
            errno = error;
            return LISTING_FAILED;
        }
        found = bytes / sizeof *processes;
        each = malloc((found > 0 ? found : 1) * sizeof *each);
        if (each == NULL) {
            free(processes);
            return LISTING_FAILED;
        }
        for (i = 0; i < found; i++) {
            each[i].pid = processes[i].kp_proc.p_pid;
            each[i].running = processes[i].kp_proc.p_stat != SZOMB;
        }
        free(processes);
        *listed = each;
        *count = found;
        return LISTED;
    }
    return LISTING_OUTRUN;
}

/* Whether PID is a process that has not ended. */
static int still_running(pid_t pid)
{
    struct proc_bsdinfo info;
    return proc_pidinfo(pid, PROC_PIDTBSDINFO, 0, &info, sizeof info) == (int)sizeof info
        && info.pbi_status != SZOMB;
}

/* What sandbox_check says of a listed process and the build's mark. */
enum mark_answer {
    /* Not a process of this build. */
    MARK_ABSENT,
    /* A process of this build. */
    MARK_PRESENT,
    /* Not known: a process that was running when listed and has ended
     * since, which may have been this build's and may have started
     * another first, or one sandbox_check answered with an error. */
    MARK_UNKNOWN
};

/* Whether a listed process is one of this build's: its sandbox refuses
 * the decoy and lets it look up the token.  Under sandbox = true only
 * this build's profile allows the token, which this helper made, and ends
 * by refusing the decoy, after which nothing changes either answer; a
 * process in no sandbox, or in one that allows every name, may look up
 * the decoy; and a zombie, or a PID no process has, is refused both.
 *
 * The decoy is asked first.  A refusal shows the process is in a sandbox,
 * which can no longer change (a process in one is refused another), so
 * the token's answer comes from the same profile.  Asked the other way
 * round, a process that applies a sandbox of its own between the two,
 * as many of the user's applications and services do early in their
 * lives, would be allowed the token while it had none and refused the
 * decoy once it had one, and be taken for the build's. */
static enum mark_answer process_mark(const struct listed_process *process, const struct build_mark *mark)
{
    int name_check = SANDBOX_FILTER_GLOBAL_NAME | SANDBOX_CHECK_NO_REPORT;
    int decoy = sandbox_check(process->pid, "mach-lookup", name_check, mark->decoy);
    int token;
    if (decoy == 0)
        return MARK_ABSENT;
    if (decoy != 1)
        return MARK_UNKNOWN;
    token = sandbox_check(process->pid, "mach-lookup", name_check, mark->token);
    if (token == 0)
        return MARK_PRESENT;
    if (token != 1)
        return MARK_UNKNOWN;
    return process->running && !still_running(process->pid) ? MARK_UNKNOWN : MARK_ABSENT;
}

/* How killing a set of processes ended. */
enum ending {
    /* Every one is known to have ended. */
    ENDING_DONE,
    /* The end of one could not be watched for. */
    ENDING_UNWATCHED,
    /* One had not ended in time. */
    ENDING_LINGERING
};

/* Kill every process in SET and wait for each to end, for up to
 * SWEEP_EXIT_SECONDS.  Each is watched before it is killed, so its end
 * cannot be missed, and one that has already ended cannot be watched,
 * which says so.  Every one is killed, whatever the answer; *ERROR
 * receives the errno value behind ENDING_UNWATCHED. */
static enum ending end_processes(const struct pid_set *set, int *error)
{
    int queue = kqueue();
    enum ending ending = ENDING_DONE;
    size_t pending = 0;
    size_t i;
    struct timespec deadline;
    if (queue < 0) {
        *error = errno;
        ending = ENDING_UNWATCHED;
    }
    for (i = 0; i < set->count; i++) {
        struct kevent change;
        EV_SET(&change, (uintptr_t)set->pids[i], EVFILT_PROC, EV_ADD, NOTE_EXIT, 0, NULL);
        if (queue >= 0 && kevent(queue, &change, 1, NULL, 0, NULL) == 0) {
            pending++;
        } else if (queue >= 0 && errno != ESRCH) {
            *error = errno;
            ending = ENDING_UNWATCHED;
        }
        kill(set->pids[i], SIGKILL);
    }
    if (pending > 0 && deadline_in(SWEEP_EXIT_SECONDS, &deadline) < 0) {
        *error = errno;
        ending = ENDING_UNWATCHED;
        pending = 0;
    }
    while (pending > 0) {
        struct kevent event;
        struct timespec left;
        int got;
        int some_left = time_left(&deadline, &left);
        if (some_left < 0) {
            *error = errno;
            ending = ENDING_UNWATCHED;
            break;
        }
        if (some_left == 0) {
            if (ending == ENDING_DONE)
                ending = ENDING_LINGERING;
            break;
        }
        got = kevent(queue, NULL, 0, &event, 1, &left);
        if (got < 0 && errno == EINTR)
            continue;
        if (got < 0) {
            *error = errno;
            ending = ENDING_UNWATCHED;
            break;
        }
        if (got > 0 && event.filter == EVFILT_PROC && (event.fflags & NOTE_EXIT) != 0)
            pending--;
    }
    if (queue >= 0)
        close(queue);
    return ending;
}

/* How a sweep ended. */
enum sweep_outcome {
    /* A listing, taken once every process of the build found before had
     * ended, found none. */
    SWEEP_DONE,
    /* The user's processes could not be listed, or the sets of the
     * build's could not grow. */
    SWEEP_UNLISTED,
    /* The user's processes started faster than a listing could be sized,
     * LISTING_ATTEMPTS_MAX times running. */
    SWEEP_OUTRUN,
    /* The listings ran out while the build's processes were still found. */
    SWEEP_UNSETTLED,
    /* The end of a process the sweep killed could not be watched for. */
    SWEEP_UNWATCHED,
    /* A process the sweep killed had not ended in time. */
    SWEEP_LINGERING
};

/* Kill every process in STOPPED that is still one of the build's, and
 * wait for each to end.  Only a process sandbox_check finds to be the
 * build's is killed, which never kills a PID another process has taken
 * since; one it cannot answer for (MARK_UNKNOWN) is left as it is,
 * stopped if it was the build's, neither killed nor continued, as
 * continuing it could free a process of the build.  With no room to
 * gather them, each is asked and killed in turn without waiting for its
 * end. */
static void end_still_marked(const struct pid_set *stopped, const struct build_mark *mark)
{
    struct pid_set still = {NULL, 0, 0};
    size_t i;
    int ignored;
    if (pid_set_reserve(&still, stopped->count) != 0) {
        for (i = 0; i < stopped->count; i++) {
            struct listed_process process;
            process.pid = stopped->pids[i];
            process.running = 1;
            if (process_mark(&process, mark) == MARK_PRESENT)
                kill(process.pid, SIGKILL);
        }
        return;
    }
    for (i = 0; i < stopped->count; i++) {
        struct listed_process process;
        process.pid = stopped->pids[i];
        process.running = 1;
        if (process_mark(&process, mark) == MARK_PRESENT)
            pid_set_put(&still, process.pid);
    }
    end_processes(&still, &ignored);
    free(still.pids);
}

/* End every process of the build, whose processes are USER's.  Each
 * listing stops every process of the
 * build it finds, those it has stopped before included, since one of them
 * may continue another (SIGCONT, which the profile lets a build send its
 * own processes), and a process stopped while it forks still starts its
 * child.  Room to record every process a listing holds is made before any
 * is stopped, and each is recorded before it is stopped, so none is
 * stopped and then forgotten.  Once a listing finds none it has not
 * stopped, and no process that ended before it could be asked, the
 * processes of the build that listing found are killed and waited for,
 * and the listing starts over.  A PID stopped before and absent from that
 * listing is not killed: the listing holds every process whose effective
 * user is USER, so that PID now names none of the user's, the process
 * having ended, or another having taken its PID; a process of the build
 * cannot become another user, as macOS refuses a process in a sandbox the
 * exec of a setuid program.  The sweep is done only when a listing, taken
 * once every process of the build found before has ended, finds none of
 * the build's and none it could not tell.  A process can start another
 * only while it runs, and every process running when that listing was
 * taken was asked, so none of the build's is left, short of one that ended
 * after that listing and whose PID another process was given before it
 * was asked.  The listings are bounded; whatever the outcome, every process
 * stopped that is still the build's is killed (end_still_marked, which
 * says which few it leaves).  *ERROR receives the errno value behind
 * SWEEP_UNLISTED or SWEEP_UNWATCHED. */
static enum sweep_outcome sweep_build(const struct build_mark *mark, uid_t user, int *error)
{
    struct pid_set stopped = {NULL, 0, 0};
    struct pid_set present = {NULL, 0, 0};
    pid_t self = getpid();
    enum sweep_outcome outcome = SWEEP_UNSETTLED;
    size_t pass;
    for (pass = 0; pass < SWEEP_PASSES_MAX && outcome == SWEEP_UNSETTLED; pass++) {
        size_t count = 0;
        size_t fresh = 0;
        size_t unknown = 0;
        size_t i;
        struct listed_process *listed = NULL;
        switch (user_processes(user, &listed, &count)) {
        case LISTED:
            break;
        case LISTING_FAILED:
            *error = errno;
            outcome = SWEEP_UNLISTED;
            break;
        case LISTING_OUTRUN:
            outcome = SWEEP_OUTRUN;
            break;
        }
        if (outcome != SWEEP_UNSETTLED)
            break;
        present.count = 0;
        if (pid_set_reserve(&present, count) != 0 || pid_set_reserve(&stopped, stopped.count + count) != 0) {
            free(listed);
            *error = ENOMEM;
            outcome = SWEEP_UNLISTED;
            break;
        }
        for (i = 0; i < count; i++) {
            pid_t pid = listed[i].pid;
            if (pid <= 0 || pid == self)
                continue;
            switch (process_mark(&listed[i], mark)) {
            case MARK_ABSENT:
                break;
            case MARK_UNKNOWN:
                unknown++;
                break;
            case MARK_PRESENT:
                pid_set_put(&present, pid);
                if (!pid_set_has(&stopped, pid)) {
                    pid_set_put(&stopped, pid);
                    fresh++;
                }
                kill(pid, SIGSTOP);
                break;
            }
        }
        free(listed);
        if (fresh > 0 || unknown > 0)
            continue;
        if (present.count == 0) {
            outcome = SWEEP_DONE;
            break;
        }
        switch (end_processes(&present, error)) {
        case ENDING_DONE:
            break;
        case ENDING_UNWATCHED:
            outcome = SWEEP_UNWATCHED;
            break;
        case ENDING_LINGERING:
            outcome = SWEEP_LINGERING;
            break;
        }
        present.count = 0;
        stopped.count = 0;
    }
    if (stopped.count > 0)
        end_still_marked(&stopped, mark);
    free(present.pids);
    free(stopped.pids);
    return outcome;
}

/* ------------------------------------------------------------------------
 * The helper
 * ------------------------------------------------------------------------ */

/* The handled signals the helper watches, written into WATCHED, which
 * holds HANDLED_SIGNAL_COUNT; answers how many.  A signal this process
 * started out ignoring is left out, as one started under nohup ignores
 * SIGHUP, and the builder inherits it ignored: the helper runs in
 * nova-nix's process group, and a signal sent to that group, which
 * nova-nix ignores and carries on through, must not end the build, as it
 * never reaches upstream's builder in a session of its own. */
static size_t watched_signals(int *watched)
{
    size_t count = 0;
    size_t i;
    for (i = 0; i < HANDLED_SIGNAL_COUNT; i++) {
        struct sigaction current;
        if (handled_signals[i] != SIGCHLD && sigaction(handled_signals[i], NULL, &current) == 0
            && current.sa_handler == SIG_IGN)
            continue;
        watched[count++] = handled_signals[i];
    }
    return count;
}

/* A queue that wakes on each of the COUNT signals in WATCHED, on the
 * lifeline, and on the builder's pipe, made before the builder exists.
 * Answers the queue, or -1 with errno set. */
static int watch_queue(int lifeline_fd, int log_fd, const int *watched, size_t count)
{
    struct kevent changes[HANDLED_SIGNAL_COUNT + 2];
    size_t i;
    int queue = kqueue();
    if (queue < 0)
        return -1;
    for (i = 0; i < count; i++)
        EV_SET(&changes[i], (uintptr_t)watched[i], EVFILT_SIGNAL, EV_ADD, 0, 0, NULL);
    EV_SET(&changes[count], (uintptr_t)lifeline_fd, EVFILT_READ, EV_ADD, 0, 0, NULL);
    EV_SET(&changes[count + 1], (uintptr_t)log_fd, EVFILT_READ, EV_ADD, 0, 0, NULL);
    if (kevent(queue, changes, (int)(count + 2), NULL, 0, NULL) < 0) {
        int error = errno;
        close(queue);
        errno = error;
        return -1;
    }
    return queue;
}

/* Add the log taking more to QUEUE, not yet watched for.  Standard error
 * has to be something kqueue can watch for writing, as the pipe runHelper
 * passes is and /dev/null is not (EINVAL).  Answers 0, or -1 with errno
 * set. */
static int watch_log(int queue)
{
    struct kevent change;
    EV_SET(&change, (uintptr_t)STDERR_FILENO, EVFILT_WRITE, EV_ADD | EV_DISABLE, 0, 0, NULL);
    return kevent(queue, &change, 1, NULL, 0, NULL);
}

/* Kill every process of the build that is still in the builder's group,
 * and the builder itself, which may not yet lead the group it makes. */
static void kill_build(pid_t child)
{
    kill(-child, SIGKILL);
    kill(child, SIGKILL);
}

/* Whether the builder has ended, leaving it to be reaped. */
static int builder_ended(pid_t child)
{
    siginfo_t info;
    memset(&info, 0, sizeof info);
    return waitid(P_PID, (id_t)child, &info, WEXITED | WNOHANG | WNOWAIT) == 0 && info.si_pid == child;
}

/* Wait for the builder, copying its output into the build log as the log
 * takes it, killing the build on a terminating signal or when the
 * lifeline closes, and the rest of its group once it has ended.  Answers
 * 0 with how it ended in *WAIT_STATUS, or the errno of a wait that
 * failed, which ends the build: the builder is killed, and how it ended
 * cannot be known. */
static int wait_for_builder(pid_t child, struct watch *watch, struct log_copy *copy, int *wait_status)
{
    int queue = watch->queue;
    for (;;) {
        struct kevent event;
        int got;
        if (builder_ended(child)) {
            kill(-child, SIGKILL);
            return posix_reap(child, wait_status);
        }
        got = kevent(queue, NULL, 0, &event, 1, NULL);
        if (got < 0) {
            if (errno == EINTR)
                continue;
            /* With nothing left to watch, the build is ended and reaped. */
            kill_build(child);
            return posix_reap(child, wait_status);
        }
        if (got == 0)
            continue;
        if (event.filter == EVFILT_READ && copy->from >= 0 && (int)event.ident == copy->from) {
            /* A read the queue reported before it stopped watching the
             * pipe waits until what is held has gone. */
            if (copy->start == copy->end)
                log_fill(copy);
            log_flush(copy);
            log_watch(queue, copy);
            if (copy->stalled != 0 && !watch->ending) {
                watch->ending = 1;
                kill_build(child);
            }
        } else if (event.filter == EVFILT_WRITE && (int)event.ident == STDERR_FILENO) {
            log_flush(copy);
            log_watch(queue, copy);
            if (copy->stalled != 0 && !watch->ending) {
                watch->ending = 1;
                kill_build(child);
            }
        } else {
            watch_note(watch, &event);
            if (watch->ending)
                kill_build(child);
        }
    }
}

/* Relay to the status channel what the child reported on its pipe, read
 * from FD once the child has ended or executed the builder, either of
 * which closes its end.  Answers 1 when it reported a failed step, and 0
 * when it reported nothing; a read that fails ends the helper with its
 * own record. */
static int relay_report(int fd)
{
    char report[REPORT_BYTES_MAX];
    size_t got = 0;
    for (;;) {
        ssize_t read_now = read(fd, report + got, sizeof report - got);
        if (read_now < 0 && errno == EINTR)
            continue;
        if (read_now < 0)
            status_fail("relay", errno, NO_INDEX);
        if (read_now == 0 || (got += (size_t)read_now) == sizeof report)
            break;
    }
    close(fd);
    if (got == 0)
        return 0;
    backend_write_status(report, got);
    return 1;
}

/* Set O_NONBLOCK on FD.  Answers 0, or -1 with errno set. */
static int set_nonblocking(int fd)
{
    int flags = fcntl(fd, F_GETFL);
    return flags < 0 ? -1 : fcntl(fd, F_SETFL, flags | O_NONBLOCK);
}

int backend_run(const struct plan *plan, char **argv)
{
    int null_fd;
    int log_pipe[2];
    int report_pipe[2];
    int reported;
    int error;
    int sweep_error = 0;
    size_t i;
    size_t watched_count;
    int watched[HANDLED_SIGNAL_COUNT];
    pid_t child;
    sigset_t handled;
    sigset_t original_mask;
    int wait_status = 0;
    struct build_mark mark;
    const char **parameters;
    enum sweep_outcome sweep;
    enum drain_outcome drain;
    struct watch watch;
    struct log_copy copy;

    /* Standard input, past the plan, is the lifeline; the builder reads
     * /dev/null (commonChildInit). */
    watch.lifeline_fd = fcntl(STDIN_FILENO, F_DUPFD_CLOEXEC, 3);
    watch.ending = 0;
    null_fd = open("/dev/null", O_RDWR | O_CLOEXEC);
    if (watch.lifeline_fd < 0 || null_fd < 0 || dup2(null_fd, STDIN_FILENO) < 0)
        status_fail("stdin", errno, NO_INDEX);
    close(null_fd);

    /* The builder's pipe, both ends closed on exec: the builder takes the
     * writing end as its standard output and error, and the helper reads
     * it without waiting. */
    if (pipe(log_pipe) < 0 || fcntl(log_pipe[0], F_SETFD, FD_CLOEXEC) < 0
        || fcntl(log_pipe[1], F_SETFD, FD_CLOEXEC) < 0 || set_nonblocking(log_pipe[0]) < 0)
        status_fail("log", errno, NO_INDEX);
    /* The helper writes the log without waiting either. */
    if (set_nonblocking(STDERR_FILENO) < 0)
        status_fail("logmode", errno, NO_INDEX);
    /* The child's report of a failed setup step, both ends closed on
     * exec, so that the helper alone writes the status channel once the
     * child exists and writes at most one record. */
    if (pipe(report_pipe) < 0 || fcntl(report_pipe[0], F_SETFD, FD_CLOEXEC) < 0
        || fcntl(report_pipe[1], F_SETFD, FD_CLOEXEC) < 0)
        status_fail("report", errno, NO_INDEX);
    copy.from = log_pipe[0];
    copy.start = copy.end = 0;
    copy.gone = 0;
    copy.unwatched = 0;
    copy.stalled = 0;

    make_mark_name(mark.token);
    make_mark_name(mark.decoy);
    parameters = profile_parameters(plan, &mark);
    if (parameters == NULL)
        status_fail("sweep", errno, NO_INDEX);

    /* The signals the helper answers are read through the queue, which
     * records each one sent once it watches for it, blocked or not, but
     * not one already pending when it starts to watch.  So the queue
     * watches first and the signals are blocked after: one sent before the
     * block ends the helper, before any builder exists, and none sent
     * after it is lost.  The builder unblocks them before it executes. */
    watched_count = watched_signals(watched);
    watch.queue = watch_queue(watch.lifeline_fd, log_pipe[0], watched, watched_count);
    if (watch.queue < 0)
        status_fail("signals", errno, NO_INDEX);
    if (watch_log(watch.queue) < 0)
        status_fail("logwatch", errno, NO_INDEX);
    sigemptyset(&handled);
    for (i = 0; i < watched_count; i++)
        sigaddset(&handled, watched[i]);
    if (sigprocmask(SIG_BLOCK, &handled, &original_mask) < 0)
        status_fail("signals", errno, NO_INDEX);

    child = fork();
    if (child < 0)
        status_fail("fork", errno, NO_INDEX);
    if (child == 0) {
        close(report_pipe[0]);
        child_main(plan, parameters, log_pipe[1], report_pipe[1], argv, &original_mask);
        _exit(EXIT_FAILURE);
    }
    /* The child reports a failed setup step on its pipe, which its exec
     * closes, and the helper relays it.  The helper keeps the status
     * channel for that and for the one failure the build log cannot carry,
     * one to watch the log itself, which it reports only when the child
     * reported nothing, so the channel never holds two records. */
    close(log_pipe[1]);
    close(report_pipe[1]);
    free(parameters);

    /* The queue stays open past the builder's end, so the drain and the
     * last messages wait on the log only while the build is not to end. */
    error = wait_for_builder(child, &watch, &copy, &wait_status);
    reported = relay_report(report_pipe[0]);
    sweep = sweep_build(&mark, plan->as_build_user ? (uid_t)plan->build_uid : getuid(), &sweep_error);
    drain = log_drain(&watch, &copy);
    if (copy.unwatched != 0 && !reported)
        status_fail("logwatch", copy.unwatched, NO_INDEX);
    /* Every other reason goes to the build log. */
    if (error != 0) {
        /* Upstream's words for a wait that fails (Pid::wait in
         * processes.cc). */
        log_message(&watch, &copy, "nova-nix-sandbox: cannot get exit status of PID %ld: %s\n", (long)child,
                    strerror(error));
        return EXIT_FAILURE;
    }
    /* A process of the build may be left in each of these, so the build
     * fails. */
    switch (sweep) {
    case SWEEP_DONE:
        break;
    case SWEEP_UNLISTED:
        log_message(&watch, &copy, "nova-nix-sandbox: cannot list the build's processes: %s\n",
                    strerror(sweep_error));
        return EXIT_FAILURE;
    case SWEEP_OUTRUN:
        log_message(&watch, &copy,
                    "nova-nix-sandbox: processes started faster than they could be listed, %d times running\n",
                    LISTING_ATTEMPTS_MAX);
        return EXIT_FAILURE;
    case SWEEP_UNSETTLED:
        log_message(&watch, &copy, "nova-nix-sandbox: the build's processes were still running after %d listings\n",
                    SWEEP_PASSES_MAX);
        return EXIT_FAILURE;
    case SWEEP_UNWATCHED:
        log_message(&watch, &copy, "nova-nix-sandbox: cannot watch the build's processes end: %s\n",
                    strerror(sweep_error));
        return EXIT_FAILURE;
    case SWEEP_LINGERING:
        log_message(&watch, &copy,
                    "nova-nix-sandbox: a process of the build had not ended %d seconds after it was killed\n",
                    SWEEP_EXIT_SECONDS);
        return EXIT_FAILURE;
    }
    switch (drain) {
    case DRAIN_DONE:
        break;
    case DRAIN_ENDLESS:
        log_message(&watch, &copy, "nova-nix-sandbox: the build's output went on past %d bytes after it ended\n",
                    LOG_DRAIN_BYTES_MAX);
        return EXIT_FAILURE;
    case DRAIN_CUT:
        /* The build was to end before the log took all its output, at
         * nova-nix's word, which no longer reads it: the end the builder
         * came to is relayed all the same, as for a build that ended with
         * nothing left to copy. */
        break;
    }
    return posix_relay(wait_status);
}

/* ------------------------------------------------------------------------
 * The probe
 * ------------------------------------------------------------------------ */

const char *backend_probe(int *error)
{
    static const char *const no_parameters[] = {NULL};
    char *refusal = NULL;
    *error = 0;
    if (sandbox_init_with_parameters(probe_profile, 0, no_parameters, &refusal) != 0) {
        if (refusal != NULL)
            fprintf(stderr, "nova-nix-sandbox: %s\n", refusal);
        return "sandbox";
    }
    return NULL;
}
