/*
 * nova-nix-sandbox: runs one nova-nix builder in fresh user, mount, PID,
 * IPC and UTS namespaces, and a network namespace unless the build may
 * reach the host's network, with a chroot that holds the build's inputs.
 *
 * It is a program of its own because the setup has to run in C between
 * the namespaces' creation and the builder's exec, and nova-nix cannot run
 * any: the process library it spawns builders with gives no hook between
 * fork and exec, and a child forked from the threaded GHC runtime may make
 * only async-signal-safe calls before it execs.  nova-nix spawns this
 * program as it spawns a builder, with an empty environment, so the
 * dynamic loader reads no variable the derivation set.
 *
 *   nova-nix-sandbox run PROGRAM [ARG...]
 *     Reads the plan from standard input, sets the sandbox up and executes
 *     PROGRAM with ARGs and the environment the plan holds.  Standard
 *     output is the status pipe and standard error the build log: the
 *     status pipe moves to a close-on-exec descriptor and standard output
 *     becomes the log, so a builder that starts closes it, and the reader
 *     sees end of file with nothing written.  A step that fails writes one
 *     record, "STAGE ERRNO INDEX\n", and the process exits.  Standard input
 *     stays open after the plan as a lifeline: when nova-nix closes it, by
 *     ending or dying, the builder is killed.
 *
 *   nova-nix-sandbox probe
 *     Checks that this system lets an unprivileged process make the
 *     namespaces, as upstream's userNamespacesSupported and
 *     mountAndPidNamespacesSupported do (namespaces.cc:16-98 at Nix
 *     2.24.9).  Exits 0 when it does, or writes one record naming the
 *     check that failed and exits 1.
 *
 * The plan is a version line, a line holding the length of the rest, then
 * the tokens plan.h describes, which plan.c parses.  Its format and the
 * stage names are shared with Nix.Builder.Sandbox, which writes the one
 * and reads the other.
 *
 * The steps follow upstream's LocalDerivationGoal::startBuilder and
 * runChild (local-derivation-goal.cc at 2.24.9), which this file cites by
 * line as LDG.
 */

#define _GNU_SOURCE

#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <grp.h>
#include <limits.h>
#include <linux/keyctl.h>
#include <net/if.h>
#include <netinet/in.h>
#include <poll.h>
#include <sched.h>
#include <signal.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/mount.h>
#include <sys/prctl.h>
#include <sys/resource.h>
#include <sys/signalfd.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/statvfs.h>
#include <sys/syscall.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

#include "mountinfo.h"
#include "plan.h"

/* mount_setattr and what it takes, from the kernel's UAPI at Linux 5.12
 * (include/uapi/linux/mount.h, include/uapi/linux/fcntl.h and the syscall
 * tables).  They are spelled out here because a C library before glibc
 * 2.36 declares none of them, and <linux/mount.h> conflicts with
 * <sys/mount.h> there.  The number is 442 in the asm-generic table and
 * x86's; an architecture this file does not know uses the mountinfo walk
 * alone. */
#if defined(SYS_mount_setattr)
#define MOUNT_SETATTR_SYSCALL SYS_mount_setattr
#elif defined(__x86_64__) || defined(__i386__) || defined(__aarch64__) || defined(__arm__) || defined(__riscv)
#define MOUNT_SETATTR_SYSCALL 442
#endif
#define MOUNT_SETATTR_READ_ONLY ((uint64_t)0x00000001)
#define MOUNT_SETATTR_RECURSIVE 0x8000

struct mount_setattr_attributes {
    uint64_t attr_set;
    uint64_t attr_clr;
    uint64_t propagation;
    uint64_t userns_fd;
};

/* The largest plan read, so a runaway writer cannot exhaust memory. */
#define PLAN_MAX_BYTES ((size_t)64 * 1024 * 1024)

/* The longest line of the plan's header, its length line included. */
#define PLAN_HEADER_LINE_MAX 64


/* The index a record carries when the failure is of no plan entry. */
#define NO_INDEX (-1L)

/* The byte the helper sends the child once its ids are mapped. */
#define RELEASE_BYTE '1'

/* Exit statuses of the probe's mount check, read back by the probe. */
#define PROBE_PRIVATE_FAILED 1
#define PROBE_PROC_FAILED 2

/* The hostname and domain name a build sees (LDG 1781-1786).  Upstream
 * passes sizeof, so the kernel holds the terminating NUL too. */
static const char sandbox_hostname[] = "localhost";
static const char sandbox_domainname[] = "(none)";

/* What a devpts instance of the sandbox's own is mounted with (LDG 1916). */
static const char devpts_options[] = "newinstance,mode=0620";

/* The mount table of the process reading it. */
static const char mountinfo_path[] = "/proc/self/mountinfo";

/* What the read of a file with no size, such as one in /proc, starts with. */
#define PROC_READ_START_BYTES ((size_t)16 * 1024)

/* What the cloned child needs: the plan, the program and the pipes. */
struct child_context {
    const struct plan *plan;
    char **argv;
    int status_fd;
    int sync_read;
    int sync_write;
    sigset_t original_mask;
};

/* ------------------------------------------------------------------------
 * Records
 * ------------------------------------------------------------------------ */

static void write_all(int fd, const char *bytes, size_t length)
{
    while (length > 0) {
        ssize_t written = write(fd, bytes, length);
        if (written < 0) {
            if (errno == EINTR)
                continue;
            return;
        }
        bytes += written;
        length -= (size_t)written;
    }
}

/* Write the one record a failure reports. */
static void report(int status_fd, const char *stage, int error, long index)
{
    char record[128];
    int length = snprintf(record, sizeof record, "%s %d %ld\n", stage, error, index);
    if (length > 0 && (size_t)length < sizeof record)
        write_all(status_fd, record, (size_t)length);
}

/* Report a failure and end the process that saw it. */
static void fail(int status_fd, const char *stage, int error, long index)
{
    report(status_fd, stage, error, index);
    _exit(EXIT_FAILURE);
}

/* ------------------------------------------------------------------------
 * The plan
 * ------------------------------------------------------------------------ */

/* Read exactly LENGTH bytes from standard input.  Answers 0, or an errno
 * value; EPIPE for input that ends early. */
static int read_exactly(char *bytes, size_t length)
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

/* Read one header line, a byte at a time so nothing past it is taken from
 * the pipe.  Answers 0, or an errno value. */
static int read_header_line(char *line, size_t capacity)
{
    size_t length = 0;
    for (;;) {
        char byte;
        int error = read_exactly(&byte, 1);
        if (error != 0)
            return error;
        if (length + 1 >= capacity)
            return EINVAL;
        line[length++] = byte;
        if (byte == '\n') {
            line[length] = '\0';
            return 0;
        }
    }
}

/* Read the plan: the version line, the length line, then that many bytes
 * of tokens, leaving the pipe open after them.  Answers 0, or an errno
 * value. */
static int read_plan_bytes(char **bytes_out, size_t *length_out)
{
    char line[PLAN_HEADER_LINE_MAX];
    char *rest = NULL;
    unsigned long long length;
    char *bytes;
    int error = read_header_line(line, sizeof line);
    if (error != 0)
        return error;
    if (strcmp(line, PLAN_VERSION_LINE) != 0)
        return EINVAL;
    error = read_header_line(line, sizeof line);
    if (error != 0)
        return error;
    if (line[0] < '0' || line[0] > '9')
        return EINVAL;
    errno = 0;
    length = strtoull(line, &rest, 10);
    if (errno != 0 || rest == NULL || strcmp(rest, "\n") != 0)
        return EINVAL;
    if (length > PLAN_MAX_BYTES)
        return E2BIG;
    bytes = malloc(length > 0 ? (size_t)length : 1);
    if (bytes == NULL)
        return ENOMEM;
    error = read_exactly(bytes, (size_t)length);
    if (error != 0) {
        free(bytes);
        return error;
    }
    *bytes_out = bytes;
    *length_out = (size_t)length;
    return 0;
}

/* ------------------------------------------------------------------------
 * Paths
 * ------------------------------------------------------------------------ */

/* PREFIX followed by PATH, or NULL with errno set. */
static char *joined(const char *prefix, const char *path)
{
    size_t prefix_length = strlen(prefix);
    size_t path_length = strlen(path);
    char *result = malloc(prefix_length + path_length + 1);
    if (result == NULL) {
        errno = ENOMEM;
        return NULL;
    }
    memcpy(result, prefix, prefix_length);
    memcpy(result + prefix_length, path, path_length + 1);
    return result;
}

static int path_exists(const char *path)
{
    struct stat st;
    return lstat(path, &st) == 0;
}

/* Create PATH and every missing directory above it, as upstream's
 * createDirs does.  Answers 0, or -1 with errno set. */
static int create_dirs(const char *path)
{
    char *copy = joined("", path);
    char *slash;
    if (copy == NULL)
        return -1;
    for (slash = strchr(copy + 1, '/'); ; slash = strchr(slash + 1, '/')) {
        if (slash != NULL)
            *slash = '\0';
        if (mkdir(copy, 0777) < 0 && errno != EEXIST) {
            int error = errno;
            free(copy);
            errno = error;
            return -1;
        }
        if (slash == NULL)
            break;
        *slash = '/';
    }
    free(copy);
    return 0;
}

/* ------------------------------------------------------------------------
 * Mounts
 * ------------------------------------------------------------------------ */

/* The flags a read-only remount of a bind must keep: in a user namespace
 * the kernel refuses to clear those the source mount locked. */
static unsigned long locked_flags(const char *path, int *error)
{
    struct statvfs fs;
    unsigned long flags = 0;
    if (statvfs(path, &fs) < 0) {
        *error = errno;
        return 0;
    }
    if (fs.f_flag & ST_NOSUID) flags |= MS_NOSUID;
    if (fs.f_flag & ST_NODEV) flags |= MS_NODEV;
    if (fs.f_flag & ST_NOEXEC) flags |= MS_NOEXEC;
    if (fs.f_flag & ST_NOATIME) flags |= MS_NOATIME;
    if (fs.f_flag & ST_NODIRATIME) flags |= MS_NODIRATIME;
    if (fs.f_flag & ST_RELATIME) flags |= MS_RELATIME;
    *error = 0;
    return flags;
}

/* Read the whole of a file that reports no size, such as one in /proc,
 * into a buffer of its own.  Answers the buffer, or NULL with errno set. */
static char *read_whole(const char *path, size_t *length)
{
    size_t capacity = PROC_READ_START_BYTES;
    size_t used = 0;
    char *bytes = malloc(capacity);
    int fd = open(path, O_RDONLY | O_CLOEXEC);
    int error = 0;
    if (bytes == NULL || fd < 0) {
        error = bytes == NULL ? ENOMEM : errno;
        goto failed;
    }
    for (;;) {
        ssize_t got;
        if (used == capacity) {
            char *larger = realloc(bytes, capacity * 2);
            if (larger == NULL) {
                error = ENOMEM;
                goto failed;
            }
            bytes = larger;
            capacity *= 2;
        }
        got = read(fd, bytes + used, capacity - used);
        if (got < 0) {
            if (errno == EINTR)
                continue;
            error = errno;
            goto failed;
        }
        if (got == 0)
            break;
        used += (size_t)got;
    }
    close(fd);
    *length = used;
    return bytes;

failed:
    if (fd >= 0)
        close(fd);
    free(bytes);
    errno = error;
    return NULL;
}

/* The /proc entry for a descriptor, through which a path-taking call acts
 * on what the descriptor opened. */
static void descriptor_path(char *path, size_t capacity, int fd)
{
    snprintf(path, capacity, "/proc/self/fd/%d", fd);
}

/* Open what is at RELATIVE beneath the directory or mount TOP names, one
 * component at a time without following a link, so the walk stays beneath
 * TOP whatever another user does to a path meanwhile; each step crosses
 * into a mount on the component it reaches.  Answers a descriptor, or -1
 * with errno set. */
static int open_beneath(int top, char *relative)
{
    char *cursor = relative;
    int at = fcntl(top, F_DUPFD_CLOEXEC, 0);
    while (at >= 0) {
        char *slash;
        int next;
        int error;
        while (*cursor == '/')
            cursor++;
        if (*cursor == '\0')
            break;
        slash = strchr(cursor, '/');
        if (slash != NULL)
            *slash = '\0';
        next = openat(at, cursor, O_PATH | O_NOFOLLOW | O_CLOEXEC);
        error = errno;
        close(at);
        errno = error;
        at = next;
        if (slash == NULL)
            break;
        cursor = slash + 1;
    }
    return at;
}

/* Remount the mount a descriptor names read-only, keeping the flags it
 * locked.  Answers 0, or an errno value. */
static int remount_read_only_at(int mounted)
{
    char path[64];
    int error = 0;
    unsigned long keep;
    descriptor_path(path, sizeof path, mounted);
    keep = locked_flags(path, &error);
    if (error != 0)
        return error;
    return mount(NULL, path, NULL, MS_BIND | MS_REMOUNT | MS_RDONLY | keep, NULL) < 0 ? errno : 0;
}

/* Remount the mount MOUNTED names read-only, and each mount beneath it,
 * as the process's mountinfo lists them, each reached from MOUNTED by
 * open_beneath, not by its path.  The mount namespace is the child's own
 * and private, and nothing else mounts in it, so the list does not
 * change under the walk.  Answers 0, or an errno value. */
static int remount_tree_read_only(int mounted)
{
    char link[64];
    char top[PATH_MAX];
    char point[PATH_MAX];
    size_t point_length = 0;
    size_t length = 0;
    struct mountinfo_cursor cursor;
    enum mountinfo_read found;
    int error = 0;
    ssize_t top_length;
    char *mountinfo;
    descriptor_path(link, sizeof link, mounted);
    top_length = readlink(link, top, sizeof top - 1);
    if (top_length < 0)
        return errno;
    top[top_length] = '\0';
    mountinfo = read_whole(mountinfo_path, &length);
    if (mountinfo == NULL)
        return errno;
    cursor.next = mountinfo;
    cursor.end = mountinfo + length;
    while ((found = mountinfo_next_within(&cursor, top, (size_t)top_length, point, sizeof point - 1, &point_length))
           == MOUNTINFO_FOUND) {
        int beneath;
        point[point_length] = '\0';
        if (strlen(point) != point_length) {
            error = EINVAL;
            break;
        }
        beneath = open_beneath(mounted, point + top_length);
        if (beneath < 0) {
            error = errno;
            break;
        }
        error = remount_read_only_at(beneath);
        close(beneath);
        if (error != 0)
            break;
    }
    if (error == 0 && found == MOUNTINFO_TOO_LONG)
        error = ENAMETOOLONG;
    free(mountinfo);
    return error;
}

/* Make the mount a descriptor names read-only, and every mount beneath
 * it, which a recursive bind brought along, so no part of a host path a
 * build sees stays writable.  mount_setattr does it in one step, setting
 * the flag without touching the others, so a locked one stays as it is.
 * A kernel before Linux 5.12 has no mount_setattr; there each mount is
 * remounted read-only in turn.  Answers 0, or an errno value. */
static int make_read_only(int mounted)
{
#if defined(MOUNT_SETATTR_SYSCALL)
    struct mount_setattr_attributes attributes;
    memset(&attributes, 0, sizeof attributes);
    attributes.attr_set = MOUNT_SETATTR_READ_ONLY;
    if (syscall(MOUNT_SETATTR_SYSCALL, mounted, "", AT_EMPTY_PATH | MOUNT_SETATTR_RECURSIVE, &attributes,
                sizeof attributes)
        == 0)
        return 0;
    if (errno != ENOSYS)
        return errno;
#endif
    return remount_tree_read_only(mounted);
}

/* What make_point answers for a symlink source, copied rather than
 * bound. */
#define TARGET_IS_COPIED_LINK (-2)

/* How a directory on the way to a target is opened: as a path alone,
 * which takes search on it and not read, as a directory a bind may have
 * brought in at 0711 allows, and serves to open, make and link beneath
 * it and to mount on. */
#define WALK_FLAGS (O_PATH | O_DIRECTORY | O_NOFOLLOW | O_CLOEXEC)

/* Open the directory NAME in PARENT, making it where it is missing, without
 * following a link.  Answers a descriptor, or -1 with errno set, ELOOP for
 * a link. */
static int enter_directory(int parent, const char *name)
{
    struct stat st;
    int below = openat(parent, name, WALK_FLAGS);
    if (below < 0 && errno == ENOENT) {
        if (mkdirat(parent, name, 0777) < 0 && errno != EEXIST)
            return -1;
        below = openat(parent, name, WALK_FLAGS);
    }
    /* A link opened with O_DIRECTORY fails as no directory first. */
    if (below < 0 && errno == ENOTDIR && fstatat(parent, name, &st, AT_SYMLINK_NOFOLLOW) == 0 && S_ISLNK(st.st_mode))
        errno = ELOOP;
    return below;
}

/* Open the directory that holds TARGET beneath the chroot's ROOT, making
 * each directory on the way where it is missing.  Each component is opened
 * without following a link, so a target whose path crosses one, such as
 * the skeleton's /dev/fd or a link an earlier entry copied in, fails with
 * ELOOP where joining it to the root and following what is there would
 * lead out of the chroot; upstream joins and follows (LDG 402-433).
 * Answers the directory's descriptor and points *NAME at the target's last
 * component, in *COMPONENTS, which the caller frees, or at nothing when
 * TARGET is the root itself; or -1 with errno set. */
static int walk_to_parent(const char *root, const char *target, char **components, const char **name)
{
    char *cursor;
    int directory;
    *name = NULL;
    *components = joined("", target);
    if (*components == NULL)
        return -1;
    directory = open(root, WALK_FLAGS);
    cursor = *components;
    while (directory >= 0) {
        char *slash;
        while (*cursor == '/')
            cursor++;
        if (*cursor == '\0')
            break;
        slash = strchr(cursor, '/');
        if (slash != NULL)
            *slash = '\0';
        if (*name != NULL) {
            int below = enter_directory(directory, *name);
            int error = errno;
            close(directory);
            errno = error;
            directory = below;
            if (directory < 0)
                break;
        }
        *name = cursor;
        if (slash == NULL)
            break;
        cursor = slash + 1;
    }
    if (directory < 0) {
        int error = errno;
        free(*components);
        *components = NULL;
        errno = error;
    }
    return directory;
}

/* Make, in PARENT at NAME, the place for a bind of SOURCE, whose lstat is
 * ST: a directory or an empty file to mount on, opened without following
 * a link, or for a symlink source a copy of the link, which cannot be
 * bound (LDG 422-427).  NAME is nothing for the root itself, which only a
 * directory binds.  Answers a descriptor of what to mount on,
 * TARGET_IS_COPIED_LINK, or -1 with errno set. */
static int make_point(int parent, const char *name, const struct stat *st, const char *source)
{
    if (S_ISLNK(st->st_mode)) {
        char contents[PATH_MAX];
        ssize_t length = readlink(source, contents, sizeof contents - 1);
        if (length < 0)
            return -1;
        if (name == NULL) {
            errno = EEXIST;
            return -1;
        }
        contents[length] = '\0';
        return symlinkat(contents, parent, name) < 0 ? -1 : TARGET_IS_COPIED_LINK;
    }
    if (name == NULL) {
        if (S_ISDIR(st->st_mode))
            return fcntl(parent, F_DUPFD_CLOEXEC, 0);
        errno = EISDIR;
        return -1;
    }
    if (S_ISDIR(st->st_mode))
        return enter_directory(parent, name);
    return openat(parent, name, O_WRONLY | O_CREAT | O_TRUNC | O_NOFOLLOW | O_CLOEXEC, 0666);
}

/* Open the bind just made at NAME in PARENT, or on the root ROOT itself,
 * reached through the directory the walk found, not by a path joined
 * again; the lookup crosses into the new mount.  Answers a descriptor, or
 * -1 with errno set. */
static int open_bind(const char *root, int parent, const char *name)
{
    if (name == NULL)
        return open(root, WALK_FLAGS);
    return openat(parent, name, O_PATH | O_NOFOLLOW | O_CLOEXEC);
}

/* Bind SOURCE onto the mount point a descriptor names, through the
 * descriptor's /proc entry, so the path the walk found is the one bound
 * onto.  Answers 0, or an errno value. */
static int bind_onto(const char *source, int mount_point)
{
    char path[64];
    snprintf(path, sizeof path, "/proc/self/fd/%d", mount_point);
    return mount(source, path, "", MS_BIND | MS_REC, NULL) < 0 ? errno : 0;
}

/* Bind one plan entry into the chroot, as upstream's doBind (LDG 402-433),
 * and make it read-only when the plan says so.  Fails the process. */
static void bind_entry(const struct plan *plan, int status_fd, size_t index)
{
    const struct bind *bind = &plan->binds[index];
    const char *source = bind->source;
    char *resolved = NULL;
    char *components;
    const char *name;
    struct stat st;
    long at = (long)index;
    int parent;
    int mount_point;
    int error;

    if (bind->canonical) {
        resolved = realpath(source, NULL);
        if (resolved == NULL) {
            if (bind->optional && errno == ENOENT)
                return;
            fail(status_fd, "bind-source", errno, at);
        }
        source = resolved;
    }
    /* Upstream leaves /proc to the procfs it mounts (LDG 1874). */
    if (strcmp(source, "/proc") == 0) {
        free(resolved);
        return;
    }
    if (lstat(source, &st) < 0) {
        if (bind->optional && errno == ENOENT) {
            free(resolved);
            return;
        }
        fail(status_fd, "bind-source", errno, at);
    }
    parent = walk_to_parent(plan->root, bind->target, &components, &name);
    if (parent < 0)
        fail(status_fd, "bind-target", errno, at);
    mount_point = make_point(parent, name, &st, source);
    if (mount_point == -1)
        fail(status_fd, "bind-target", errno, at);
    if (mount_point != TARGET_IS_COPIED_LINK) {
        error = bind_onto(source, mount_point);
        close(mount_point);
        if (error != 0)
            fail(status_fd, "bind", error, at);
        if (bind->read_only) {
            int mounted = open_bind(plan->root, parent, name);
            error = mounted < 0 ? errno : make_read_only(mounted);
            if (mounted >= 0)
                close(mounted);
            if (error != 0)
                fail(status_fd, "remount", error, at);
        }
    }
    close(parent);
    free(components);
    free(resolved);
}

/* Whether the plan binds anything at TARGET in the chroot. */
static int plan_binds(const struct plan *plan, const char *target)
{
    size_t i;
    for (i = 0; i < plan->bind_count; i++)
        if (strcmp(plan->binds[i].target, target) == 0)
            return 1;
    return 0;
}

/* Bind a host device directory or node the devpts fallback needs
 * (LDG 1926-1927), as doBind binds it: writable, the one host path the
 * sandbox binds so, since a terminal is written through it, and only where
 * the kernel will not mount a devpts of the sandbox's own. */
static void bind_host(const struct plan *plan, int status_fd, const char *source, const char *stage)
{
    struct stat st;
    char *components;
    const char *name;
    int parent;
    int mount_point;
    int error = 0;
    if (lstat(source, &st) < 0)
        fail(status_fd, stage, errno, NO_INDEX);
    parent = walk_to_parent(plan->root, source, &components, &name);
    if (parent < 0)
        fail(status_fd, stage, errno, NO_INDEX);
    mount_point = make_point(parent, name, &st, source);
    if (mount_point == -1)
        fail(status_fd, stage, errno, NO_INDEX);
    /* A host node that is a link is copied, as upstream's doBind copies
     * one (LDG 411-433), and nothing is left to bind. */
    if (mount_point != TARGET_IS_COPIED_LINK) {
        error = bind_onto(source, mount_point);
        close(mount_point);
    }
    close(parent);
    free(components);
    if (error != 0)
        fail(status_fd, stage, error, NO_INDEX);
}

/* /proc, /dev/shm and /dev/pts of the sandbox's own (LDG 1890-1929). */
static void mount_filesystems(const struct plan *plan, int status_fd)
{
    char *proc = joined(plan->root, "/proc");
    char *shm = joined(plan->root, "/dev/shm");
    char *pts = joined(plan->root, "/dev/pts");
    char *ptmx = joined(plan->root, "/dev/ptmx");
    char *pts_ptmx = joined(plan->root, "/dev/pts/ptmx");
    if (proc == NULL || shm == NULL || pts == NULL || ptmx == NULL || pts_ptmx == NULL)
        fail(status_fd, "proc", ENOMEM, NO_INDEX);

    if (create_dirs(proc) < 0 || mount("none", proc, "proc", 0, NULL) < 0)
        fail(status_fd, "proc", errno, NO_INDEX);

    if (path_exists("/dev/shm")) {
        char *options = joined("size=", plan->dev_shm_size);
        if (options == NULL || mount("none", shm, "tmpfs", 0, options) < 0)
            fail(status_fd, "dev-shm", errno, NO_INDEX);
        free(options);
    }

    if (path_exists("/dev/pts/ptmx") && !path_exists(ptmx) && !plan_binds(plan, "/dev/pts")) {
        if (mount("none", pts, "devpts", 0, devpts_options) == 0) {
            if (symlink("/dev/pts/ptmx", ptmx) < 0)
                fail(status_fd, "devpts", errno, NO_INDEX);
            /* With some kernels ptmx is created with mode 0 (LDG 1920-1922). */
            if (chmod(pts_ptmx, 0666) < 0)
                fail(status_fd, "devpts", errno, NO_INDEX);
        } else {
            if (errno != EINVAL)
                fail(status_fd, "devpts", errno, NO_INDEX);
            bind_host(plan, status_fd, "/dev/pts", "devpts");
            bind_host(plan, status_fd, "/dev/ptmx", "devpts");
        }
    }
    free(proc);
    free(shm);
    free(pts);
    free(ptmx);
    free(pts_ptmx);
}

/* ------------------------------------------------------------------------
 * The child: PID 1 of the new namespaces
 * ------------------------------------------------------------------------ */

/* Bring the private network's loopback interface up (LDG 1767-1778). */
static void loopback_up(int status_fd)
{
    struct ifreq request;
    int fd = socket(PF_INET, SOCK_DGRAM | SOCK_CLOEXEC, IPPROTO_IP);
    if (fd < 0)
        fail(status_fd, "loopback-socket", errno, NO_INDEX);
    memset(&request, 0, sizeof request);
    memcpy(request.ifr_name, "lo", sizeof "lo");
    request.ifr_flags = IFF_UP | IFF_LOOPBACK | IFF_RUNNING;
    if (ioctl(fd, SIOCSIFFLAGS, &request) < 0)
        fail(status_fd, "loopback", errno, NO_INDEX);
    close(fd);
}

/* Close every descriptor from 3 up but the status pipe, which closes when
 * the builder starts (LDG 1989). */
static void close_descriptors(int keep)
{
    DIR *dir = opendir("/proc/self/fd");
    if (dir != NULL) {
        int own = dirfd(dir);
        int fds[1024];
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
        int top = 1024;
        if (getrlimit(RLIMIT_NOFILE, &limit) == 0 && limit.rlim_cur != RLIM_INFINITY && limit.rlim_cur < (rlim_t)INT_MAX)
            top = (int)limit.rlim_cur;
        for (fd = 3; fd < top; fd++)
            if (fd != keep)
                close(fd);
    }
}

/* Die with the helper, as upstream's child dies with its parent
 * (processes.cc:206).  A credential change can clear the setting, so it is
 * made again once the ids are final. */
static void die_with_helper(int status_fd)
{
    if (prctl(PR_SET_PDEATHSIG, SIGKILL) < 0)
        fail(status_fd, "pdeathsig", errno, NO_INDEX);
}

/* clone(2) as fork makes a child, with no stack of its own: without
 * CLONE_VM the child runs on its copy of this process's stack.  Upstream
 * clones onto a stack it maps (processes.cc:223-236), a switch the
 * address sanitizer cannot follow (google/sanitizers issue 189), so its
 * reports from such a child may be false; on its copy of a stack the
 * sanitizer knows, the child's are true.  The raw system call, as
 * bubblewrap makes it, since glibc's clone takes a stack; the arguments
 * after the stack are read only for flags this file does not pass.  The
 * child gets 0, and runs single-threaded, as this process does. */
static pid_t raw_clone(int flags)
{
#if defined(__s390__) || defined(__CRIS__)
    return (pid_t)syscall(SYS_clone, (void *)NULL, (unsigned long)flags);
#else
    return (pid_t)syscall(SYS_clone, (unsigned long)flags, (void *)NULL);
#endif
}

static int child_main(const struct child_context *context)
{
    const struct plan *plan = context->plan;
    int status_fd = context->status_fd;
    char release = '\0';
    ssize_t got;
    size_t i;
    char *etc;
    struct rlimit core;

    /* Before the release, so a helper that has already died is seen as the
     * end of the sync pipe, and one that dies later kills this process. */
    die_with_helper(status_fd);

    /* Wait until the helper has mapped this namespace's ids; end of file
     * means it failed and has reported why (LDG 1760-1765). */
    close(context->sync_write);
    do {
        got = read(context->sync_read, &release, 1);
    } while (got < 0 && errno == EINTR);
    if (got != 1 || release != RELEASE_BYTE)
        _exit(EXIT_FAILURE);
    close(context->sync_read);

    /* No controlling terminal, and no terminal signals (child.cc:17-22). */
    if (setsid() < 0)
        fail(status_fd, "setsid", errno, NO_INDEX);

    if (plan->private_network)
        loopback_up(status_fd);

    if (sethostname(sandbox_hostname, sizeof sandbox_hostname) < 0)
        fail(status_fd, "hostname", errno, NO_INDEX);
    if (setdomainname(sandbox_domainname, sizeof sandbox_domainname) < 0)
        fail(status_fd, "domainname", errno, NO_INDEX);

    /* Mounts made below must not propagate out (LDG 1788-1797). */
    if (mount(NULL, "/", NULL, MS_PRIVATE | MS_REC, NULL) < 0)
        fail(status_fd, "private", errno, NO_INDEX);

    /* pivot_root needs the new root to be a mount of its own (LDG 1799-1802). */
    if (mount(plan->root, plan->root, NULL, MS_BIND, NULL) < 0)
        fail(status_fd, "bind-root", errno, NO_INDEX);

    for (i = 0; i < plan->bind_count; i++)
        bind_entry(plan, status_fd, i);

    mount_filesystems(plan, status_fd);

    /* Upstream's mode for /etc once its files are in place (LDG 1931-1933).
     * Upstream's /etc belongs to root (LDG 721-723); this one belongs to
     * the user who made the chroot, as whom the builder runs, so the
     * builder can make it writable again.  It is the build's own copy,
     * removed with the chroot, and its host files are read-only binds. */
    etc = joined(plan->root, "/etc");
    if (etc == NULL || chmod(etc, 0555) < 0)
        fail(status_fd, "etc", errno, NO_INDEX);
    free(etc);

    /* Make the chroot the root and detach the old one (LDG 1954-1971). */
    if (chdir(plan->root) < 0)
        fail(status_fd, "chdir-root", errno, NO_INDEX);
    if (mkdir("real-root", 0) < 0)
        fail(status_fd, "real-root", errno, NO_INDEX);
    if (syscall(SYS_pivot_root, ".", "real-root") < 0)
        fail(status_fd, "pivot", errno, NO_INDEX);
    if (chroot(".") < 0)
        fail(status_fd, "chroot", errno, NO_INDEX);
    if (umount2("real-root", MNT_DETACH) < 0)
        fail(status_fd, "umount", errno, NO_INDEX);
    if (rmdir("real-root") < 0)
        fail(status_fd, "rmdir", errno, NO_INDEX);

    /* The sandbox's ids, which map to the invoking user (LDG 1973-1979). */
    if (setgid((gid_t)plan->sandbox_gid) < 0)
        fail(status_fd, "setgid", errno, NO_INDEX);
    if (setuid((uid_t)plan->sandbox_uid) < 0)
        fail(status_fd, "setuid", errno, NO_INDEX);
    die_with_helper(status_fd);

    if (chdir(plan->build_dir) < 0)
        fail(status_fd, "chdir", errno, NO_INDEX);

    close_descriptors(status_fd);

    /* A session keyring of the builder's own, anonymous and empty, which
     * upstream does not make: its builder runs as a build user, with no
     * keys of the invoking user's.  This one runs as that user, and would
     * otherwise inherit the user's session keyring through the
     * namespaces, and with it possession of every key linked from it.
     * The join stops that inheritance and nothing more (security/keys at
     * Linux 6.8): /proc/keys lists the keys of every uid the namespace
     * maps, the builder's own among them (proc.c:60-75); the user keyring
     * gives its owner's uid every permission (process_keys.c:86); and a
     * key's owner gets those permissions whatever namespace asks
     * (permission.c:52-55).  So the builder can link the user keyring into
     * its own (keyctl.c:515-532), possess it, and read what it holds.
     * Only a filter refusing keyctl, add_key and request_key closes that.
     * A kernel without keyrings has none to inherit, and a filter that
     * refuses the join refuses the builder's keyctl too. */
    if (syscall(SYS_keyctl, KEYCTL_JOIN_SESSION_KEYRING, (char *)NULL) < 0 && errno != ENOSYS && errno != EPERM)
        fail(status_fd, "keyring", errno, NO_INDEX);

    /* No privileges gained through the exec, from a setuid program or file
     * capabilities a sandbox-paths entry makes reachable, as upstream's
     * builder gains none unless allow-new-privileges is set (globals.hh:
     * 1076-1090, LDG 1718-1719). */
    if (prctl(PR_SET_NO_NEW_PRIVS, 1, 0, 0, 0) < 0)
        fail(status_fd, "no-new-privs", errno, NO_INDEX);

    /* No core dumps (LDG 1995-1997).  Upstream also raises the hard limit
     * to infinity, which an unprivileged process may not; the hard limit is
     * left as it is. */
    if (getrlimit(RLIMIT_CORE, &core) == 0) {
        core.rlim_cur = 0;
        setrlimit(RLIMIT_CORE, &core);
    }

    sigprocmask(SIG_SETMASK, &context->original_mask, NULL);
    execve(context->argv[0], context->argv, plan->environment);
    fail(status_fd, "exec", errno, NO_INDEX);
    return EXIT_FAILURE;
}

/* ------------------------------------------------------------------------
 * The helper: outside the new namespaces
 * ------------------------------------------------------------------------ */

/* Write a namespace's id map from outside it (LDG 994-1009).  Answers 0,
 * or an errno value. */
static int write_proc_file(pid_t pid, const char *name, const char *contents)
{
    char path[64];
    int fd;
    size_t length = strlen(contents);
    ssize_t written;
    snprintf(path, sizeof path, "/proc/%ld/%s", (long)pid, name);
    fd = open(path, O_WRONLY | O_CLOEXEC);
    if (fd < 0)
        return errno;
    written = write(fd, contents, length);
    if (written < 0 || (size_t)written != length) {
        int error = written < 0 ? errno : EIO;
        close(fd);
        return error;
    }
    close(fd);
    return 0;
}

/* Report a failure of the helper's own, after the child is made.  The
 * child's PID namespace ends with it. */
static void fail_after_clone(int status_fd, pid_t child, const char *stage, int error)
{
    int wait_status;
    kill(child, SIGKILL);
    while (waitpid(child, &wait_status, 0) < 0 && errno == EINTR)
        ;
    report(status_fd, stage, error, (long)child);
    _exit(EXIT_FAILURE);
}

/* Put SIGCHLD back to its default, as upstream's initNix does
 * (shared.cc:130-137 at 2.24.9): a SIGCHLD ignored where this program was
 * started would have the kernel reap the builder unasked, leaving nothing
 * to wait for.  Answers 0, or an errno value. */
static int reset_sigchld(void)
{
    struct sigaction action;
    memset(&action, 0, sizeof action);
    sigemptyset(&action.sa_mask);
    action.sa_handler = SIG_DFL;
    return sigaction(SIGCHLD, &action, NULL) < 0 ? errno : 0;
}

/* Reap the builder, blocking.  Answers 0, or the errno of a wait that
 * failed. */
static int reap_builder(pid_t child, int *wait_status)
{
    for (;;) {
        if (waitpid(child, wait_status, 0) == child)
            return 0;
        if (errno != EINTR)
            return errno;
    }
}

/* Wait for the builder, killing it on a terminating signal or when the
 * lifeline closes.  Answers 0 with how it ended in *WAIT_STATUS, or the
 * errno of a wait that failed, which ends the build: the builder is
 * killed, and how it ended cannot be known.  The kernel ends the builder's
 * PID namespace with the builder, PID 1 there, so once it is reaped no
 * process of the build is left. */
static int wait_for_builder(pid_t child, int signals_fd, int lifeline_fd, int *wait_status)
{
    for (;;) {
        struct pollfd watched[2];
        pid_t reaped;
        watched[0].fd = signals_fd;
        watched[0].events = POLLIN;
        watched[0].revents = 0;
        watched[1].fd = lifeline_fd;
        watched[1].events = POLLIN;
        watched[1].revents = 0;
        if (poll(watched, 2, -1) < 0) {
            if (errno == EINTR)
                continue;
            /* With nothing left to watch, the builder is ended and reaped. */
            kill(child, SIGKILL);
            return reap_builder(child, wait_status);
        }
        if (watched[1].revents != 0) {
            char discard[64];
            ssize_t got = read(lifeline_fd, discard, sizeof discard);
            if (got <= 0 && !(got < 0 && errno == EINTR)) {
                kill(child, SIGKILL);
                close(lifeline_fd);
                lifeline_fd = -1;
            }
        }
        if (watched[0].revents & POLLIN) {
            struct signalfd_siginfo info;
            ssize_t got = read(signals_fd, &info, sizeof info);
            if (got == (ssize_t)sizeof info && info.ssi_signo != SIGCHLD)
                kill(child, SIGKILL);
        }
        reaped = waitpid(child, wait_status, WNOHANG);
        if (reaped == child)
            return 0;
        if (reaped < 0 && errno != EINTR) {
            int error = errno;
            kill(child, SIGKILL);
            return error;
        }
    }
}

/* End this process as the builder ended: with its exit status, or by the
 * signal that killed it, with no core dump of this process's own. */
static int relay(int wait_status)
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

static int run_builder(char **argv)
{
    struct plan plan;
    struct child_context context;
    char *plan_bytes = NULL;
    size_t plan_length = 0;
    int status_fd;
    int lifeline_fd;
    int signals_fd;
    int null_fd;
    int sync_pipe[2];
    int error;
    int flags;
    pid_t child;
    char map[64];
    sigset_t handled;
    int wait_status = 0;

    /* The status pipe moves to a descriptor the builder's exec closes, and
     * standard output becomes the log, as upstream's child gets standard
     * error on both (child.cc:24-26). */
    status_fd = fcntl(STDOUT_FILENO, F_DUPFD_CLOEXEC, 3);
    if (status_fd < 0)
        return EXIT_FAILURE;
    if (dup2(STDERR_FILENO, STDOUT_FILENO) < 0)
        fail(status_fd, "stdout", errno, NO_INDEX);

    if ((error = reset_sigchld()) != 0)
        fail(status_fd, "sigchld", error, NO_INDEX);

    error = read_plan_bytes(&plan_bytes, &plan_length);
    if (error == 0)
        error = plan_parse(plan_bytes, plan_length, &plan);
    if (error != 0)
        fail(status_fd, "plan", error, NO_INDEX);

    /* Standard input, past the plan, is the lifeline; the builder reads
     * /dev/null (child.cc:28-34). */
    lifeline_fd = fcntl(STDIN_FILENO, F_DUPFD_CLOEXEC, 3);
    null_fd = open("/dev/null", O_RDWR | O_CLOEXEC);
    if (lifeline_fd < 0 || null_fd < 0 || dup2(null_fd, STDIN_FILENO) < 0)
        fail(status_fd, "stdin", errno, NO_INDEX);
    close(null_fd);

    /* Supplementary groups cannot be dropped inside the new user namespace
     * (LDG 955-962). */
    if (setgroups(0, NULL) < 0) {
        if (errno != EPERM || plan.require_drop_groups)
            fail(status_fd, "setgroups", errno, NO_INDEX);
    }

    if (pipe2(sync_pipe, O_CLOEXEC) < 0)
        fail(status_fd, "sync", errno, NO_INDEX);

    /* The signals the helper answers are blocked from before the builder
     * exists, and read through a descriptor once it does, so none is lost
     * and none interrupts the setup; the builder unblocks them before it
     * execs. */
    sigemptyset(&handled);
    sigaddset(&handled, SIGTERM);
    sigaddset(&handled, SIGINT);
    sigaddset(&handled, SIGHUP);
    sigaddset(&handled, SIGCHLD);
    if (sigprocmask(SIG_BLOCK, &handled, &context.original_mask) < 0)
        fail(status_fd, "signals", errno, NO_INDEX);

    context.plan = &plan;
    context.argv = argv;
    context.status_fd = status_fd;
    context.sync_read = sync_pipe[0];
    context.sync_write = sync_pipe[1];

    /* No CLONE_PARENT, so the builder stays this process's child and its
     * exit is this process's to relay (processes.cc:223-236, LDG 965-969). */
    flags = CLONE_NEWUSER | CLONE_NEWPID | CLONE_NEWNS | CLONE_NEWIPC | CLONE_NEWUTS | SIGCHLD;
    if (plan.private_network)
        flags |= CLONE_NEWNET;
    child = raw_clone(flags);
    if (child < 0)
        fail(status_fd, "clone", errno, NO_INDEX);
    if (child == 0)
        _exit(child_main(&context));
    close(sync_pipe[0]);

    signals_fd = signalfd(-1, &handled, SFD_CLOEXEC);
    if (signals_fd < 0)
        fail_after_clone(status_fd, child, "signals", errno);

    /* The sandbox's ids map to the invoking user's (LDG 994-1009). */
    snprintf(map, sizeof map, "%lu %lu 1", (unsigned long)plan.sandbox_uid, (unsigned long)getuid());
    if ((error = write_proc_file(child, "uid_map", map)) != 0)
        fail_after_clone(status_fd, child, "uid-map", error);
    if ((error = write_proc_file(child, "setgroups", "deny")) != 0)
        fail_after_clone(status_fd, child, "setgroups-deny", error);
    snprintf(map, sizeof map, "%lu %lu 1", (unsigned long)plan.sandbox_gid, (unsigned long)getgid());
    if ((error = write_proc_file(child, "gid_map", map)) != 0)
        fail_after_clone(status_fd, child, "gid-map", error);

    {
        char release = RELEASE_BYTE;
        ssize_t written;
        do {
            written = write(sync_pipe[1], &release, 1);
        } while (written < 0 && errno == EINTR);
        if (written != 1)
            fail_after_clone(status_fd, child, "release", written < 0 ? errno : EIO);
    }
    close(sync_pipe[1]);
    close(status_fd);
    /* The child has its own copy of the plan. */
    plan_release(&plan);
    free(plan_bytes);

    error = wait_for_builder(child, signals_fd, lifeline_fd, &wait_status);
    close(signals_fd);
    if (error != 0) {
        /* The status pipe is closed, so the reason goes to the build log,
         * in upstream's words for a wait that fails (processes.cc:98). */
        fprintf(stderr, "nova-nix-sandbox: cannot get exit status of PID %ld: %s\n", (long)child, strerror(error));
        return EXIT_FAILURE;
    }
    return relay(wait_status);
}

/* ------------------------------------------------------------------------
 * The probe
 * ------------------------------------------------------------------------ */

/* Compare a sysctl's contents, its trailing newlines and spaces trimmed as
 * upstream trims them (namespaces.cc:25-39), with EXPECTED: 1 when they
 * match, 0 when they do not, and -1 when the file cannot be read. */
static int sysctl_is(const char *path, const char *expected)
{
    char contents[32];
    ssize_t length;
    int fd = open(path, O_RDONLY | O_CLOEXEC);
    if (fd < 0)
        return -1;
    length = read(fd, contents, sizeof contents - 1);
    close(fd);
    if (length < 0)
        return -1;
    while (length > 0 && (contents[length - 1] == '\n' || contents[length - 1] == ' '))
        length--;
    contents[length] = '\0';
    return strcmp(contents, expected) == 0;
}

static int probe_exit(void)
{
    return EXIT_SUCCESS;
}

static int probe_mounts(void)
{
    /* So the remount below does not touch the parent's /proc. */
    if (mount(NULL, "/", NULL, MS_PRIVATE | MS_REC, NULL) < 0)
        return PROBE_PRIVATE_FAILED;
    if (mount("none", "/proc", "proc", 0, NULL) < 0)
        return PROBE_PROC_FAILED;
    return EXIT_SUCCESS;
}

/* Clone a child into the namespaces and wait for it.  Answers the child's
 * exit status, or -1 with errno set when it could not be made. */
static int probe_child(int (*entry)(void), int flags)
{
    int wait_status;
    pid_t child = raw_clone(flags | SIGCHLD);
    if (child < 0)
        return -1;
    if (child == 0)
        _exit(entry());
    while (waitpid(child, &wait_status, 0) < 0)
        if (errno != EINTR)
            return -1;
    return WIFEXITED(wait_status) ? WEXITSTATUS(wait_status) : EXIT_FAILURE;
}

/* The namespace checks.  Answers the stage of the check that failed and
 * its errno, or NULL when none did. */
static const char *probe_namespaces(int *error)
{
    int outcome = probe_child(probe_exit, CLONE_NEWUSER);
    *error = 0;
    if (outcome != 0) {
        *error = outcome < 0 ? errno : 0;
        return "userns";
    }
    outcome = probe_child(probe_mounts, CLONE_NEWNS | CLONE_NEWPID | CLONE_NEWUSER);
    if (outcome < 0) {
        *error = errno;
        return "mountns";
    }
    if (outcome != 0) {
        /* Ubuntu's AppArmor lets an unprivileged process make a user
         * namespace but denies it capabilities there, so the mounts fail;
         * naming the restriction says what to change. */
        if (sysctl_is("/proc/sys/kernel/apparmor_restrict_unprivileged_userns", "1") == 1)
            return "apparmor";
        return outcome == PROBE_PROC_FAILED ? "proc-remount" : "mountns";
    }
    return NULL;
}

static int probe(void)
{
    int status_fd = STDOUT_FILENO;
    int error;
    const char *failed;

    if ((error = reset_sigchld()) != 0) {
        report(status_fd, "sigchld", error, NO_INDEX);
        return EXIT_FAILURE;
    }
    if (!path_exists("/proc/self/ns/user")) {
        report(status_fd, "userns-missing", 0, NO_INDEX);
        return EXIT_FAILURE;
    }
    if (sysctl_is("/proc/sys/user/max_user_namespaces", "0") != 0) {
        report(status_fd, "userns-max", 0, NO_INDEX);
        return EXIT_FAILURE;
    }
    if (sysctl_is("/proc/sys/kernel/unprivileged_userns_clone", "0") == 1) {
        report(status_fd, "userns-clone", 0, NO_INDEX);
        return EXIT_FAILURE;
    }

    failed = probe_namespaces(&error);
    if (failed != NULL) {
        report(status_fd, failed, error, NO_INDEX);
        return EXIT_FAILURE;
    }
    return EXIT_SUCCESS;
}

int main(int argc, char **argv)
{
    if (argc >= 3 && strcmp(argv[1], "run") == 0)
        return run_builder(argv + 2);
    if (argc == 2 && strcmp(argv[1], "probe") == 0)
        return probe();
    fprintf(stderr, "usage: %s run PROGRAM [ARG...] | %s probe\n", argv[0], argv[0]);
    return EXIT_FAILURE;
}
