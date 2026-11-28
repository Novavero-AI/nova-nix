/*
 * The plan nova-nix-sandbox carries out, as Nix.Builder.Sandbox renders it,
 * parsed without IO.  It is portable C, so the test suite links it too and
 * checks it against the plans the renderer writes.
 *
 * After the header the plan holds tokens, each its length in decimal, a
 * colon, that many bytes and a NUL, so a token's bytes cannot end it early
 * or start another.  The tokens are keys and their values in one fixed
 * shape.  The first is backend, naming the backend the plan is for; the
 * rest are that backend's:
 *
 *   linux: root, build-dir, dev-shm-size, sandbox-uid, sandbox-gid,
 *   network and drop-groups, each once and in that order, then any number
 *   of binds;
 *
 *   darwin: build-dir and profile, each once and in that order, then any
 *   number of params, each a name and its value;
 *
 *   windows: build-dir, token, slot-sid, desktop and memory-cap, each
 *   once and in that order;
 *
 * then, for any of them, any number of environment entries.
 */

#ifndef NOVA_NIX_SANDBOX_PLAN_H
#define NOVA_NIX_SANDBOX_PLAN_H

#include <stddef.h>

/* The first line of every plan; a plan of another version is refused. */
#define PLAN_VERSION_LINE "nova-nix-sandbox-plan 4\n"

/* The backend a plan is for, named as the OS that backend runs on. */
enum plan_backend {
    PLAN_LINUX,
    PLAN_DARWIN,
    PLAN_WINDOWS
};

struct bind {
    const char *target;
    const char *source;
    int read_only;
    int optional;
    int canonical;
};

struct plan {
    enum plan_backend backend;
    const char *build_dir;

    /* Linux's. */
    const char *root;
    const char *dev_shm_size;
    unsigned long sandbox_uid;
    unsigned long sandbox_gid;
    int private_network;
    int require_drop_groups;
    struct bind *binds;
    size_t bind_count;

    /* Darwin's: the profile's text, and its parameters as
     * sandbox_init_with_parameters takes them, each name followed by its
     * value, the whole NULL-terminated. */
    const char *profile;
    const char **parameters;
    size_t parameter_count;

    /* Windows's: the value of the inherited handle to the primary token
     * of the build slot's account, that account's SID in its S-1-...
     * form, the window station and desktop to make, as STATION\DESKTOP,
     * and the most memory, in bytes, each process of the build may
     * commit. */
    unsigned long token_handle;
    const char *slot_sid;
    const char *desktop;
    unsigned long long memory_cap;

    /* NAME=VALUE entries, NULL-terminated, for the builder's execve, or
     * on Windows its environment block. */
    char **environment;
    size_t environment_count;
};

/* Parse the LENGTH bytes of tokens at BYTES into PLAN, whose strings point
 * into BYTES.  Answers 0, ENOMEM, or EINVAL for tokens of any other shape,
 * a token holding a NUL among them, since each is used as a C string.  On
 * failure nothing is left to release. */
int plan_parse(char *bytes, size_t length, struct plan *plan);

/* Release what a parsed plan holds, apart from the bytes it points into. */
void plan_release(struct plan *plan);

#endif
