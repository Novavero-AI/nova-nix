/*
 * The plan nova-nix-sandbox carries out, as Nix.Builder.Sandbox renders it,
 * parsed without IO.  It is portable C, so the test suite links it too and
 * checks it against the plans the renderer writes.
 *
 * After the header the plan holds tokens, each its length in decimal, a
 * colon, that many bytes and a NUL, so a token's bytes cannot end it early
 * or start another.  The tokens are keys and their values in one fixed
 * shape: root, build-dir, dev-shm-size, sandbox-uid, sandbox-gid, network
 * and drop-groups, each once and in that order, then any number of binds,
 * then any number of environment entries.
 */

#ifndef NOVA_NIX_SANDBOX_PLAN_H
#define NOVA_NIX_SANDBOX_PLAN_H

#include <stddef.h>

/* The first line of every plan; a plan of another version is refused. */
#define PLAN_VERSION_LINE "nova-nix-sandbox-plan 3\n"

struct bind {
    const char *target;
    const char *source;
    int read_only;
    int optional;
    int canonical;
};

struct plan {
    const char *root;
    const char *build_dir;
    const char *dev_shm_size;
    unsigned long sandbox_uid;
    unsigned long sandbox_gid;
    int private_network;
    int require_drop_groups;
    struct bind *binds;
    size_t bind_count;
    /* NAME=VALUE entries, NULL-terminated, for the builder's execve. */
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
