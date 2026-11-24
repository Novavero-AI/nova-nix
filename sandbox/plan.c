/*
 * The sandbox helper's plan parser: plan.h says what a plan holds.
 */

#include "plan.h"

#include <errno.h>
#include <limits.h>
#include <stdlib.h>
#include <string.h>

/* A cursor over a plan's tokens. */
struct tokens {
    char *next;
    char *end;
};

/* What reading a token came to. */
enum token_read {
    TOKEN_FOUND,
    TOKEN_END,
    TOKEN_MALFORMED
};

/* Read the next token: its decimal length, with no leading zero, a colon,
 * that many bytes holding no NUL, then a NUL. */
static enum token_read read_token(struct tokens *cursor, char **token)
{
    char *at = cursor->next;
    size_t remaining = (size_t)(cursor->end - at);
    size_t length = 0;
    size_t digits = 0;
    char *bytes;

    if (at >= cursor->end)
        return TOKEN_END;
    while (at < cursor->end && *at >= '0' && *at <= '9') {
        size_t digit = (size_t)(*at - '0');
        if (length > remaining / 10)
            return TOKEN_MALFORMED;
        length = length * 10 + digit;
        if (length > remaining)
            return TOKEN_MALFORMED;
        digits++;
        at++;
    }
    if (digits == 0 || (digits > 1 && cursor->next[0] == '0'))
        return TOKEN_MALFORMED;
    if (at >= cursor->end || *at != ':')
        return TOKEN_MALFORMED;
    bytes = at + 1;
    if ((size_t)(cursor->end - bytes) <= length || bytes[length] != '\0')
        return TOKEN_MALFORMED;
    if (memchr(bytes, '\0', length) != NULL)
        return TOKEN_MALFORMED;
    cursor->next = bytes + length + 1;
    *token = bytes;
    return TOKEN_FOUND;
}

/* Read KEY's token, then its value's. */
static int read_value(struct tokens *cursor, const char *key, char **value)
{
    char *found;
    return read_token(cursor, &found) == TOKEN_FOUND && strcmp(found, key) == 0
        && read_token(cursor, value) == TOKEN_FOUND;
}

/* Pass over COUNT tokens. */
static int skip_tokens(struct tokens *cursor, size_t count)
{
    char *token;
    size_t i;
    for (i = 0; i < count; i++)
        if (read_token(cursor, &token) != TOKEN_FOUND)
            return 0;
    return 1;
}

/* A decimal id that fits the int a uid_t or gid_t is given as. */
static int parse_id(const char *text, unsigned long *out)
{
    char *rest = NULL;
    unsigned long value;
    if (*text < '0' || *text > '9')
        return 0;
    errno = 0;
    value = strtoul(text, &rest, 10);
    if (errno != 0 || rest == NULL || *rest != '\0' || value > (unsigned long)INT_MAX)
        return 0;
    *out = value;
    return 1;
}

/* A bind's flags: "-" for none, else letters r (read-only), o (optional)
 * and c (resolve the source's symlinks first). */
static int parse_bind_flags(const char *text, struct bind *bind)
{
    const char *letter;
    if (strcmp(text, "-") == 0)
        return 1;
    if (*text == '\0')
        return 0;
    for (letter = text; *letter != '\0'; letter++) {
        switch (*letter) {
        case 'r': bind->read_only = 1; break;
        case 'o': bind->optional = 1; break;
        case 'c': bind->canonical = 1; break;
        default: return 0;
        }
    }
    return 1;
}

/* Choose between two words a value may be: 1 for YES, 0 for NO. */
static int parse_choice(const char *value, const char *yes, const char *no, int *out)
{
    if (strcmp(value, yes) == 0)
        *out = 1;
    else if (strcmp(value, no) == 0)
        *out = 0;
    else
        return 0;
    return 1;
}

static int parse_linux_singletons(struct tokens *cursor, struct plan *plan)
{
    char *value;
    if (!read_value(cursor, "root", &value) || value[0] != '/')
        return 0;
    plan->root = value;
    if (!read_value(cursor, "build-dir", &value) || value[0] != '/')
        return 0;
    plan->build_dir = value;
    if (!read_value(cursor, "dev-shm-size", &value))
        return 0;
    plan->dev_shm_size = value;
    if (!read_value(cursor, "sandbox-uid", &value) || !parse_id(value, &plan->sandbox_uid))
        return 0;
    if (!read_value(cursor, "sandbox-gid", &value) || !parse_id(value, &plan->sandbox_gid))
        return 0;
    if (!read_value(cursor, "network", &value) || !parse_choice(value, "private", "host", &plan->private_network))
        return 0;
    return read_value(cursor, "drop-groups", &value) && parse_choice(value, "require", "tolerate", &plan->require_drop_groups);
}

static int parse_darwin_singletons(struct tokens *cursor, struct plan *plan)
{
    char *value;
    if (!read_value(cursor, "build-dir", &value) || value[0] != '/')
        return 0;
    plan->build_dir = value;
    if (!read_value(cursor, "profile", &value))
        return 0;
    plan->profile = value;
    return 1;
}

static int parse_singletons(struct tokens *cursor, struct plan *plan)
{
    char *value;
    if (!read_value(cursor, "backend", &value))
        return 0;
    if (strcmp(value, "linux") == 0) {
        plan->backend = PLAN_LINUX;
        return parse_linux_singletons(cursor, plan);
    }
    if (strcmp(value, "darwin") == 0) {
        plan->backend = PLAN_DARWIN;
        return parse_darwin_singletons(cursor, plan);
    }
    return 0;
}

/* The entries a backend's plan repeats before the environment: binds on
 * Linux, each three tokens after its key, and params on Darwin, each two. */
struct section {
    const char *key;
    size_t width;
};

static struct section backend_section(enum plan_backend backend)
{
    struct section section;
    if (backend == PLAN_LINUX) {
        section.key = "bind";
        section.width = 3;
    } else {
        section.key = "param";
        section.width = 2;
    }
    return section;
}

/* Count the backend's entries and the environment's after the singletons,
 * checking that nothing but those follows. */
static int count_sections(struct tokens cursor, struct section section, size_t *repeated, size_t *entries)
{
    char *key = NULL;
    enum token_read read = read_token(&cursor, &key);
    *repeated = 0;
    *entries = 0;
    while (read == TOKEN_FOUND && strcmp(key, section.key) == 0) {
        if (!skip_tokens(&cursor, section.width))
            return 0;
        (*repeated)++;
        read = read_token(&cursor, &key);
    }
    while (read == TOKEN_FOUND && strcmp(key, "env") == 0) {
        if (!skip_tokens(&cursor, 1))
            return 0;
        (*entries)++;
        read = read_token(&cursor, &key);
    }
    return read == TOKEN_END;
}

static int parse_bind(struct tokens *cursor, struct bind *bind)
{
    char *target;
    char *source;
    char *flags;
    if (!read_value(cursor, "bind", &target) || read_token(cursor, &source) != TOKEN_FOUND
        || read_token(cursor, &flags) != TOKEN_FOUND)
        return 0;
    if (target[0] != '/' || source[0] != '/')
        return 0;
    bind->target = target;
    bind->source = source;
    return parse_bind_flags(flags, bind);
}

/* A param's name, which is not empty, and its value. */
static int parse_parameter(struct tokens *cursor, const char **name, const char **value)
{
    char *read_name;
    char *read_value_token;
    if (!read_value(cursor, "param", &read_name) || read_token(cursor, &read_value_token) != TOKEN_FOUND
        || read_name[0] == '\0')
        return 0;
    *name = read_name;
    *value = read_value_token;
    return 1;
}

/* Allocate what the counted entries are parsed into. */
static int allocate_sections(struct plan *plan, size_t repeated, size_t entries)
{
    if (plan->backend == PLAN_LINUX && repeated > 0) {
        plan->binds = calloc(repeated, sizeof *plan->binds);
        if (plan->binds == NULL)
            return 0;
    }
    if (plan->backend == PLAN_DARWIN) {
        plan->parameters = calloc(2 * repeated + 1, sizeof *plan->parameters);
        if (plan->parameters == NULL)
            return 0;
    }
    plan->environment = calloc(entries + 1, sizeof *plan->environment);
    return plan->environment != NULL;
}

static int parse_repeated(struct tokens *cursor, struct plan *plan, size_t repeated)
{
    size_t i;
    for (i = 0; i < repeated; i++) {
        if (plan->backend == PLAN_LINUX) {
            if (!parse_bind(cursor, &plan->binds[i]))
                return 0;
            plan->bind_count++;
        } else {
            if (!parse_parameter(cursor, &plan->parameters[2 * i], &plan->parameters[2 * i + 1]))
                return 0;
            plan->parameter_count++;
        }
    }
    return 1;
}

int plan_parse(char *bytes, size_t length, struct plan *plan)
{
    struct tokens cursor;
    size_t repeated;
    size_t entries;
    size_t i;

    memset(plan, 0, sizeof *plan);
    cursor.next = bytes;
    cursor.end = bytes + length;
    if (!parse_singletons(&cursor, plan) || !count_sections(cursor, backend_section(plan->backend), &repeated, &entries))
        return EINVAL;

    if (!allocate_sections(plan, repeated, entries)) {
        plan_release(plan);
        return ENOMEM;
    }
    if (!parse_repeated(&cursor, plan, repeated)) {
        plan_release(plan);
        return EINVAL;
    }
    for (i = 0; i < entries; i++) {
        char *entry;
        if (!read_value(&cursor, "env", &entry)) {
            plan_release(plan);
            return EINVAL;
        }
        plan->environment[plan->environment_count++] = entry;
    }
    return 0;
}

void plan_release(struct plan *plan)
{
    free(plan->binds);
    free(plan->parameters);
    free(plan->environment);
    plan->binds = NULL;
    plan->bind_count = 0;
    plan->parameters = NULL;
    plan->parameter_count = 0;
    plan->environment = NULL;
    plan->environment_count = 0;
}
