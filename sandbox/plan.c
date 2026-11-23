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

static int parse_singletons(struct tokens *cursor, struct plan *plan)
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

/* Count the binds and the entries after the singletons, checking that
 * nothing but those follows. */
static int count_sections(struct tokens cursor, size_t *binds, size_t *entries)
{
    char *key = NULL;
    enum token_read read = read_token(&cursor, &key);
    *binds = 0;
    *entries = 0;
    while (read == TOKEN_FOUND && strcmp(key, "bind") == 0) {
        if (!skip_tokens(&cursor, 3))
            return 0;
        (*binds)++;
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

int plan_parse(char *bytes, size_t length, struct plan *plan)
{
    struct tokens cursor;
    size_t binds;
    size_t entries;
    size_t i;

    memset(plan, 0, sizeof *plan);
    cursor.next = bytes;
    cursor.end = bytes + length;
    if (!parse_singletons(&cursor, plan) || !count_sections(cursor, &binds, &entries))
        return EINVAL;

    if (binds > 0) {
        plan->binds = calloc(binds, sizeof *plan->binds);
        if (plan->binds == NULL)
            return ENOMEM;
    }
    plan->environment = calloc(entries + 1, sizeof *plan->environment);
    if (plan->environment == NULL) {
        plan_release(plan);
        return ENOMEM;
    }
    for (i = 0; i < binds; i++) {
        if (!parse_bind(&cursor, &plan->binds[i])) {
            plan_release(plan);
            return EINVAL;
        }
        plan->bind_count++;
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
    free(plan->environment);
    plan->binds = NULL;
    plan->bind_count = 0;
    plan->environment = NULL;
    plan->environment_count = 0;
}
