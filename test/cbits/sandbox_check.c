/*
 * The sandbox helper's parsers as the test suite calls them: a plan parsed
 * by plan.c and written back in the plan's own token form, and the mount
 * points mountinfo.c finds, so the suite can hold each to what
 * Nix.Builder.Sandbox renders and Nix.Builder.Sandbox.Spawn reads.
 */

#include "mountinfo.h"
#include "plan.h"

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* The longest decimal length a token is given, a 64-bit size's. */
#define LENGTH_DIGITS_MAX 20

struct buffer {
    char *bytes;
    size_t used;
    size_t capacity;
};

static int append(struct buffer *buffer, const char *bytes, size_t length)
{
    if (buffer->capacity - buffer->used < length) {
        size_t capacity = buffer->capacity == 0 ? 256 : buffer->capacity;
        char *larger;
        while (capacity - buffer->used < length)
            capacity *= 2;
        larger = realloc(buffer->bytes, capacity);
        if (larger == NULL)
            return 0;
        buffer->bytes = larger;
        buffer->capacity = capacity;
    }
    memcpy(buffer->bytes + buffer->used, bytes, length);
    buffer->used += length;
    return 1;
}

/* Append LENGTH bytes as a length-prefixed item: the length in decimal, a
 * colon and the bytes, then a NUL when TERMINATED, as a plan's tokens
 * end. */
static int append_item(struct buffer *buffer, const char *bytes, size_t length, int terminated)
{
    char prefix[LENGTH_DIGITS_MAX + 2];
    int written = snprintf(prefix, sizeof prefix, "%lu:", (unsigned long)length);
    return written > 0 && append(buffer, prefix, (size_t)written) && append(buffer, bytes, length)
        && (!terminated || append(buffer, "", 1));
}

static int append_token(struct buffer *buffer, const char *token)
{
    return append_item(buffer, token, strlen(token), 1);
}

static int append_number(struct buffer *buffer, unsigned long number)
{
    char text[LENGTH_DIGITS_MAX + 1];
    snprintf(text, sizeof text, "%lu", number);
    return append_token(buffer, text);
}

static int append_bind(struct buffer *buffer, const struct bind *bind)
{
    char flags[4];
    size_t count = 0;
    if (bind->read_only)
        flags[count++] = 'r';
    if (bind->optional)
        flags[count++] = 'o';
    if (bind->canonical)
        flags[count++] = 'c';
    if (count == 0)
        flags[count++] = '-';
    flags[count] = '\0';
    return append_token(buffer, "bind") && append_token(buffer, bind->target) && append_token(buffer, bind->source)
        && append_token(buffer, flags);
}

static int render_linux(struct buffer *buffer, const struct plan *plan)
{
    size_t i;
    if (!(append_token(buffer, "backend") && append_token(buffer, "linux") && append_token(buffer, "root")
          && append_token(buffer, plan->root) && append_token(buffer, "build-dir")
          && append_token(buffer, plan->build_dir) && append_token(buffer, "dev-shm-size")
          && append_token(buffer, plan->dev_shm_size) && append_token(buffer, "sandbox-uid")
          && append_number(buffer, plan->sandbox_uid) && append_token(buffer, "sandbox-gid")
          && append_number(buffer, plan->sandbox_gid) && append_token(buffer, "network")
          && append_token(buffer, plan->private_network ? "private" : "host") && append_token(buffer, "drop-groups")
          && append_token(buffer, plan->require_drop_groups ? "require" : "tolerate")))
        return 0;
    for (i = 0; i < plan->bind_count; i++)
        if (!append_bind(buffer, &plan->binds[i]))
            return 0;
    return 1;
}

static int render_darwin(struct buffer *buffer, const struct plan *plan)
{
    size_t i;
    if (!(append_token(buffer, "backend") && append_token(buffer, "darwin") && append_token(buffer, "build-dir")
          && append_token(buffer, plan->build_dir) && append_token(buffer, "profile")
          && append_token(buffer, plan->profile)))
        return 0;
    for (i = 0; i < plan->parameter_count; i++)
        if (!(append_token(buffer, "param") && append_token(buffer, plan->parameters[2 * i])
              && append_token(buffer, plan->parameters[2 * i + 1])))
            return 0;
    return plan->parameters[2 * plan->parameter_count] == NULL;
}

static int render_windows(struct buffer *buffer, const struct plan *plan)
{
    char cap[LENGTH_DIGITS_MAX + 1];
    snprintf(cap, sizeof cap, "%llu", plan->memory_cap);
    return append_token(buffer, "backend") && append_token(buffer, "windows") && append_token(buffer, "build-dir")
        && append_token(buffer, plan->build_dir) && append_token(buffer, "token")
        && append_number(buffer, plan->token_handle) && append_token(buffer, "slot-sid")
        && append_token(buffer, plan->slot_sid) && append_token(buffer, "desktop")
        && append_token(buffer, plan->desktop) && append_token(buffer, "memory-cap") && append_token(buffer, cap);
}

static int render_backend(struct buffer *buffer, const struct plan *plan)
{
    switch (plan->backend) {
    case PLAN_LINUX:
        return render_linux(buffer, plan);
    case PLAN_DARWIN:
        return render_darwin(buffer, plan);
    case PLAN_WINDOWS:
        return render_windows(buffer, plan);
    }
    return 0;
}

static int render(struct buffer *buffer, const struct plan *plan)
{
    size_t i;
    if (!render_backend(buffer, plan))
        return 0;
    for (i = 0; i < plan->environment_count; i++)
        if (!append_token(buffer, "env") || !append_token(buffer, plan->environment[i]))
            return 0;
    return plan->environment[plan->environment_count] == NULL;
}

/* Parse the LENGTH bytes of a plan's tokens at BYTES and write the plan
 * parsed back as tokens, in a buffer for the caller to free.  Answers 0,
 * or plan_parse's errno value. */
int nova_test_plan_echo(const char *bytes, size_t length, char **out, size_t *out_length)
{
    struct plan plan;
    struct buffer rendered = {NULL, 0, 0};
    char *copy = malloc(length > 0 ? length : 1);
    int error;
    if (copy == NULL)
        return ENOMEM;
    memcpy(copy, bytes, length);
    error = plan_parse(copy, length, &plan);
    if (error == 0) {
        if (!render(&rendered, &plan))
            error = ENOMEM;
        plan_release(&plan);
    }
    free(copy);
    if (error != 0) {
        free(rendered.bytes);
        return error;
    }
    *out = rendered.bytes;
    *out_length = rendered.used;
    return 0;
}

/* The mount points at or beneath DIR in a mountinfo text, each written
 * length-prefixed, with no terminator, in a buffer for the caller to free.
 * Answers 0, or ENOMEM. */
int nova_test_mountinfo_within(const char *text, size_t length, const char *dir, size_t dir_length, char **out,
                               size_t *out_length)
{
    struct mountinfo_cursor cursor;
    struct buffer found = {NULL, 0, 0};
    size_t capacity = length + 1;
    char *point = malloc(capacity);
    size_t point_length = 0;
    int error = 0;
    if (point == NULL)
        return ENOMEM;
    cursor.next = text;
    cursor.end = text + length;
    while (mountinfo_next_within(&cursor, dir, dir_length, point, capacity, &point_length) == MOUNTINFO_FOUND)
        if (!append_item(&found, point, point_length, 0)) {
            error = ENOMEM;
            break;
        }
    free(point);
    if (error != 0) {
        free(found.bytes);
        return error;
    }
    *out = found.bytes;
    *out_length = found.used;
    return 0;
}
