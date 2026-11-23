/*
 * The mountinfo reader: mountinfo.h says what it reads.
 */

#include "mountinfo.h"

#include <string.h>

/* The field a line's mount point is, counting from one. */
#define MOUNT_POINT_FIELD 5

/* The digits of one of the kernel's octal escapes. */
#define ESCAPE_DIGITS 3

static int is_octal(char byte)
{
    return byte >= '0' && byte <= '7';
}

/* Undo the escapes in the LENGTH bytes at FIELD, writing the first
 * CAPACITY bytes of the result to OUT.  Answers the length of the whole
 * result.  A backslash that does not begin an escape stays as it is, and
 * an escape past 0377 keeps its low eight bits, as the kernel writes
 * none. */
static size_t unescape(const char *field, size_t length, char *out, size_t capacity)
{
    size_t read = 0;
    size_t produced = 0;
    while (read < length) {
        char byte = field[read];
        if (byte == '\\' && length - read > ESCAPE_DIGITS && is_octal(field[read + 1]) && is_octal(field[read + 2])
            && is_octal(field[read + 3])) {
            int value = (field[read + 1] - '0') * 64 + (field[read + 2] - '0') * 8 + (field[read + 3] - '0');
            byte = (char)(unsigned char)(value & 0xff);
            read += ESCAPE_DIGITS + 1;
        } else {
            read++;
        }
        if (produced < capacity)
            out[produced] = byte;
        produced++;
    }
    return produced;
}

/* Whether the LENGTH bytes at POINT are DIR or a path beneath it. */
static int within(const char *point, size_t length, const char *dir, size_t dir_length)
{
    if (length < dir_length || memcmp(point, dir, dir_length) != 0)
        return 0;
    return length == dir_length || point[dir_length] == '/';
}

enum mountinfo_read mountinfo_next_within(struct mountinfo_cursor *cursor, const char *dir, size_t dir_length,
                                          char *point, size_t capacity, size_t *point_length)
{
    while (cursor->next < cursor->end) {
        const char *line = cursor->next;
        const char *line_end = memchr(line, '\n', (size_t)(cursor->end - line));
        const char *field = line;
        const char *field_end;
        int number;
        size_t length;

        if (line_end == NULL)
            line_end = cursor->end;
        cursor->next = line_end < cursor->end ? line_end + 1 : cursor->end;

        for (number = 1; number < MOUNT_POINT_FIELD && field != NULL; number++) {
            const char *space = memchr(field, ' ', (size_t)(line_end - field));
            field = space == NULL ? NULL : space + 1;
        }
        if (field == NULL)
            continue;
        field_end = memchr(field, ' ', (size_t)(line_end - field));
        if (field_end == NULL)
            field_end = line_end;

        length = unescape(field, (size_t)(field_end - field), point, capacity);
        if (length > capacity) {
            /* Too long to hold, which matters only for a point beneath
             * DIR, or one whose first bytes cannot say. */
            if (capacity <= dir_length || within(point, capacity, dir, dir_length))
                return MOUNTINFO_TOO_LONG;
            continue;
        }
        if (within(point, length, dir, dir_length)) {
            *point_length = length;
            return MOUNTINFO_FOUND;
        }
    }
    return MOUNTINFO_END;
}
