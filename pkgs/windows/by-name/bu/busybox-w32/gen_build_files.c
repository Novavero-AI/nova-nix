/*
 * gen_build_files.c - scripts/gen_build_files.sh, for a tree with no shell.
 *
 * Run from the top of a busybox-w32 tree, it writes what the script writes
 * there, byte for byte: include/applets.h, from include/applets.src.h and
 * the "//applet:" lines of the .c files one and two directories down;
 * include/usage.h, from include/usage.src.h and their "//usage:" lines;
 * and each directory's Config.in, from its Config.src and the "//config:"
 * lines of the .c files beside it.  The script's Kbuild files are not
 * written, since the package lists the sources it compiles.  The tree has
 * no embed/ directory, so no scripted applet joins applets.h.
 *
 * The script lists files with shell globs under LC_ALL=C, which skip names
 * that begin with a dot and sort each pattern's matches bytewise; the
 * listings here do the same, through msvcrt's _findfirst.
 */

#include <io.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct text
{
    char *bytes;
    size_t length;
    size_t capacity;
};

struct names
{
    char **items;
    size_t count;
    size_t capacity;
};

static void
fail(const char *what, const char *path)
{
    fprintf(stderr, "gen_build_files: %s %s\n", what, path);
    exit(EXIT_FAILURE);
}

static void
append(struct text *text, const char *bytes, size_t length)
{
    if (text->length + length > text->capacity) {
        size_t capacity = text->capacity ? text->capacity : 4096;
        while (capacity < text->length + length)
            capacity *= 2;
        text->bytes = realloc(text->bytes, capacity);
        if (!text->bytes)
            fail("out of memory for", "a text");
        text->capacity = capacity;
    }
    memcpy(text->bytes + text->length, bytes, length);
    text->length += length;
}

static void
append_string(struct text *text, const char *string)
{
    append(text, string, strlen(string));
}

static struct text
read_file(const char *path)
{
    struct text text = { NULL, 0, 0 };
    char buffer[4096];
    size_t got;
    FILE *file = fopen(path, "rb");
    if (!file)
        fail("cannot open", path);
    while ((got = fread(buffer, 1, sizeof buffer, file)) > 0)
        append(&text, buffer, got);
    if (ferror(file))
        fail("cannot read", path);
    fclose(file);
    return text;
}

static void
write_file(const char *path, const struct text *text)
{
    FILE *file = fopen(path, "wb");
    if (!file)
        fail("cannot create", path);
    if (fwrite(text->bytes, 1, text->length, file) != text->length
        || fclose(file) != 0)
        fail("cannot write", path);
}

static char *
join(const char *directory, const char *name)
{
    size_t prefix = strlen(directory);
    char *path = malloc(prefix + strlen(name) + 2);
    if (!path)
        fail("out of memory for", name);
    if (prefix)
        sprintf(path, "%s/%s", directory, name);
    else
        strcpy(path, name);
    return path;
}

static void
add_name(struct names *names, char *name)
{
    if (names->count == names->capacity) {
        names->capacity = names->capacity ? names->capacity * 2 : 64;
        names->items = realloc(names->items, names->capacity * sizeof *names->items);
        if (!names->items)
            fail("out of memory for", name);
    }
    names->items[names->count++] = name;
}

static void
free_names(struct names *names)
{
    size_t i;
    for (i = 0; i < names->count; i++)
        free(names->items[i]);
    free(names->items);
    names->items = NULL;
    names->count = names->capacity = 0;
}

static int
compare_names(const void *a, const void *b)
{
    return strcmp(*(char *const *) a, *(char *const *) b);
}

static int
has_suffix(const char *name, const char *suffix)
{
    size_t length = strlen(name);
    size_t tail = strlen(suffix);
    return length > tail && strcmp(name + length - tail, suffix) == 0;
}

enum entry_kind { DIRECTORIES, C_SOURCES };

/*
 * Adds to names the entries of directory ("" for the top) that a glob of
 * kind would match there, each as directory/name.  A C source is checked
 * for its .c suffix, since _findfirst's "*.c" also matches a name whose
 * 8.3 alias ends in .C.
 */
static void
list_directory(const char *directory, enum entry_kind kind, struct names *names)
{
    struct _finddata_t entry;
    char *pattern = join(directory, kind == DIRECTORIES ? "*" : "*.c");
    intptr_t handle = _findfirst(pattern, &entry);
    if (handle != -1) {
        do {
            int is_directory = (entry.attrib & _A_SUBDIR) != 0;
            if (entry.name[0] == '.')
                continue;
            if (kind == DIRECTORIES ? is_directory
                                    : !is_directory && has_suffix(entry.name, ".c"))
                add_name(names, join(directory, entry.name));
        } while (_findnext(handle, &entry) == 0);
        _findclose(handle);
    }
    free(pattern);
}

enum line_form { AS_IS, USAGE };

/*
 * The script's sed over files: each line that starts with prefix, the
 * prefix removed.  AS_IS is 's@^PREFIX@@p'; USAGE is the two expressions
 * that write usage.h, which end each line in " \" and start a line that
 * does not begin with a space or a tab on a line of its own.
 */
static void
collect(const struct names *files, const char *prefix, enum line_form form, struct text *out)
{
    size_t prefix_length = strlen(prefix);
    size_t i;
    for (i = 0; i < files->count; i++) {
        struct text source = read_file(files->items[i]);
        size_t start = 0;
        while (start < source.length) {
            const char *line = source.bytes + start;
            const char *newline = memchr(line, '\n', source.length - start);
            size_t length = newline ? (size_t) (newline - line) : source.length - start;
            start += length + 1;
            if (length < prefix_length || memcmp(line, prefix, prefix_length) != 0)
                continue;
            line += prefix_length;
            length -= prefix_length;
            if (form == AS_IS) {
                append(out, line, length);
                append_string(out, "\n");
            } else if (length > 0) {
                if (line[0] != ' ' && line[0] != '\t')
                    append_string(out, "\n");
                append(out, line, length);
                append_string(out, " \\\n");
            }
        }
        free(source.bytes);
    }
}

/*
 * The script's generate: header, the lines of source before its first line
 * that is exactly INSERT, body, then the lines after it, or all of source
 * and then body when it has no INSERT line.
 */
static void
generate(const char *source_path, const char *target_path, const char *header, const struct text *body)
{
    static const char marker[] = "INSERT\n";
    struct text source = read_file(source_path);
    struct text out = { NULL, 0, 0 };
    size_t before = source.length;
    size_t after = source.length;
    size_t start = 0;
    while (start < source.length) {
        const char *line = source.bytes + start;
        const char *newline = memchr(line, '\n', source.length - start);
        size_t length = newline ? (size_t) (newline - line) + 1 : source.length - start;
        if (length == sizeof marker - 1 && memcmp(line, marker, length) == 0) {
            before = start;
            after = start + length;
            break;
        }
        start += length;
    }
    append_string(&out, header);
    append_string(&out, "\n");
    append(&out, source.bytes, before);
    append(&out, body->bytes, body->length);
    append(&out, source.bytes + after, source.length - after);
    write_file(target_path, &out);
    free(out.bytes);
    free(source.bytes);
}

/* Each directory under directory, at any depth, with a Config.src. */
static void
generate_config_in(const char *directory)
{
    struct names subdirectories = { NULL, 0, 0 };
    char *config_src = join(directory, "Config.src");
    size_t i;
    if (_access(config_src, 0) == 0) {
        struct names sources = { NULL, 0, 0 };
        struct text body = { NULL, 0, 0 };
        char *config_in = join(directory, "Config.in");
        list_directory(directory, C_SOURCES, &sources);
        qsort(sources.items, sources.count, sizeof *sources.items, compare_names);
        collect(&sources, "//config:", AS_IS, &body);
        generate(config_src, config_in, "# DO NOT EDIT. This file is generated from Config.src", &body);
        free(config_in);
        free(body.bytes);
        free_names(&sources);
    }
    free(config_src);
    list_directory(directory, DIRECTORIES, &subdirectories);
    for (i = 0; i < subdirectories.count; i++)
        generate_config_in(subdirectories.items[i]);
    free_names(&subdirectories);
}

int
main(void)
{
    struct names top = { NULL, 0, 0 };
    struct names second = { NULL, 0, 0 };
    struct names sources = { NULL, 0, 0 };
    struct names deeper = { NULL, 0, 0 };
    struct text applets = { NULL, 0, 0 };
    struct text usage = { NULL, 0, 0 };
    size_t i;

    /* The .c files one directory down, sorted, then those two down. */
    list_directory("", DIRECTORIES, &top);
    for (i = 0; i < top.count; i++) {
        list_directory(top.items[i], C_SOURCES, &sources);
        list_directory(top.items[i], DIRECTORIES, &second);
    }
    for (i = 0; i < second.count; i++)
        list_directory(second.items[i], C_SOURCES, &deeper);
    qsort(sources.items, sources.count, sizeof *sources.items, compare_names);
    qsort(deeper.items, deeper.count, sizeof *deeper.items, compare_names);
    for (i = 0; i < deeper.count; i++)
        add_name(&sources, deeper.items[i]);
    deeper.count = 0;

    collect(&sources, "//applet:", AS_IS, &applets);
    generate("include/applets.src.h", "include/applets.h",
             "/* DO NOT EDIT. This file is generated from applets.src.h */", &applets);
    collect(&sources, "//usage:", USAGE, &usage);
    generate("include/usage.src.h", "include/usage.h",
             "/* DO NOT EDIT. This file is generated from usage.src.h */", &usage);
    generate_config_in("");

    free(applets.bytes);
    free(usage.bytes);
    free_names(&deeper);
    free_names(&sources);
    free_names(&second);
    free_names(&top);
    return 0;
}
