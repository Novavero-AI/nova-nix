/*
 * usage_compressed.c - applets/usage piped through
 * applets/usage_compressed, for a tree with no shell, od, sed or bzip2.
 *
 * It compiles applets/usage.c into itself and runs that program's main
 * with stdout on the file its first argument names, where the script
 * reads it through a pipe.  It then writes the header its second argument
 * names as the script writes it, up to PACKED_USAGE: UNPACKED_USAGE, those
 * bytes as octal escapes sixteen to a line, and UNPACKED_USAGE_LENGTH.
 * PACKED_USAGE, the script's bzip2 -1 stream, is left out, since
 * libbb/appletlib.c reads it only with FEATURE_COMPRESS_USAGE, which the
 * package turns off for want of bzip2.
 *
 * Compiled from the top of the tree, with -I . -I include.
 */

#include <fcntl.h>
#include <io.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>

#define main usage_main
#include "applets/usage.c"
#undef main

#if ENABLE_FEATURE_COMPRESS_USAGE
#error "FEATURE_COMPRESS_USAGE reads PACKED_USAGE, which this program does not write"
#endif

enum { BYTES_PER_LINE = 16 };

static void
fail(const char *what, const char *path)
{
    fprintf(stderr, "usage_compressed: %s %s\n", what, path);
    exit(EXIT_FAILURE);
}

int
main(int argc, char **argv)
{
    FILE *messages;
    FILE *header;
    long length = 0;
    int byte;
    int fd;

    if (argc != 3) {
        fprintf(stderr, "usage: usage_compressed MESSAGES HEADER\n");
        return EXIT_FAILURE;
    }

    fd = _open(argv[1], _O_WRONLY | _O_CREAT | _O_TRUNC | _O_BINARY, _S_IREAD | _S_IWRITE);
    if (fd < 0)
        fail("cannot create", argv[1]);
    if (fd != STDOUT_FILENO) {
        if (_dup2(fd, STDOUT_FILENO) != 0)
            fail("cannot redirect stdout to", argv[1]);
        _close(fd);
    }
    if (usage_main() != 0)
        fail("usage failed writing", argv[1]);
    _close(STDOUT_FILENO);

    messages = fopen(argv[1], "rb");
    if (!messages)
        fail("cannot open", argv[1]);
    header = fopen(argv[2], "wb");
    if (!header)
        fail("cannot create", argv[2]);
    fputs("#define UNPACKED_USAGE \"\" \\\n", header);
    while ((byte = getc(messages)) != EOF) {
        if (length % BYTES_PER_LINE == 0)
            fputs("\"", header);
        fprintf(header, "\\%03o", byte);
        length++;
        if (length % BYTES_PER_LINE == 0)
            fputs("\" \\\n", header);
    }
    if (ferror(messages))
        fail("cannot read", argv[1]);
    if (length % BYTES_PER_LINE != 0)
        fputs("\" \\\n", header);
    fprintf(header, "\n#define UNPACKED_USAGE_LENGTH %ld\n\n", length);
    fclose(messages);
    if (fclose(header) != 0)
        fail("cannot write", argv[2]);
    return EXIT_SUCCESS;
}
