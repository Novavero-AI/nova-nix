/*
 * nova-nix-sandbox: runs one nova-nix builder in the sandbox its plan
 * describes.  This file is the program as every OS runs it; the backend
 * for the OS it is built for (backend.h) makes the sandbox.  nova-nix
 * runs every sandboxed build through this program, spawning it as it
 * spawns a builder, with an empty environment, so the dynamic loader
 * reads no variable the derivation set.
 *
 *   nova-nix-sandbox run PROGRAM [ARG...]
 *     Reads the plan from standard input, sets the sandbox up and runs
 *     PROGRAM there with ARGs and the environment the plan holds.
 *     Standard output is the status channel and standard error the build
 *     log.  On Linux the builder writes its standard output and standard
 *     error to the log itself; on macOS it writes both to a pipe of the
 *     helper's, which the helper copies into the log (darwin.c).  A
 *     step that fails writes one record, "STAGE ERRNO INDEX\n", on the
 *     status channel and the process exits.  A builder that starts leaves
 *     the channel empty, but on macOS for a failure to watch the build
 *     log, and the process exits as the builder did, or fails with the
 *     reason in the build log where it cannot see the build out
 *     (backend_run in backend.h).
 *     Standard input stays open after the plan as a lifeline: when
 *     nova-nix closes it, by ending or dying, the builder is killed.
 *
 *   nova-nix-sandbox probe
 *     Checks that this system lets the backend make the sandbox.  Exits 0
 *     when it does, or writes one record naming the check that failed and
 *     exits 1.
 *
 * The plan is a version line, a line holding the length of the rest, then
 * the tokens plan.h describes, which plan.c parses; a plan for another
 * backend than the one linked is refused as one that cannot be read.  Its
 * format and the stage names are shared with Nix.Builder.Sandbox, which
 * writes the one and reads the other.
 */

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "backend.h"
#include "plan.h"

/* The largest plan read, so a runaway writer cannot exhaust memory. */
#define PLAN_MAX_BYTES ((size_t)64 * 1024 * 1024)

/* The longest line of the plan's header, its length line included. */
#define PLAN_HEADER_LINE_MAX 64

/* ------------------------------------------------------------------------
 * Records
 * ------------------------------------------------------------------------ */

void status_report(const char *stage, int error, long index)
{
    char record[128];
    int length = snprintf(record, sizeof record, "%s %d %ld\n", stage, error, index);
    if (length > 0 && (size_t)length < sizeof record)
        backend_write_status(record, (size_t)length);
}

void status_fail(const char *stage, int error, long index)
{
    status_report(stage, error, index);
    _Exit(EXIT_FAILURE);
}

/* ------------------------------------------------------------------------
 * The plan
 * ------------------------------------------------------------------------ */

/* Read one header line, a byte at a time so nothing past it is taken from
 * the pipe.  Answers 0, or an errno value. */
static int read_header_line(char *line, size_t capacity)
{
    size_t length = 0;
    for (;;) {
        char byte;
        int error = backend_read_input(&byte, 1);
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
    error = backend_read_input(bytes, (size_t)length);
    if (error != 0) {
        free(bytes);
        return error;
    }
    *bytes_out = bytes;
    *length_out = (size_t)length;
    return 0;
}

/* ------------------------------------------------------------------------
 * The commands
 * ------------------------------------------------------------------------ */

static int run(char **argv)
{
    struct plan plan;
    char *plan_bytes = NULL;
    size_t plan_length = 0;
    int error;
    int status;

    if (backend_prepare_run() != 0)
        return EXIT_FAILURE;
    error = read_plan_bytes(&plan_bytes, &plan_length);
    if (error == 0)
        error = plan_parse(plan_bytes, plan_length, &plan);
    if (error == 0 && plan.backend != backend_plan)
        error = EINVAL;
    if (error != 0)
        status_fail("plan", error, NO_INDEX);
    status = backend_run(&plan, argv);
    plan_release(&plan);
    free(plan_bytes);
    return status;
}

static int probe(void)
{
    int error = 0;
    const char *failed = backend_probe(&error);
    if (failed != NULL) {
        status_report(failed, error, NO_INDEX);
        return EXIT_FAILURE;
    }
    return EXIT_SUCCESS;
}

int main(int argc, char **argv)
{
    if (argc >= 3 && strcmp(argv[1], "run") == 0)
        return run(argv + 2);
    if (argc == 2 && strcmp(argv[1], "probe") == 0)
        return probe();
    fprintf(stderr, "usage: %s run PROGRAM [ARG...] | %s probe\n", argv[0], argv[0]);
    return EXIT_FAILURE;
}
