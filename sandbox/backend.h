/*
 * What nova-nix-sandbox's core and its backend give each other.
 *
 * The core (nova-nix-sandbox.c) is the program as every OS runs it: the
 * command line, the plan read from standard input, and the record a step
 * that fails writes on the status channel.  A backend is the sandbox of
 * one OS (linux.c, darwin.c): it starts the builder there as the plan
 * says, waits for it, ends it when the lifeline closes, and has the helper
 * end as the builder ended.  The executable links the core with the one
 * backend for the OS it is built for.  The core reads and writes its
 * channels through the backend, since each OS has its own calls for them;
 * the POSIX backends share theirs (posix.h).
 */

#ifndef NOVA_NIX_SANDBOX_BACKEND_H
#define NOVA_NIX_SANDBOX_BACKEND_H

#include <stddef.h>

#include "plan.h"

/* ------------------------------------------------------------------------
 * The core, for a backend
 * ------------------------------------------------------------------------ */

/* The index a record carries when the failure is of no plan entry. */
#define NO_INDEX (-1L)

/* Marks a function that never returns, for compilers that take the GNU
 * attribute, so that each file calling status_fail knows the step after
 * a failure is not reached.  C99 has no keyword for it. */
#if defined(__GNUC__)
#define NO_RETURN __attribute__((noreturn))
#else
#define NO_RETURN
#endif

/* Write the one record a failure reports, "STAGE ERROR INDEX\n", on the
 * status channel: the step, the errno it failed with or 0, and the plan
 * entry or process it concerns. */
void status_report(const char *stage, int error, long index);

/* Report a failure and end the process that saw it. */
void status_fail(const char *stage, int error, long index) NO_RETURN;

/* ------------------------------------------------------------------------
 * A backend, for the core
 * ------------------------------------------------------------------------ */

/* The backend a plan must be for; the core refuses a plan for another. */
extern const enum plan_backend backend_plan;

/* Ready this process to run a builder, before the core reads the plan.
 * Once it answers 0 the status channel takes records, and nothing the
 * builder writes reaches it.  Answers -1 when there is no status channel,
 * so that nothing can be reported; a step that fails once there is one
 * reports and ends the process. */
int backend_prepare_run(void);

/* Read exactly LENGTH bytes from standard input, taking nothing past them,
 * since what follows the plan is the lifeline.  Answers 0, or an errno
 * value; EPIPE for input that ends early. */
int backend_read_input(char *bytes, size_t length);

/* Write the LENGTH bytes at BYTES on the status channel, as far as it
 * takes them: standard output, unless backend_prepare_run moved it. */
void backend_write_status(const char *bytes, size_t length);

/* Run ARGV[0] with the arguments ARGV and the plan's environment, in the
 * sandbox PLAN describes.  Standard input, past the plan, is the
 * lifeline: when it closes, the builder is killed.  A step that fails
 * before the builder starts reports and ends the process.  Otherwise the
 * helper ends as the builder ended: this answers the builder's exit
 * status, or a failure when how it ended cannot be known, or does not
 * return when the backend ends the helper by the signal that ended the
 * builder. */
int backend_run(const struct plan *plan, char **argv);

/* Check that this system can make the sandbox.  Answers NULL when it can,
 * or the stage of the check that failed, with the errno it failed with,
 * or 0, in *ERROR. */
const char *backend_probe(int *error);

#endif
