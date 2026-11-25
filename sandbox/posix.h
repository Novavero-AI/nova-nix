/*
 * What nova-nix-sandbox's POSIX backends (linux.c, darwin.c) share.  This
 * file's C gives backend.h's channels as a POSIX system has them: the plan
 * on standard input, and the status channel on the pipe the helper gets
 * as standard output, which a run moves to a descriptor the builder's
 * exec closes.  It also starts a run and ends one: it reaps the builder,
 * and ends the helper as the builder ended.
 */

#ifndef NOVA_NIX_SANDBOX_POSIX_H
#define NOVA_NIX_SANDBOX_POSIX_H

#include <sys/types.h>

/* The status channel's descriptor: standard output until
 * backend_prepare_run moves it, and -1 once posix_close_status has closed
 * it. */
int posix_status_fd(void);

/* Close the status channel, once the builder is released and nothing is
 * left to report on it. */
void posix_close_status(void);

/* Put SIGCHLD back to its default, as upstream's initNix does (in
 * shared.cc at 2.24.9): a SIGCHLD ignored where this program was
 * started would have the kernel reap the builder unasked, leaving nothing
 * to wait for.  Answers 0, or an errno value. */
int posix_reset_sigchld(void);

/* Reap CHILD, blocking.  Answers 0, or the errno of a wait that failed. */
int posix_reap(pid_t child, int *wait_status);

/* End this process as the builder ended: by the signal that killed it,
 * with no core dump of this process's own, or by answering the status
 * this process is to exit with: the builder's exit status, 128 plus the
 * signal should raising it return, or EXIT_FAILURE for any other end. */
int posix_relay(int wait_status);

/* Close every descriptor from 3 up but KEEP, as upstream's child closes
 * them before it executes the builder.  LISTING is a directory naming each
 * open descriptor; where it cannot be read, every descriptor below the
 * process's limit is closed, or below CLOSE_FALLBACK_TOP where getrlimit
 * gives no finite limit an int holds. */
void posix_close_descriptors(const char *listing, int keep);

#endif
