/* The processes of a build user, for a build that runs as one
 * (Nix.Builder.BuildUser): its builder's start, which drops to the build
 * user between the fork and the exec, and the kill of every process the
 * user has.  POSIX only; every other platform compiles stubs that answer
 * ENOSYS, because the cabal c-sources entry is unconditional and the
 * Haskell side calls them only on Linux and macOS. */
#ifndef NN_BUILDUSERS_H
#define NN_BUILDUSERS_H

/* The step of a start that failed, which nn_spawn_as_user answers in
 * *STEP with the errno it failed with. */
enum nn_spawn_step {
  NN_SPAWN_PIPE = 1,
  NN_SPAWN_FORK = 2,
  NN_SPAWN_SESSION = 3,
  NN_SPAWN_INPUT = 4,
  NN_SPAWN_OUTPUT = 5,
  NN_SPAWN_CHDIR = 6,
  NN_SPAWN_GROUPS = 7,
  NN_SPAWN_GID = 8,
  NN_SPAWN_UID = 9,
  NN_SPAWN_EXEC = 10
};

/* Start PROGRAM with the arguments ARGV and the environment ENVP, each
 * NULL-terminated, in the directory CWD, reading INPUT_FD and writing
 * OUTPUT_FD as both its standard output and its standard error, as the
 * user UID with the group GID and the GROUP_COUNT supplementary groups
 * GROUPS, leading a session of its own, with no core dumps, the default
 * disposition for every signal and every other descriptor closed.
 * Answers 0 with the child's process id in *PID once the program has
 * replaced the child, or the errno of the step that failed, named in
 * *STEP, with no child left behind; EINVAL for uid 0, which is no build
 * user. */
int nn_spawn_as_user(const char *program, char *const argv[], char *const envp[], const char *cwd, int input_fd,
                     int output_fd, unsigned int uid, unsigned int gid, int group_count, const unsigned int *groups,
                     int *pid, int *step);

/* Send SIGKILL to every process whose user is UID, from a child that is
 * that user, as kill(-1) reaches only the processes its caller may
 * signal.  Answers 0 with *STATUS 0 once the signal went out, or with
 * *STATUS the child's exit code, or 256 plus the signal that ended it,
 * when the child could not send it; or the errno of a fork or a wait that
 * failed, EINVAL for uid 0, whose kill would reach every process. */
int nn_kill_user(unsigned int uid, int *status);

/* Wait for the child PID to end, leaving it to be reaped, so its process
 * id names no other process until it is.  Answers 0, or the errno of a
 * wait that failed. */
int nn_await_exit(int pid);

/* The system's description of the signal SIGNAL. */
const char *nn_signal_text(int signal);

#endif
