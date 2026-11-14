/* The processor count upstream Nix's C++ runtime reports for
 * std::thread::hardware_concurrency, which upstream's default for its
 * `cores` setting is built on.  Windows compiles a constant -1 stub
 * because the cabal c-sources entry is unconditional; the Haskell side
 * counts processors through GHC's RTS there and never calls it. */
#ifndef NN_CPUS_H
#define NN_CPUS_H

/* sysconf(_SC_NPROCESSORS_ONLN), the call libc++ makes for
 * hardware_concurrency.  libstdc++ calls glibc's get_nprocs instead, the
 * function glibc's sysconf answers that name with.  Returns the count, or
 * -1 when sysconf has no answer, and -1 on Windows. */
long nn_online_processors(void);

#endif
