#!/usr/bin/bash
# nova-nix stage-1 cc-wrapper.
#
# Installed onto the build's PATH under the real tool names (gcc, cc), ahead of
# the actual mingw toolchain, so every compiler call flows through here.  We add
# the flags every build should get -- environment hygiene for hermeticity, and
# determinism on links -- then exec the real compiler, named by its absolute
# path ($NN_REAL_CC) so we never re-enter ourselves.
#
# Name-to-intercept, absolute-path-to-delegate: the wrapper is named `gcc`, the
# real one is `gcc.exe`, so a PATH search hits us first and our exec hits it --
# distinct filenames, so the wrapper can never accidentally find itself.

# Hermeticity: refuse ambient header/library search paths that could leak in via
# the environment.  The compile sees only what the toolchain and buildInputs
# declare on the command line, never whatever happens to be set on the host.
unset CPATH C_INCLUDE_PATH CPLUS_INCLUDE_PATH OBJC_INCLUDE_PATH LIBRARY_PATH

# Determinism: on a link step, tell the linker not to stamp the PE header with
# the wall-clock build time.  Compile-only invocations (-c / -E / -S / dependency
# generation) drive no linker, so we leave their command line untouched.
linkStep=1
for arg in "$@"; do
  case "$arg" in
    -c | -E | -S | -M | -MM) linkStep=0 ;;
  esac
done

# Compatibility: gcc 15+ defaults to C23, where an empty parameter list () means
# (void).  That breaks the K&R-style `extern char *getenv ();` declarations still
# shipped in the gnulib bundled by classic GNU autotools releases (make,
# coreutils, ...).  Default to gnu17 -- the gcc-14 default, the standard that code
# was written against.  It precedes "$@" so a package can override it with its own
# -std later on the line (gcc honours the last -std wins).
stdDefault=-std=gnu17

if [ "$linkStep" = 1 ]; then
  exec "$NN_REAL_CC" "$stdDefault" "$@" -Wl,--no-insert-timestamp
else
  exec "$NN_REAL_CC" "$stdDefault" "$@"
fi
