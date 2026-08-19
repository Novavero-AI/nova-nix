#!/usr/bin/bash
# nova-nix stage-1 cc-wrapper.
#
# Installed onto the build's PATH under the real tool names (gcc, cc, g++,
# c++, ld, windres), ahead of the actual mingw toolchain, so every toolchain
# call flows through here.  We add the flags every build should get --
# environment hygiene for hermeticity, and determinism on links -- then exec
# the real tool, found by absolute path under $NN_TOOLCHAIN so we never
# re-enter ourselves.
#
# Name-to-intercept, absolute-path-to-delegate: the wrapper is named `gcc`,
# the real one is `gcc.exe`, so a PATH search hits us first and our exec hits
# it -- distinct filenames, so the wrapper can never accidentally find itself.

# Hermeticity: refuse ambient header/library search paths that could leak in
# via the environment.  The build sees only what the toolchain and buildInputs
# declare on the command line, never whatever happens to be set on the host.
# (windres runs the C preprocessor internally, so it needs the same scrub.)
unset CPATH C_INCLUDE_PATH CPLUS_INCLUDE_PATH OBJC_INCLUDE_PATH LIBRARY_PATH

tool="$(basename "$0")"
case "$tool" in
  gcc | cc) real="$NN_TOOLCHAIN/gcc.exe" ;;
  g++ | c++) real="$NN_TOOLCHAIN/g++.exe" ;;
  ld) real="$NN_TOOLCHAIN/ld.exe" ;;
  windres) real="$NN_TOOLCHAIN/windres.exe" ;;
  *)
    echo "cc-wrapper: installed under unknown tool name: $tool" >&2
    exit 1
    ;;
esac

# Determinism: a linked PE image must not carry the wall-clock build time in
# its header.  Direct ld always links (flag unprefixed); windres links
# nothing and gets the env scrub alone; the compiler drivers link unless the
# line is compile-only (-c / -E / -S / dependency generation).
case "$tool" in
  ld) exec "$real" "$@" --no-insert-timestamp ;;
  windres) exec "$real" "$@" ;;
esac

linkStep=1
for arg in "$@"; do
  case "$arg" in
    -c | -E | -S | -M | -MM) linkStep=0 ;;
  esac
done

# Compatibility (C drivers only): gcc 15+ defaults to C23, where an empty
# parameter list () means (void).  That breaks the K&R-style
# `extern char *getenv ();` declarations still shipped in the gnulib bundled
# by classic GNU autotools releases (make, coreutils, ...).  Default to
# gnu17 -- the gcc-14 default, the standard that code was written against.
# It precedes "$@" so a package can override it with its own -std later on
# the line (gcc honours the last -std wins).  The C++ drivers get no
# default: their dialect default is unchanged, and a C -std would be
# rejected.
std=""
case "$tool" in
  gcc | cc) std="-std=gnu17" ;;
esac

if [ "$linkStep" = 1 ]; then
  exec "$real" $std "$@" -Wl,--no-insert-timestamp
else
  exec "$real" $std "$@"
fi
