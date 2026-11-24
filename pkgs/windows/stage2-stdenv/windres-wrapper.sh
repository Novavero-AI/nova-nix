#!/bin/sh
# nova-nix stage-2 stdenv: binutils-native-x86_64's resource compiler, as
# the stdenv's builds run it under the name windres.  wrap.sh writes it
# from this template, with windres.exe as @windres@ and
# gcc-native-x86_64's gcc.exe as @driver@.
#
# windres preprocesses a resource script by running gcc -E -xc
# -DRC_INVOKED through cmd.exe, which popen starts
# (DEFAULT_PREPROCESSOR_CMD, DEFAULT_PREPROCESSOR_ARGS and
# open_input_stream in binutils/resrc.c in binutils 2.46), and cmd.exe
# runs a program only by an extension it knows, which the stdenv's gcc, a
# script, has not.  A --preprocessor that holds a space is quoted whole as
# one program name (OPTION_PREPROCESSOR in binutils/windres.c's main), so it
# cannot name busybox's sh and the script either.  The preprocessor is the
# driver itself, by its path, with windres' default arguments ahead of any
# the package passes, and a --preprocessor of the package's own replaces it,
# since windres keeps the last.  The driver runs without the variables that
# name directories GCC searches, as cc-wrapper.sh runs it.
unset CPATH C_INCLUDE_PATH CPLUS_INCLUDE_PATH COMPILER_PATH LIBRARY_PATH LPATH GCC_EXEC_PREFIX
exec '@windres@' '--preprocessor=@driver@' --preprocessor-arg=-E --preprocessor-arg=-xc --preprocessor-arg=-DRC_INVOKED "$@"
