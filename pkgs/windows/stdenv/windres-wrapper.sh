#!/bin/sh
# nova-nix stdenv: binutils-native-x86_64's resource compiler, as
# the stdenv's builds run it under the name windres.  wrap.sh writes it
# from this template, with windres.exe as @windres@ and
# gcc-native-x86_64's gcc.exe as @driver@.
#
# windres preprocesses a resource script by running gcc -E -xc
# -DRC_INVOKED, and reads the output through popen, which starts the
# preprocessor through cmd.exe, a program from outside the store, unless
# --use-temp-file has windres start the preprocessor itself and read its
# output from a file, as it does here.  The preprocessor is the driver
# itself, by its path, so that what runs does not depend on PATH, with
# windres' default arguments ahead of any the package passes, and a
# --preprocessor of the package's own replaces it, since windres keeps the
# last.  The driver runs without the variables that name directories GCC
# searches, as cc-wrapper.sh runs it.
unset CPATH C_INCLUDE_PATH CPLUS_INCLUDE_PATH COMPILER_PATH LIBRARY_PATH LPATH GCC_EXEC_PREFIX
exec '@windres@' --use-temp-file '--preprocessor=@driver@' --preprocessor-arg=-E --preprocessor-arg=-xc --preprocessor-arg=-DRC_INVOKED "$@"
