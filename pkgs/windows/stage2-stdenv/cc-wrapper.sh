#!/bin/sh
# nova-nix stage-2 stdenv: one of gcc-native-x86_64's compiler drivers, as
# the stdenv's builds run it.  wrap.sh writes gcc, cc, g++ and c++ from
# this template, each with its driver as @driver@, -std=gnu17 as @std@ for
# the C drivers and nothing for the C++ ones, and link.specs as @specs@.
#
# GCC 16.2.0 searches directories that variables of the environment name:
# for headers CPATH, and C_INCLUDE_PATH or CPLUS_INCLUDE_PATH by the
# language (register_include_chains in gcc/incpath.cc; the Objective-C
# ones serve a language gcc-native-x86_64 does not compile), and the
# include directory of each COMPILER_PATH names (process_command, and %I in
# do_spec_1, in gcc/gcc.cc); for libraries LIBRARY_PATH and LPATH, and
# GCC_EXEC_PREFIX, which also moves where it finds its own programs
# (process_command in gcc/gcc.cc).  Unset, a build's compiler searches the
# directories its command line names and its own alone.  The driver sets
# GCC_EXEC_PREFIX, COMPILER_PATH and LIBRARY_PATH itself for the programs
# it runs (process_command and driver::maybe_run_linker in gcc/gcc.cc).
unset CPATH C_INCLUDE_PATH CPLUS_INCLUDE_PATH COMPILER_PATH LIBRARY_PATH LPATH GCC_EXEC_PREFIX

# GCC 16.2.0 compiles C as gnu23 by default (c_common_init_options in
# gcc/c-family/c-opts.cc), where an empty parameter list declares a
# function that takes no arguments, so the K&R declarations older gnulib
# copies carry, such as extern char *getenv ();, conflict with the C
# library's; the C drivers compile at gnu17, GCC 14's default, instead.
# -specs gives every link the driver makes --no-insert-timestamp (link.specs
# says how).  -std precedes the package's arguments, and the link spec the
# package's objects and -Wl options on the linker's command line
# (LINK_COMMAND_SPEC in gcc/gcc.cc), so a package's own -std or
# -Wl,--insert-timestamp wins.
exec '@driver@' @std@ '-specs=@specs@' "$@"
