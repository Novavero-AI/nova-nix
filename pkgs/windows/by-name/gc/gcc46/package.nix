# GCC 4.6.4's C compiler, libgcc and MinGW-w64's runtime in one prefix, so
# that the driver links a C source into a Windows program with no flag
# naming any of them.  nixpkgs' minimal-bootstrap names this stage gcc46
# (nixpkgs 5eb598da22ce76bc18ec85ecc1de66ca838b3514, default.nix), the GCC
# tinycc builds; here that build is gcc-core, and this lays out its
# installed tree with the runtime it compiled, mingw-w64-crt.
#
# MinGW-w64's build guide installs the runtime in the GCC core's prefix,
# with --prefix=<prefix>/<target> (mingw-w64-doc/howto-build/
# mingw-w64-howto-build-adv.txt, "Build the mingw-w64 CRT"), and GCC's top
# level hands its own target libraries the same directories,
# -L ${prefix}/${target}/lib and -isystem ${prefix}/${target}/include
# (FLAGS_FOR_TARGET's *-w64-mingw* case in configure.ac).  A derivation's
# prefix is its output, so this copies gcc-core's output and then the
# runtime's into its i686-w64-mingw32 directory.
#
# The driver finds the runtime there because it relocates: when the
# directory it runs from is not the bin it was configured with, it takes
# every prefix relative to the one it runs from, and none of the
# configured ones (gcc_exec_prefix in gcc/gcc.c's process_command,
# make_relative_prefix_1 in libiberty/make-relative-prefix.c).  It then
# searches lib/gcc/i686-w64-mingw32/4.6.4 beside it for libgcc and
# crtbegin.o, and the target's lib for the rest (tooldir_prefix in
# process_command): crt2.o and the -lmingw32, -lmoldname, -lmingwex,
# -lmsvcrt, -ladvapi32, -lshell32, -luser32 and -lkernel32 every link
# names (LIB_SPEC, REAL_LIBGCC_SPEC and STARTFILE_SPEC in
# gcc/config/i386/mingw32.h).  A cross driver has no other place for
# them: it reads no LIBRARY_PATH (LIBRARY_PATH_ENV in process_command) and
# searches no system directory (main's startfile prefixes for a native
# compiler or a sysroot).  cc1 takes the header directories from the same
# prefix (add_standard_paths in gcc/incpath.c).  The programs still name
# gcc-core's output, which stays in this one's closure.
#
# busybox's cp copies a file's data (libbb/copy_file.c) and not the
# nova.exec stream nova-nix reads the executable bit from.  None of
# gcc-core's files carries one, since neither tcc nor busybox-w32's chmod,
# which is msvcrt.dll's (win32/mingw.c), writes it, so each copy registers
# as its original does.
{
  busybox-w32,
  gcc-core,
  mingw-w64-crt,
}:
let
  inherit (builtins.parseDrvName gcc-core.name) version;
in
derivation {
  name = "gcc-${version}";
  system = "i686-windows";
  builder = "${busybox-w32}/bin/busybox.exe";
  args = [
    "sh"
    (builtins.toFile "gcc46.sh" ''
      set -e
      cp -R "$gccCore" "$out"
      cp -R "$crt"/. "$out/$target"
    '')
  ];
  gccCore = gcc-core;
  crt = mingw-w64-crt;
  target = "i686-w64-mingw32";
}
