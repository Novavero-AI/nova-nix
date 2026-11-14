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
# (configure.ac:3110-3115).  A derivation's prefix is its output, so this
# copies gcc-core's output and then the runtime's into its
# i686-w64-mingw32 directory.
#
# The driver finds the runtime there because it relocates: when the
# directory it runs from is not the bin it was configured with, it takes
# every prefix relative to the one it runs from, and none of the
# configured ones (gcc/gcc.c:3580-3592, 3622-3642 and 3870-3881,
# libiberty/make-relative-prefix.c:333-343).  It then searches
# lib/gcc/i686-w64-mingw32/4.6.4 beside it for libgcc and crtbegin.o, and
# the target's lib for the rest (gcc.c:3884-3900): crt2.o and the
# -lmingw32, -lmoldname, -lmingwex, -lmsvcrt, -ladvapi32, -lshell32,
# -luser32 and -lkernel32 every link names (gcc/config/i386/mingw32.h:
# 78-80 and 113-123).  A cross driver has no other place for them: it
# reads no LIBRARY_PATH (gcc.c:3681-3682) and searches no system directory
# (6395-6436).  cc1 takes the header directories from the same prefix
# (gcc/incpath.c:136-199).  The programs still name gcc-core's output,
# which stays in this one's closure.
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
