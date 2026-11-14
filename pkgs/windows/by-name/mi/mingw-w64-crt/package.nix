# MinGW-w64's runtime for i686-w64-mingw32, as nixpkgs' mingw_w64 builds it
# (nixpkgs 5eb598da22ce76bc18ec85ecc1de66ca838b3514, pkgs/os-specific/
# windows/mingw-w64/default.nix), compiled by gcc-core with binutils'
# assembler, archiver and dlltool, in the seed stdenv: crt2.o and the other
# startup objects, libmingw32.a, libmingwex.a, libmoldname.a, libm.a, and
# an import library for each DLL, libmsvcrt.a and libkernel32.a among them.
# It is MinGW-w64's build guide's next step after the GCC core
# (mingw-w64-doc/howto-build, "Building the crt"), and gcc46 lays it out
# where the guide installs it, in the GCC core's prefix.
#
# The release is mingw-w64-headers' v8.0.3, whose runtime calls no builtin
# GCC 4.6.4 lacks (see mingw-w64-headers), from the same archive.  That
# package lists the archive among the set's sources, so this one lists
# none.  nixpkgs configures the archive's top level, which builds the
# headers again; this configures the runtime's own directory, as
# mingw-w64-headers configures the headers', from a build directory beside
# the source, as the guide's every step does.
#
# --host names the target: the stdenv's i686-pc-mingw32 stays the build,
# and configure looks up the compiler, ar, ranlib, dlltool and as under the
# host's name (configure.ac:37-46), on PATH through buildInputs.  It takes
# a compiler and an archiver as the environment names them first, and the
# stdenv's CC and AR are tcc's, so preConfigure unsets them; with tcc,
# configure stops at its check for MinGW-w64's _mingw_mac.h (configure.ac:
# 384).  It unsets C_INCLUDE_PATH too, which holds TinyCC's headers for
# tcc: GCC reads the variable as well, searches it before its own headers
# and MinGW-w64's (gcc/incpath.c:455-462 in 4.6.4), and rejects TinyCC's
# stdio.h, so configure stops at the same check.  The runtime reads no LD.
#
# --with-sysroot names mingw-w64-headers' output, whose include directory
# the runtime is compiled against (Makefile.am:14-22).  Its default is the
# prefix, which here holds no headers.
#
# --with-default-msvcrt=msvcrt is nixpkgs' flag, as mingw-w64-headers
# passes it: libmsvcrt.a is the import library for msvcrt.dll
# (configure.ac:236-247), and the runtime is compiled for its
# __MSVCRT_VERSION__ 0x700 (Makefile.am:23).
#
# --enable-lib32 and --disable-lib64 are nixpkgs' for an i686 host.  Left
# to itself, configure decides each by compiling a test with -m32 and -m64
# (configure.ac:64-100): a compiler that runs but fails the -m32 test would
# leave a runtime with no libraries and a build that passes, where with the
# flags the build compiles the runtime or fails.  GCC 4.6.4 for
# i686-w64-mingw32 has a 64-bit mode only with --enable-targets=all
# (gcc/config.gcc:1503-1508), and says so for -m64.  The ARM runtimes are
# off for an i686 host (configure.ac:102-136).
#
# --disable-dependency-tracking is what nixpkgs' configure phase adds to
# every configure that has it, and make builds with $NIX_BUILD_CORES jobs,
# as nixpkgs' enableParallelBuilding has it.
#
# CFLAGS=-O2 is configure's default for GCC without its -g, and the
# assembler sources take it too (CCASFLAGS defaults to CFLAGS): debugging
# information would name the build directory and the store paths the
# compiler read, in crt2.o and in every object of the libraries.
#
# configure links one test program, with -nostdlib, to ask whether ld
# defines __CTOR_LIST__ (configure.ac:304-361), and binutils' does.  If the
# link failed, crt2.o would define the symbol itself, and nothing would
# fail; test/data/gcc46.nix pins crt2.o's symbols for that reason.
#
# The libraries install in lib, the directory configure names for an i686
# host's 32-bit runtime (configure.ac:142-152, Makefile.am:601), and twelve
# of libsrc's C files, which the runtime installs uncompiled, in include
# (Makefile.am:2131-2135).
{
  seedStdenv,
  binutils,
  gcc-core,
  mingw-w64-headers,
}:
let
  pname = "mingw-w64-crt";
  version = "8.0.3";
  target = "i686-w64-mingw32";
in
seedStdenv.mkDerivation {
  name = "${pname}-${version}";

  inherit (mingw-w64-headers) src;

  buildInputs = [
    gcc-core
    binutils
  ];

  preConfigure = ''
    unset CC AR C_INCLUDE_PATH
    mkdir ../build
    cd ../build
    configureScript=../mingw-w64-v${version}/mingw-w64-crt/configure
  '';

  configureFlags = [
    "--host=${target}"
    "--with-sysroot=${mingw-w64-headers}"
    "--with-default-msvcrt=msvcrt"
    "--enable-lib32"
    "--disable-lib64"
    "--disable-dependency-tracking"
    "CFLAGS=-O2"
  ];

  enableParallelBuilding = true;
  # Two rules install lib32/libntoskrnl.a, lib32_LIBRARIES and lib32_DATA
  # (Makefile.am:773, and lib32/Makefile.am:208, which Makefile.am:820
  # includes), and make -j runs them at once on the one file.
  enableParallelInstalling = false;
}
// {
  srcs = [ ];
}
