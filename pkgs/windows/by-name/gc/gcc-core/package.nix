# GCC 4.6.4's C compiler and libgcc, as nixpkgs' minimal-bootstrap/gcc/
# 4.6.nix builds GCC with tinycc (nixpkgs
# 5eb598da22ce76bc18ec85ecc1de66ca838b3514), for Windows: the driver, cc1,
# cpp, collect2 and GCC's own headers, running on i686 Windows and writing
# assembly for i686-w64-mingw32, configured, compiled and installed in the
# seed stdenv by tinycc-native, and libgcc, compiled by that compiler in
# the same make, as nixpkgs' make builds it.  The compiler is what make
# all-gcc builds and make install-gcc installs, the one MinGW-w64's build
# guide installs before the runtime exists (mingw-w64-doc/howto-build,
# "Building the GCC core cross-compiler"); libgcc is the guide's make
# all-target-libgcc and make install-target-libgcc (howto-build-adv.txt,
# "Build libgcc").  MinGW-w64's runtime is the next rung, which this
# compiler compiles, and gcc46 lays the two out in one prefix, where the
# driver links a program.
#
# The stdenv's triple, i686-pc-mingw32, is the build and the host, and the
# target is binutils', so GCC takes itself for a cross compiler.  That is
# what it is: tcc reads TinyCC's headers and links msvcrt.dll alone, and
# GCC's target has MinGW-w64's headers and runtime.  The programs install
# as bin/i686-w64-mingw32-<name>.exe, the build looks its tools up under
# the target's name, on PATH through buildInputs, and never compiles itself
# a second time, which nixpkgs' --disable-bootstrap asks of a native build.
#
# --with-headers names mingw-w64-headers' include directory, which GCC's
# top-level configure copies into this output as
# i686-w64-mingw32/sys-include (the --with-headers case and copy_dirs in
# configure.ac), a cross compiler's own place for its target's headers.
# The build writes a limits.h that goes on to MinGW-w64's only if it finds
# one there (LIMITS_H_TEST in gcc/Makefile.in's stmp-int-hdrs), and the
# compiler searches the directory for every source, after its own headers
# (gcc/cppdefault.c).  Libraries it takes from its own directories and
# those -B names, and no others (tooldir_prefix in gcc/gcc.c's
# process_command, and main's startfile prefixes for a native compiler or
# a sysroot): a cross driver reads no LIBRARY_PATH (LIBRARY_PATH_ENV in
# process_command).
#
# GCC's manual prefers --with-sysroot, which names the headers where they
# are, and nixpkgs gives one to its cross compilers.  No store path can be
# one here.  The top-level hands each option to every directory's
# configure; mpc's is written by libtool 2.4, which has a --with-sysroot of
# its own and takes a path for absolute only if it begins with '/'
# (the --with-sysroot check in mpc/configure), and a store path on Windows
# begins with a drive letter.  --with-headers is read by the top-level and
# by gcc's configure alone.  nixpkgs' --with-native-system-header-dir, an
# option GCC has from 4.7 on, its --with-build-sysroot and its
# no-system-headers.patch point its native compiler at its C library's
# headers.
#
# --with-as and --with-ld name binutils' assembler and linker by their
# files.  A cross GCC runs the as and ld in its own prefix and then any on
# PATH, and binutils is another output, so the paths are compiled into the
# driver and collect2 (DEFAULT_LINKER and DEFAULT_ASSEMBLER in
# gcc/configure.ac) and configure's tests of both tools run the same
# files.
#
# gmp, mpfr and mpc are built in the tree, as in nixpkgs, which links each
# under the name configure looks for; preConfigure unpacks and renames
# them, there being no symbolic link.  GCC's Makefile has gmp configured
# for the host none-pc-mingw32, in C alone (the gmp host module in
# Makefile.def).  gmp's libtool archives libgmp.a's 402 objects in one
# command of some 7,400 characters, under the 8192 it takes for the limit
# on a MinGW build host; past that it archives in pieces, which tcc -ar,
# writing an archive anew each time, cannot.
#
# gmp writes its table of each base's digits per bit at build time, with
# a program it compiles and runs on the build machine, which computes
# each value with the machine's log and printf, so that the last digit
# depends on the machine: under the ARM64 leg's x86 emulator, which
# carries out x87 arithmetic in double precision, gcc46-cxx's program
# writes other digits for some bases than on x64, and that rung's
# compilers differ between the legs.  gmp-bases-digits.patch has the
# program write fixed digits instead, those an x86 processor gives, so
# the table is the one this rung and gcc46-cxx write on x64 without the
# patch.  preConfigure applies it once it has unpacked gmp, and gcc46-cxx
# applies it to the same archive.
#
# GCC configures from a build directory beside its source, as its
# installation guide recommends (INSTALL/configure.html), where nixpkgs
# configures in the source.  In the source, the top level builds the
# compiler a directory deeper, in host-i686-pc-mingw32/gcc, and gcc's
# Makefile then names the build machine's fixincl by two paths that
# differ: its rule that takes the program as built (the
# ../$(build_subdir)/fixincludes/fixincl rule in gcc/Makefile.in) and the
# prerequisite that needs it (stmp-fixinc's).  The program is fixincl.exe,
# so neither path is a file, and make links fixincl.o alone by its
# built-in rule.
#
# Of nixpkgs' other flags, those passed mean here what they mean there.
# --enable-checking=release is the default for a release
# (AC_ARG_ENABLE(checking) in gcc/configure.ac).  The --disable-lib* flags
# name target libraries other than libgcc, which the make targets here
# never configure, five of them later than 4.6.4.  --disable-lto-plugin
# follows from --disable-lto for a MinGW target (build_lto_plugin in the
# top-level configure.ac).  The three configure answers it exports are for
# link tests after GCC_NO_EXECUTABLES, of which libgcc's configure makes
# none (GCC_NO_EXECUTABLES in libgcc/configure.ac).  install-strip has
# nothing to strip: tcc writes no symbol table into a program, and libgcc
# carries no debugging information (below).  The two thread flags make
# libgcc's thread model single; here it is win32, a MinGW target's default
# (thread_file in gcc/config.gcc's i[34567]86-*-mingw* case), on the
# Windows API alone, as nixpkgs' MinGW-w64 has it
# (pkgs/os-specific/windows/mingw-w64/default.nix).  Exceptions are SJLJ,
# 4.6.4's default for a 32-bit MinGW target (DWARF2_UNWIND_INFO in
# gcc/config/i386/cygming.h).
# --disable-win32-registry is not nixpkgs': a GCC on Windows takes its
# directories from the registry wherever its configure can link advapi32
# (AC_ARG_ENABLE(win32-registry) in gcc/configure.ac), which under tcc it
# cannot only because TinyCC ships no import library for it.
#
# libgcc is compiled by the xgcc the same make links, from the gcc
# directory's generated headers and variables (libgcc/Makefile.in's
# include of libgcc.mvars).  Static, as --disable-shared has it, it links
# nothing: its configure's one link test fails for want of the runtime,
# after which a link test would be an error (config/no-executables.m4),
# and its later tests compile (the tests after AC_PROG_CC in
# libgcc/configure.ac), so it needs MinGW-w64's headers alone.  The guide
# builds it after the runtime for --enable-shared, under which it also
# links libgcc's DLL.  The top level runs xgcc with -B on the gcc
# directory and -isystem on the copy of MinGW-w64's headers
# (GCC_FOR_TARGET, and FLAGS_FOR_TARGET's build_tooldir directories, in
# configure.ac), and the driver turns the -B into an -isystem for GCC's
# own headers (%I in gcc/gcc.c's do_spec_1): GCC searches both before the
# directories the seed stdenv's C_INCLUDE_PATH names for tcc
# (register_include_chains in gcc/incpath.c), and every system header
# libgcc's sources include for this target is GCC's or MinGW-w64's.
#
# libgcc carries no debugging information, which would name the build
# directory and the store paths it read.  -g reaches its compiles from two
# places, and a setting removes each:
#
#   CFLAGS_FOR_TARGET=-O2: the target libraries' CFLAGS, which the top
#   level's configure takes from CFLAGS with -g added unless it is given,
#   -g -Dalloca=alloca here (CFLAGS_FOR_TARGET in configure.ac).  -O2 is
#   the optimization that default means to add, and the alloca macro is
#   for tcc alone.
#
#   LIBGCC2_DEBUG_CFLAGS=-g0: gcc's Makefile adds this variable, -g, to
#   every libgcc compile (LIBGCC2_DEBUG_CFLAGS and LIBGCC2_CFLAGS in
#   gcc/Makefile.in); -g0 is the value GCC's own picochip and arm/netbsd
#   fragments give it.  It is a make variable, which the top level passes
#   on to the gcc directory only when it is not empty (EXTRA_GCC_FLAGS in
#   Makefile.tpl).  Only the build reads it, when the gcc directory writes
#   libgcc.mvars (the libgcc.mvars rule in gcc/Makefile.in); makeFlags
#   reaches the install phase too, as nixpkgs' installPhase passes it.
#
# What follows is for TinyCC and its headers:
#
#   fix-tinycc-attribute.patch: the edit nixpkgs makes to binutils'
#   include/ansidecl.h, made to this copy of the file (see binutils).
#   Without it 25 calls in three objects pop the arguments a Windows API
#   function has already popped: libiberty's pex-win32.c, on every program
#   the driver runs, and lrealpath.c, and gcc/config/i386/host-mingw32.c.
#
#   CFLAGS=-Dalloca=alloca: libiberty's cp-demangle.c, gmp's gmp-impl.h
#   and mpfr's mpfr-gmp.h each declare alloca as returning char * unless it
#   is a macro, which tcc, declaring it itself, rejects, as the bundled
#   libintl does, hence --disable-nls.  binutils makes it a macro in
#   malloc.h, which the two libraries' headers never read.  A macro that
#   names itself leaves their calls to tcc's alloca; GCC's own sources take
#   libiberty's C_alloca, which libiberty.h gives every compiler that is
#   not GCC (the alloca macro in include/libiberty.h).
#
#   LDFLAGS=-luser32: libiberty's lrealpath.c calls CharLowerBuff, in
#   user32.dll.  GCC built for MinGW links user32 into every program
#   (LIB_SPEC in gcc/config/i386/mingw32.h); tcc links msvcrt and
#   kernel32.
#
#   --with-stage1-libs: libcpp reads a directory of precompiled headers
#   with opendir (pch_open_file in libcpp/files.c), which msvcrt.dll lacks
#   and mingw-w64-dirent supplies, with the dirent.h that buildInputs puts
#   before TinyCC's.  The flag's value reaches gcc's configure as HOST_LIBS
#   (HOST_EXPORTS in Makefile.tpl), which ends LIBS, after libcpp, and
#   comes before only zlib in BACKENDLIBS (gcc/Makefile.in), so it follows
#   libcpp on each link line there that names libcpp; the stdenv splits
#   configureFlags at spaces, so it carries the one library and LDFLAGS
#   the other.
#
# GCC's own makefile fragment for a MinGW host (config/mh-mingw) adds
# -D__USE_MINGW_ACCESS, under which TinyCC's io.h drops the X_OK that
# msvcrt.dll's access rejects (__mingw_access in win32/include/io.h), and
# a 12 MB stack, which tcc's linker reserves.  -march=native is not
# available: the driver's processor detection is compiled for GCC alone
# (gcc/config/i386/driver-i386.c).
#
# The programs import from msvcrt.dll, kernel32.dll and user32.dll alone,
# and nothing msvcrt.dll gained after Windows XP.
{
  seedStdenv,
  fetchurl,
  binutils,
  mingw-w64-dirent,
  mingw-w64-headers,
}:
let
  pname = "gcc-core";
  version = "4.6.4";
  gmpVersion = "4.3.2";
  mpfrVersion = "2.4.2";
  mpcVersion = "1.0.3";

  src = fetchurl {
    url = "mirror://gnu/gcc/gcc-${version}/gcc-core-${version}.tar.gz";
    hash = "sha256-5TSlywWrg51897JJb9XfQudjUpJsHPDZTedhhMJqc5w=";
  };
  gmpSrc = fetchurl {
    url = "mirror://gnu/gmp/gmp-${gmpVersion}.tar.gz";
    hash = "sha256-e+OtFkG5mxf2qL5ql28flU6ZfEHpGa1+DEGP6EjBPJc=";
  };
  mpfrSrc = fetchurl {
    url = "mirror://gnu/mpfr/mpfr-${mpfrVersion}.tar.gz";
    hash = "sha256-JG1+GEBIsfxI02lt0wLJd04k6SEgQiFUB0XlRkAitjc=";
  };
  mpcSrc = fetchurl {
    url = "mirror://gnu/mpc/mpc-${mpcVersion}.tar.gz";
    hash = "sha256-YX3sxuoJiJ+wjt4zCRegCxaAm424jCnDG/u0nL+I7MM=";
  };

  target = "i686-w64-mingw32";
in
seedStdenv.mkDerivation {
  name = "${pname}-${version}";

  inherit
    src
    gmpSrc
    mpfrSrc
    mpcSrc
    ;

  patches = [ ./fix-tinycc-attribute.patch ];
  gmpPatch = ./gmp-bases-digits.patch;

  buildInputs = [
    mingw-w64-dirent
    binutils
  ];

  preConfigure = ''
    tar xf "$gmpSrc"
    mv gmp-${gmpVersion} gmp
    (cd gmp && patch -p1 -i "$gmpPatch")
    tar xf "$mpfrSrc"
    mv mpfr-${mpfrVersion} mpfr
    tar xf "$mpcSrc"
    mv mpc-${mpcVersion} mpc
    mkdir ../build
    cd ../build
    configureScript=../gcc-${version}/configure
  '';

  configureFlags = [
    "--target=${target}"
    "--with-headers=${mingw-w64-headers}/include"
    "--with-as=${binutils}/bin/${target}-as.exe"
    "--with-ld=${binutils}/bin/${target}-ld.exe"
    "--enable-languages=c"
    "--disable-shared"
    "--disable-multilib"
    "--disable-lto"
    "--disable-plugin"
    "--disable-decimal-float"
    "--disable-nls"
    "--disable-win32-registry"
    "--disable-dependency-tracking"
    "--with-stage1-libs=${mingw-w64-dirent}/lib/libdirent.a"
    "LDFLAGS=-luser32"
    "CFLAGS=-Dalloca=alloca"
    "CFLAGS_FOR_TARGET=-O2"
  ];

  makeFlags = [ "LIBGCC2_DEBUG_CFLAGS=-g0" ];

  # make runs $NIX_BUILD_CORES jobs, as minimal-bootstrap/gcc/4.6.nix's
  # make does.
  enableParallelBuilding = true;
  buildPhase = "make \${enableParallelBuilding:+-j$NIX_BUILD_CORES} $makeFlags all-gcc all-target-libgcc";
  installPhase = "make \${enableParallelInstalling:+-j$NIX_BUILD_CORES} $makeFlags install-gcc install-target-libgcc";
}
// {
  srcs = [
    src
    gmpSrc
    mpfrSrc
    mpcSrc
  ];
  # The directories gmpSrc, mpfrSrc and mpcSrc unpack to are named for
  # these, and gcc46-cxx unpacks the same archives.
  inherit gmpVersion mpfrVersion mpcVersion;
}
