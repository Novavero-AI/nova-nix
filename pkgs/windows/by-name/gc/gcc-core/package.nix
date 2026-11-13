# GCC 4.6.4's C compiler, as nixpkgs' minimal-bootstrap/gcc/4.6.nix builds
# GCC with tinycc (nixpkgs 5eb598da22ce76bc18ec85ecc1de66ca838b3514), for
# Windows: the driver, cc1, cpp, collect2, the specs and GCC's own headers,
# running on i686 Windows and writing assembly for i686-w64-mingw32,
# configured, compiled and installed in the seed stdenv by tinycc-native.
# It is what make all-gcc builds and make install-gcc installs, the compiler
# MinGW-w64's build guide installs before the runtime exists
# (mingw-w64-doc/howto-build, "Building the GCC core cross-compiler").
# libgcc and MinGW-w64's runtime are the next rung's, which this compiler
# compiles, so nothing it compiles links yet.
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
# i686-w64-mingw32/sys-include (configure.ac:2244-2255 and 2318-2342), a
# cross compiler's own place for its target's headers.  The build writes a
# limits.h that goes on to MinGW-w64's only if it finds one there
# (gcc/Makefile.in:4090-4094), and the compiler searches the directory for
# every source, after its own headers (gcc/cppdefault.c).  Libraries it
# takes from its own directories and those -B names, and no others
# (gcc/gcc.c:3884-3899 and 6395): a cross driver reads no LIBRARY_PATH
# (gcc.c:3681-3682).
#
# GCC's manual prefers --with-sysroot, which names the headers where they
# are, and nixpkgs gives one to its cross compilers.  No store path can be
# one here.  The top-level hands each option to every directory's
# configure; mpc's is written by libtool 2.4, which has a --with-sysroot of
# its own and takes a path for absolute only if it begins with '/'
# (mpc/configure:6776-6799), and a store path on Windows begins with a
# drive letter.  --with-headers is read by the top-level and by gcc's
# configure alone.  nixpkgs' --with-native-system-header-dir, an option GCC
# has from 4.7 on, its --with-build-sysroot and its no-system-headers.patch
# point its native compiler at its C library's headers.
#
# --with-as and --with-ld name binutils' assembler and linker by their
# files.  A cross GCC runs the as and ld in its own prefix and then any on
# PATH, and binutils is another output, so the paths are compiled into the
# driver and collect2 (gcc/configure.ac:193-257) and configure's tests of
# both tools run the same files.
#
# gmp, mpfr and mpc are built in the tree, as in nixpkgs, which links each
# under the name configure looks for; preConfigure unpacks and renames
# them, there being no symbolic link.  GCC's Makefile has gmp configured
# for the host none-pc-mingw32, in C alone (Makefile.def:63-71).  gmp's
# libtool archives libgmp.a's 402 objects in one command of some 7,400
# characters, under the 8192 it takes for the limit on a MinGW build host;
# past that it archives in pieces, which tcc -ar, writing an archive anew
# each time, cannot.
#
# GCC configures from a build directory beside its source, as its
# installation guide recommends (INSTALL/configure.html), where nixpkgs
# configures in the source.  In the source, the top level builds the
# compiler a directory deeper, in host-i686-pc-mingw32/gcc, and gcc's
# Makefile then names the build machine's fixincl by two paths that
# differ: its rule that takes the program as built (gcc/Makefile.in:4154)
# and the prerequisite that needs it (4160).  The program is fixincl.exe,
# so neither path is a file, and make links fixincl.o alone by its
# built-in rule.
#
# Of nixpkgs' other flags, those passed mean here what they mean there.
# --enable-checking=release is the default for a release
# (gcc/configure.ac:356-374).  The --disable-lib* flags name target
# libraries, which all-gcc never configures, five of them later than 4.6.4.
# --disable-lto-plugin follows from --disable-lto for a MinGW target (the
# top-level configure.ac:1757-1762).  The two thread flags choose what
# libgcc is compiled with, the next rung's choice.  The three configure
# answers it exports are for the target libraries too.  install-strip has
# nothing to strip: tcc writes no symbol table into a program.
# --disable-win32-registry is not nixpkgs': a GCC on Windows takes its
# directories from the registry wherever its configure can link advapi32
# (gcc/configure.ac:1572-1600), which under tcc it cannot only because
# TinyCC ships no import library for it.
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
#   not GCC (include/libiberty.h:649-663).
#
#   LDFLAGS=-luser32: libiberty's lrealpath.c calls CharLowerBuff, in
#   user32.dll.  GCC built for MinGW links user32 into every program
#   (gcc/config/i386/mingw32.h:78-80); tcc links msvcrt and kernel32.
#
#   --with-stage1-libs: libcpp reads a directory of precompiled headers
#   with opendir (libcpp/files.c:305-330), which msvcrt.dll lacks and
#   mingw-w64-dirent supplies, with the dirent.h that buildInputs puts
#   before TinyCC's.  The flag's value ends each link line GCC's Makefile
#   writes (gcc/Makefile.in:1053-1055); the stdenv splits configureFlags at
#   spaces, so it carries the one library and LDFLAGS the other.
#
# GCC's own makefile fragment for a MinGW host (config/mh-mingw) adds
# -D__USE_MINGW_ACCESS, under which TinyCC's io.h drops the X_OK that
# msvcrt.dll's access rejects (win32/include/io.h:396-404), and a 12 MB
# stack, which tcc's linker reserves.  -march=native is not available:
# the driver's processor detection is compiled for GCC alone
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

  buildInputs = [
    mingw-w64-dirent
    binutils
  ];

  preConfigure = ''
    tar xf "$gmpSrc"
    mv gmp-${gmpVersion} gmp
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
  ];

  buildPhase = "make all-gcc";
  installPhase = "make install-gcc";
}
// {
  srcs = [
    src
    gmpSrc
    mpfrSrc
    mpcSrc
  ];
}
