# GCC 10.4.0 with C and C++, as nixpkgs' minimal-bootstrap/gcc/10.nix
# builds gcc10 with gcc46-cxx (nixpkgs
# 5eb598da22ce76bc18ec85ecc1de66ca838b3514), for Windows: the C and C++
# drivers, cc1, cc1plus, cpp, collect2, libgcc and libstdc++, running on
# i686 Windows and writing assembly for i686-w64-mingw32, configured,
# compiled and installed in the seed stdenv by gcc46-cxx and binutils, and
# the target libraries by the compiler the same make links.  GCC 10 is the
# last GCC a C++98 compiler builds (gcc/doc/install.texi:241-250; GCC 11's
# asks for C++11), and gcc46-cxx's g++ is one.  The release is nixpkgs':
# 10.5.0 fails to build with GCC 4.6 (GCC bug 110716, which 10.nix names).
# nixpkgs then wraps the compiler, gcc10 over gcc10-unwrapped, to give it
# musl's headers and libraries; this one has MinGW-w64's in its own prefix
# (below), so the build is the rung, as gcc46-cxx is.
#
# GMP, MPFR and MPC, which GCC needs (gcc/doc/install.texi:376-405), are
# built in the tree, as 10.nix builds them: GCC's configure builds each it
# finds in a directory of its source named for it, and preConfigure
# unpacks and renames each there, as gcc-core does, there being no
# symbolic link.  The releases are nixpkgs': GMP 6.2.1, the last to
# compile with GCC 4.6 as 10.nix says, and MPFR 4.2.2 and MPC 1.4.1, which
# need GMP 5 and MPFR 4.1 (mpfr INSTALL:31, mpc configure.ac:174 and
# 196-197).  install.texi supports in the tree only the releases
# contrib/download_prerequisites fetches, GMP 6.1.0, MPFR 3.1.6 and MPC
# 1.0.3 (lines 30-32), each older than nixpkgs'.  nixpkgs builds the three
# as rungs of their own (gcc/gmp.nix, mpfr.nix and mpc.nix) for the GCC
# after this one alone, which links them by --with-gmp, --with-mpfr and
# --with-mpc, with this one as the compiler.  As in gcc-core, GCC's
# Makefile configures GMP for the host none-pc-mingw32, with no assembly
# (Makefile.def:50-60).
#
# MinGW-w64's headers and runtime are mingw-w64-headers' and
# mingw-w64-crt's, the v8.0.3 release gcc-core compiled, as nixpkgs' gcc10
# links the musl its gcc46 compiled (default.nix, musl and gcc10).  The
# release is the newest whose runtime 4.6.4 compiles (see
# mingw-w64-headers); a newer runtime is this compiler's to compile, since
# it has the three builtins those releases call.  libstdc++'s configure
# links its tests against the runtime (libstdc++-v3/crossconfig.m4:
# 195-201), so it has to be in place before the target libraries
# configure.
#
# The configuration is gcc46-cxx's, which says why of each: the stdenv's
# i686-pc-mingw32 is the build and the host and the target is binutils',
# so GCC takes itself for a cross compiler and its programs install as
# bin/i686-w64-mingw32-<name>.exe; it configures from a build directory
# beside its source; --with-as and --with-ld compile in the paths of
# binutils' assembler and linker; and --with-headers and --with-libs copy
# MinGW-w64's headers and runtime into this output
# (configure.ac:2487-2517 and 2561-2589), where the build points the
# compiler it links when it compiles and links the target libraries
# (configure.ac:3298-3303 and 3322-3325), and where the installed driver
# finds them.  No --with-sysroot, for gcc-core's reason: MPC 1.4.1's
# libtool, too, takes a sysroot for absolute only if it begins with '/'
# (mpc configure:7808-7824).  nixpkgs points its native compiler at
# musl's headers and libraries with --with-native-system-header-dir and
# --with-sysroot instead, and its sed of libstdc++-v3/configure.host is
# for musl, which libstdc++ does not recognize; for this target it reads
# os/mingw32-w64.
#
# libcpp-configure-ar.patch is the libcpp half of GCC's commit
# 731c4ce0e93065fb70db5faa2bd6c9c6bad56738, from GCC 11: libcpp's
# configure looks the archiver up as the other directories' do, where
# 10.4.0's Makefile names ar (libcpp/Makefile.in:28).  GCC builds libcpp
# for the build machine as well, for genmatch (configure.ac:127,
# gcc/Makefile.in:2868-2880), and the top level gives that make the AR it
# chose in the environment alone (Makefile.in:152 and 3290-3297), which
# the Makefile's own assignment overrides.  The ar on PATH is then
# busybox's applet, which takes no u modifier and makes no libcpp.a; the
# patched configure takes the AR the environment names.  The commit's
# libdecnumber half changes a directory built for the host alone, whose
# make the top level gives AR on its command line (Makefile.tpl:651-673).
#
# The build's compilers are gcc46-cxx's drivers and its archiver and
# linker binutils', each by the target's name, for gcc46-cxx's reason.
# CXX is new here: GCC 10 is written in C++, and the top level, the build
# being its own host, looks a C++ compiler up under bare names
# (configure.ac:1381), of which PATH has none.  The build machine's
# programs take the same compilers and flags (configure.ac:1363-1378 and
# 3312-3316, gcc/configure.ac:2331-2335).  preConfigure unsets
# C_INCLUDE_PATH, which holds TinyCC's headers for tcc and which GCC
# searches first, as gcc46-cxx's does.
#
# No program carries the time it was built, as in gcc46-cxx: LDFLAGS
# gives every link gcc46-cxx makes --no-insert-timestamp, which GCC 10's
# link spec for MinGW does not pass either (gcc/config/i386/mingw32.h:
# 139-146), and the install phase gives make a STRIP that keeps the zero
# ld wrote, which install-strip runs on each program it installs
# (gcc/Makefile.in:3521-3526, fixincludes/Makefile.in:190-191).
# configure, looking for strip under its bare name, finds none and
# records true (configure.ac:3468).
#
# CFLAGS and CXXFLAGS are nixpkgs' -O1, and the C++ target libraries' its
# -O0 without the -dynamic-linker it adds for musl, each given so that
# configure gives them no -g (configure.ac:2451-2483).  The C target
# libraries' are -O2, configure's default for them without the -g, where
# nixpkgs gives -O0.  gcc's Makefile hands libgcc CFLAGS_FOR_TARGET after
# libgcc's own -O2 (gcc/Makefile.in:455, libgcc/Makefile.in:247-249), and
# libgcc compiles its 64-bit division functions with -fexceptions and
# -fnon-call-exceptions (libgcc/Makefile.in:466-469 and 533-544).  At -O0
# __divdi3, __moddi3 and __umoddi3 then register with the SJLJ unwinder,
# and at -O2 none does.  Every C program here calls __udivdi3 and
# __umoddi3 through MinGW-w64's printf, which v8.0.3 gives C99 and later
# (mingw-w64-headers/crt/_mingw.h.in:419-431, stdio.h:354-358), GCC 10's
# default C being C17, from libmingwex, which the link reaches after the
# first -lgcc.  At -O0 the second -lgcc takes in the unwinder, which
# needs kernel32.dll's TLS functions and libmingw32's __mingwthr_key_dtor,
# whose libraries come before it (gcc/config/i386/mingw32.h:90-96 and
# 164-168), and no C program links.
#
# libgcc's objects take -g all the same.  GCC 10 sets the variable that adds
# it, which gcc46-cxx sets to -g0 on make's command line, in libgcc's own
# Makefile (libgcc/Makefile.in:246-249), after the CFLAGS the top level
# gives libgcc (lines 293-298), and the top level passes none of its command
# line's variables on (Makefile.tpl:2030-2032).  The debugging information
# names the build directory, which is named for this output's hash, and a
# program this compiler links takes in the objects of libgcc's it calls, so
# the install phase removes it with strip --strip-debug, as install-strip
# has libtool do for libstdc++'s archives and as nixpkgs' strip hook does
# for every library it installs
# (pkgs/build-support/setup-hooks/strip.sh).  It strips the three files
# libgcc compiles with those flags and installs, libgcc.a, libgcov.a and
# crtfastmath.o (libgcc/config.host:840-866 and config/t-crtfm); crtbegin.o
# and crtend.o are compiled with -g0 (libgcc/Makefile.in:301).  strip keeps
# an archive's symbol index unless it strips every symbol
# (binutils/objcopy.c:3632-3635), so the archives need no ranlib after it,
# which nixpkgs' hook runs on every archive it strips.
#
# --disable-win32-registry is not nixpkgs', for gcc46-cxx's reason: GCC 10
# for Windows looks its directories up under HKEY_LOCAL_MACHINE\SOFTWARE\
# Free Software Foundation\10.4.0 (gcc/prefix.c:40-49) wherever its
# configure links RegOpenKeyExA (gcc/configure.ac:2104-2135), and it
# links MinGW-w64's libadvapi32.a.  nixpkgs' other flags mean here what
# they mean there, but for --disable-libmpx, left out, which names a
# library 10.4.0's tree does not have; --disable-serial-configure is
# configure's default (configure.ac:2948-2958).  --disable-decimal-float,
# which 4.6.cxx.nix leaves out and 10.nix passes, turns off decimal float,
# which is on for a MinGW target (config/dfp.m4:21-27).  The thread model
# is win32 and exceptions are SJLJ, a 32-bit MinGW target's defaults in
# GCC 10 too (gcc/config.gcc:2220-2224, gcc/config/i386/cygming.h:365-376,
# libgcc/config.host:845-858).  --disable-shared, as in nixpkgs, makes
# libgcc and libstdc++ static, so a program carries what it uses of them
# and imports no DLL of GCC's.
{
  seedStdenv,
  fetchurl,
  binutils,
  gcc46-cxx,
  mingw-w64-crt,
  mingw-w64-headers,
}:
let
  pname = "gcc";
  version = "10.4.0";
  gmpVersion = "6.2.1";
  mpfrVersion = "4.2.2";
  mpcVersion = "1.4.1";

  src = fetchurl {
    url = "mirror://gnu/gcc/gcc-${version}/gcc-${version}.tar.xz";
    hash = "sha256-ySl9W818tD89/C/tU4npSMkxL9li72pM5FXP+WPr5PE=";
  };
  gmpSrc = fetchurl {
    url = "mirror://gnu/gmp/gmp-${gmpVersion}.tar.xz";
    hash = "sha256-/UgpkSzd0S+EGBw0Ucx1K+IkZD6H+sSXtp7d2txJtPI=";
  };
  mpfrSrc = fetchurl {
    url = "mirror://gnu/mpfr/mpfr-${mpfrVersion}.tar.xz";
    hash = "sha256-tnugOD736KhWNzTi6InvXsPDuJigHQD6CmhprYHGzgE=";
  };
  mpcSrc = fetchurl {
    url = "mirror://gnu/mpc/mpc-${mpcVersion}.tar.xz";
    hash = "sha256-kSBM0y8WS9O3yZLUpqjOZRlRGq2rMPeLaYLQv41z6TE=";
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

  patches = [ ./libcpp-configure-ar.patch ];

  buildInputs = [
    gcc46-cxx
    binutils
  ];

  preConfigure = ''
    tar xf "$gmpSrc"
    mv gmp-${gmpVersion} gmp
    tar xf "$mpfrSrc"
    mv mpfr-${mpfrVersion} mpfr
    tar xf "$mpcSrc"
    mv mpc-${mpcVersion} mpc
    unset C_INCLUDE_PATH
    mkdir ../build
    cd ../build
    configureScript=../gcc-${version}/configure
  '';

  configureFlags = [
    "--target=${target}"
    "--with-headers=${mingw-w64-headers}/include"
    "--with-libs=${mingw-w64-crt}/lib"
    "--with-as=${binutils}/bin/${target}-as.exe"
    "--with-ld=${binutils}/bin/${target}-ld.exe"
    "--enable-languages=c,c++"
    "--enable-checking=release"
    "--disable-serial-configure"
    "--disable-analyzer"
    "--disable-bootstrap"
    "--disable-dependency-tracking"
    "--disable-decimal-float"
    "--disable-libsanitizer"
    "--disable-libssp"
    "--disable-libgomp"
    "--disable-libquadmath"
    "--disable-libitm"
    "--disable-libvtv"
    "--disable-libatomic"
    "--disable-libstdcxx-pch"
    "--disable-lto"
    "--disable-multilib"
    "--disable-nls"
    "--disable-plugin"
    "--without-isl"
    "--disable-libstdcxx-filesystem-ts"
    "--disable-shared"
    "--disable-win32-registry"
    "CC=${target}-gcc"
    "CXX=${target}-g++"
    "AR=${target}-ar"
    "LD=${target}-ld"
    "CFLAGS=-O1"
    "CXXFLAGS=-O1"
    "CFLAGS_FOR_TARGET=-O2"
    "CXXFLAGS_FOR_TARGET=-O0"
    "LDFLAGS=-Wl,--no-insert-timestamp"
  ];

  # make runs $NIX_BUILD_CORES jobs and installs with install-strip, as
  # 10.nix's make does, stripping with -p (see above), and strip then
  # removes the debugging information from libgcc's three files.
  enableParallelBuilding = true;
  installPhase = ''
    make ''${enableParallelInstalling:+-j$NIX_BUILD_CORES} 'STRIP=${target}-strip -p' install-strip
    libgccDir=$out/lib/gcc/${target}/${version}
    ${target}-strip -p --strip-debug "$libgccDir/libgcc.a" "$libgccDir/libgcov.a" "$libgccDir/crtfastmath.o"
  '';
}
// {
  srcs = [
    src
    gmpSrc
    mpfrSrc
    mpcSrc
  ];
}
