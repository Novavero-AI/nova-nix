# GCC 16.2.0 with C and C++, the GCC nixpkgs' minimal-bootstrap builds
# with gcc10 as gcc-latest (nixpkgs
# 5eb598da22ce76bc18ec85ecc1de66ca838b3514, gcc/common.nix, ng.nix,
# libgcc.nix and libstdcxx.nix), for Windows: the C and C++ drivers, cc1,
# cc1plus, cpp, collect2, libgcc and libstdc++, running on i686 Windows
# and writing assembly for i686-w64-mingw32, configured, compiled and
# installed in the seed stdenv by gcc10 and binutils, and the target
# libraries by the compiler the same make links, against MinGW-w64
# v14.0.0's headers and the runtime gcc10 compiled.  GCC 16 needs a C++14
# compiler to build (gcc/doc/install.texi:225-230), and gcc10's g++ is
# one.
#
# nixpkgs builds this GCC in pieces, each a derivation of its own: GMP,
# MPFR and MPC (gcc/gmp.nix, mpfr.nix and mpc.nix), libiberty and
# libbacktrace, the compiler with its target libraries left unconfigured
# (ng.nix, with configure-skip-target-libs-hooks.patch), libgcc
# configured by hand from GCC's own directory (libgcc.nix, with
# regular-libdir-includedir.patch), libstdc++ (libstdcxx.nix), and a
# wrapper that hands the compiler musl's, libgcc's and libstdc++'s
# directories (wrapper.nix).  Here the compiler is one build, as gcc10 is:
# a cross compiler given its target's headers and runtime with
# --with-headers and --with-libs, which GCC's top level copies into this
# output (configure.ac:3042-3070) and points the compiler it links at
# when it builds the target libraries (configure.ac:3949-3980), so the
# same make builds libgcc and libstdc++, they install beside the runtime,
# and the installed driver finds all of them with no flag and no
# wrapper.  Neither patch has anything to do here: the first acts only
# when gccng_skip_target_lib_config is set, and the second moves the
# computation of libgcc's install directory from its Makefile to its
# configure, for a libgcc built apart from GCC's top level.
#
# GMP, MPFR and MPC are built in the tree, as gcc10 builds them: GCC's
# configure builds each it finds in a directory of its source named for
# it (gcc/doc/install.texi:438-470), and preConfigure unpacks and renames
# each there.  The releases are nixpkgs', GMP 6.3.0, MPFR 4.2.2 and MPC
# 1.3.1, the ones contrib/download_prerequisites fetches for an in-tree
# build of 16.2.0 (lines 30-32).  nixpkgs links its own by --with-gmp,
# --with-mpfr and --with-mpc, with -rpath to each for the dynamic linker;
# GCC's Makefile configures the in-tree ones with --disable-shared
# (Makefile.def:52-69), so cc1 and cc1plus carry them, and import no DLL
# a Windows loader would have to find.
#
# The configuration is gcc10's, which says why of each: the stdenv's
# i686-pc-mingw32 is the build and the host and the target is binutils',
# so GCC takes itself for a cross compiler; it configures from a build
# directory beside its source; --with-as and --with-ld compile in the
# paths of binutils' assembler and linker; no --with-sysroot, which MPC's
# libtool takes for absolute only if it begins with '/'; the build's
# compilers are gcc10's drivers and its archiver and linker binutils',
# each by the target's name; preConfigure unsets C_INCLUDE_PATH; LDFLAGS
# gives every link gcc10 makes --no-insert-timestamp, which GCC 16's link
# spec does not pass either (gcc/config/i386/mingw-w64.h:122-131); and
# the install gives make a STRIP that keeps the zero ld wrote.  nixpkgs
# names musl's headers with --with-native-system-header-dir and
# --with-sysroot=/ and passes --without-headers, none of which a
# compiler given its headers with --with-headers takes.
#
# WINDRES is new here, binutils' windres by the target's name.  GCC 16
# compiled for Windows embeds a manifest in its driver and compilers that
# sets their active code page to UTF-8 (gcc/configure.ac:1921-1936,
# gcc/config/i386/x-mingw32-utf8), which windres compiles from a resource
# script, and the top level, the build being its own host, looks windres
# up under its bare name (configure.ac:4140), as it does the compilers,
# and hands what it finds to gcc's make (Makefile.tpl:228).  PATH has no
# windres, and without the flag the build stops there.
#
# CFLAGS and CXXFLAGS are ng.nix's -O1, and the C++ target library's
# libstdcxx.nix's -O0, without the -B flags and -dynamic-linker it gives
# its build apart.  The C target library's are -O2, configure's default
# for it without the -g, where libgcc.nix gives -O0, for gcc10's reason:
# libgcc compiles its 64-bit division functions with -fexceptions and
# -fnon-call-exceptions (libgcc/Makefile.in:546-557), after the flags
# given (libgcc/Makefile.in:248-249, gcc/Makefile.in:505 and 2525), and
# at -O0 those register with the SJLJ unwinder, which a C program then
# takes in after the libraries the unwinder needs.  libgcc's own Makefile
# adds -g (libgcc/Makefile.in:247), which no variable the top level
# passes reaches, so the install phase strips the debugging information
# from what libgcc installs, as libgcc.nix strips every file it installs:
# it names the build directory, which is named for this output's hash, in
# each program that links those objects.
#
# The build phase first deletes TOPLEVEL_CONFIGURE_ARGUMENTS from the
# top-level Makefile, as ng.nix's sed does.  The Makefile hands the
# variable to gcc's configure (Makefile.tpl:102 and 246), which writes it
# into configargs.h (gcc/configure.ac:2242-2264) as the "Configured with"
# line gcc -v prints, so the compiler would name the store paths of the
# headers and runtime it holds copies of, and of binutils.
#
# ng.nix's other flags mean here what they mean there, but for
# --enable-default-pie, left out: GCC turns PIC off for 32-bit Windows
# code (gcc/config/i386/cygming.h:197-204), so the flag would change only
# the -pie the driver passes every link (gcc.cc:1073-1081), given which
# binutils' ld writes an empty export table into a program, already
# relocatable without it since dynamic base is ld's default; and
# --without-included-gettext and --disable-plugins, which no configure of
# 16.2.0's tree reads.  --enable-linker-build-id has the driver pass
# every link --build-id (gcc/configure.ac:6811-6844, gcc.cc:1084-1087),
# for which ld writes a CodeView entry holding a hash of the program into
# its debug directory.  libgcc.nix's and libstdcxx.nix's
# --disable-vtable-verify, --enable-libstdcxx-visibility,
# --with-default-libstdcxx-abi=new and --disable-libstdcxx-pch reach
# libstdc++'s configure from the top level here.  Their thread models,
# single for libgcc and posix for libstdc++, are their builds' for musl;
# this target's is win32 (gcc/config.gcc:2360-2366), for which libstdc++
# has std::thread once _WIN32_WINNT names Vista or later
# (libgcc/config/i386/gthr-win32.h:64-68), as mingw-w64-headers-latest's
# does.  Their --enable-clocale=gnu names the locale model libstdc++
# builds on glibc's locale functions (libstdc++-v3/acinclude.m4:
# 2805-2838), and this target's is generic.  The target libraries
# nixpkgs does not build, libgomp, libquadmath, libatomic, libitm,
# libsanitizer, libvtv and libssp, are turned off, as gcc10 turns them
# off.  As in gcc10, --disable-win32-registry is not nixpkgs',
# --disable-shared makes libgcc and libstdc++ static, and exceptions are
# SJLJ, a 32-bit MinGW target's default (gcc/config/i386/cygming.h:
# 358-369, libgcc/config.host:900-906).
#
# The install phase removes what ng.nix removes after install-strip: the
# install-tools directories, which hold fixincludes and what it needs to
# run again, and libstdc++'s Python pretty-printers and the manual and
# info pages, which nothing downstream reads.  It removes as well the
# script libstdc++ installs beside its library for gdb, which loads the
# printers from the directory they were removed from
# (libstdc++-v3/python/Makefile.am:29 and 46-62), and then the
# directories the removals leave empty, with rmdir, which fails the
# build if anything else is in them.
{
  seedStdenv,
  fetchurl,
  binutils,
  gcc10,
  mingw-w64-crt-latest,
  mingw-w64-headers-latest,
}:
let
  pname = "gcc";
  version = "16.2.0";
  gmpVersion = "6.3.0";
  mpfrVersion = "4.2.2";
  mpcVersion = "1.3.1";

  src = fetchurl {
    url = "mirror://gnu/gcc/gcc-${version}/gcc-${version}.tar.xz";
    hash = "sha256-5nOOKVl/czJwcxqpBgDzf/3ARQed/CfsfoGSzIEIXD4=";
  };
  gmpSrc = fetchurl {
    url = "mirror://gnu/gmp/gmp-${gmpVersion}.tar.xz";
    hash = "sha256-o8K4AgG4nmhhb0rTC8Zq7kknw85Q4zkpyoGdXENTiJg=";
  };
  mpfrSrc = fetchurl {
    url = "mirror://gnu/mpfr/mpfr-${mpfrVersion}.tar.xz";
    hash = "sha256-tnugOD736KhWNzTi6InvXsPDuJigHQD6CmhprYHGzgE=";
  };
  mpcSrc = fetchurl {
    url = "mirror://gnu/mpc/mpc-${mpcVersion}.tar.gz";
    hash = "sha256-q2QkkvXPiCt0qgy3MM1BCoHtzb7IlRg86TDnBsHHWbg=";
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

  buildInputs = [
    gcc10
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
    "--with-headers=${mingw-w64-headers-latest}/include"
    "--with-libs=${mingw-w64-crt-latest}/lib"
    "--with-as=${binutils}/bin/${target}-as.exe"
    "--with-ld=${binutils}/bin/${target}-ld.exe"
    "--with-gnu-as"
    "--with-gnu-ld"
    "--enable-languages=c,c++"
    "--enable-fast-install"
    "--enable-linker-build-id"
    "--enable-libstdcxx-visibility"
    "--with-default-libstdcxx-abi=new"
    "--disable-serial-configure"
    "--disable-analyzer"
    "--disable-bootstrap"
    "--disable-dependency-tracking"
    "--disable-decimal-float"
    "--disable-gcov"
    "--disable-install-libiberty"
    "--disable-libatomic"
    "--disable-libgomp"
    "--disable-libitm"
    "--disable-libquadmath"
    "--disable-libsanitizer"
    "--disable-libssp"
    "--disable-libstdcxx-pch"
    "--disable-libvtv"
    "--disable-lto"
    "--disable-multilib"
    "--disable-nls"
    "--disable-plugin"
    "--disable-shared"
    "--disable-vtable-verify"
    "--disable-win32-registry"
    "--without-isl"
    "CC=${target}-gcc"
    "CXX=${target}-g++"
    "AR=${target}-ar"
    "LD=${target}-ld"
    "WINDRES=${target}-windres"
    "CFLAGS=-O1"
    "CXXFLAGS=-O1"
    "CFLAGS_FOR_TARGET=-O2"
    "CXXFLAGS_FOR_TARGET=-O0"
    "LDFLAGS=-Wl,--no-insert-timestamp"
  ];

  # make runs $NIX_BUILD_CORES jobs and installs with install-strip, as
  # ng.nix's make does, stripping with -p (see gcc10), and strip then
  # removes the debugging information from what libgcc installs.
  enableParallelBuilding = true;
  buildPhase = ''
    sed -e '/TOPLEVEL_CONFIGURE_ARGUMENTS=/d' -i Makefile
    make ''${enableParallelBuilding:+-j$NIX_BUILD_CORES}
  '';
  installPhase = ''
    make ''${enableParallelInstalling:+-j$NIX_BUILD_CORES} 'STRIP=${target}-strip -p' install-strip
    libgccDir=$out/lib/gcc/${target}/${version}
    ${target}-strip -p --strip-debug "$libgccDir"/*.a "$libgccDir"/*.o
    rm -r "$out/libexec/gcc/${target}/${version}/install-tools" "$libgccDir/install-tools"
    rm -rf "$out/share/gcc-${version}/python" "$out/share/man" "$out/share/info"
    rm "$out/${target}/lib/libstdc++.a-gdb.py"
    rmdir "$out/share/gcc-${version}" "$out/share"
  '';
}
// {
  # gcc10 lists the MPFR archive the two share.
  srcs = [
    src
    gmpSrc
    mpcSrc
  ];
}
