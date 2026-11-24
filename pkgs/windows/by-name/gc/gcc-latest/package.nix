# GCC 16.2.0 with C and C++, the GCC nixpkgs' minimal-bootstrap builds
# with gcc10 as gcc-latest (nixpkgs
# 5eb598da22ce76bc18ec85ecc1de66ca838b3514, gcc/common.nix, ng.nix,
# libgcc.nix and libstdcxx.nix), for Windows: the C and C++ drivers, cc1,
# cc1plus, cpp, collect2, libgcc and libstdc++, running on i686 Windows
# and writing assembly for i686-w64-mingw32, configured, compiled and
# installed in the seed stdenv by gcc10 and binutils, and the target
# libraries by the compiler the same make links, against MinGW-w64
# v14.0.0's headers and the runtime gcc10 compiled.  GCC 16 needs a C++14
# compiler to build (the ISO C++14 compiler item of gcc/doc/install.texi's
# Prerequisites), and gcc10's g++ is one.
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
# output (configure.ac's handling of --with-headers and --with-libs) and
# points the compiler it links at when it builds the target libraries
# (FLAGS_FOR_TARGET's *-w64-mingw* case and build_tooldir directories in
# configure.ac), so the same make builds libgcc and libstdc++, they install
# beside the runtime, and the installed driver finds all of them with no
# flag and no wrapper.  Neither patch has anything to do here: the first
# acts only when gccng_skip_target_lib_config is set, and the second moves
# the computation of libgcc's install directory from its Makefile to its
# configure, for a libgcc built apart from GCC's top level.
#
# GMP, MPFR and MPC are built in the tree, as gcc10 builds them: GCC's
# configure builds each it finds in a directory of its source named for
# it (the GMP, MPFR and MPC items of gcc/doc/install.texi's Prerequisites),
# and preConfigure unpacks and renames each there.  The releases are
# nixpkgs', GMP 6.3.0, MPFR 4.2.2 and MPC 1.3.1, the ones
# contrib/download_prerequisites fetches for an in-tree build of 16.2.0 (its
# gmp, mpfr and mpc variables).  nixpkgs links its own by --with-gmp,
# --with-mpfr and --with-mpc, with -rpath to each for the dynamic linker;
# GCC's Makefile configures the in-tree ones with --disable-shared (the gmp,
# mpfr and mpc host_modules in Makefile.def), so cc1 and cc1plus carry them,
# and import no DLL a Windows loader would have to find.
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
# spec does not pass either (LINK_SPEC in gcc/config/i386/mingw-w64.h); and
# the install gives make a STRIP that keeps the zero ld wrote.  nixpkgs
# names musl's headers with --with-native-system-header-dir and
# --with-sysroot=/ and passes --without-headers, none of which a
# compiler given its headers with --with-headers takes.
#
# WINDRES is new here, binutils' windres by the target's name.  GCC 16
# compiled for Windows embeds a manifest in its driver and compilers that
# sets their active code page to UTF-8 (AC_ARG_ENABLE(win32-utf8-manifest)
# in gcc/configure.ac, gcc/config/i386/x-mingw32-utf8), which windres
# compiles from a resource script, and the top level, the build being its
# own host, looks windres up under its bare name
# (NCN_STRICT_CHECK_TOOLS(WINDRES, windres) in configure.ac), as it does the
# compilers, and hands what it finds to gcc's make (HOST_EXPORTS in
# Makefile.tpl).  PATH has no windres, and without the flag the build stops
# there.
#
# CFLAGS and CXXFLAGS are ng.nix's -O1, and the C++ target library's
# libstdcxx.nix's -O0, without the -B flags and -dynamic-linker it gives
# its build apart.  The C target library's are -O2, configure's default
# for it without the -g, where libgcc.nix gives -O0, for gcc10's reason:
# libgcc compiles its 64-bit division functions with -fexceptions and
# -fnon-call-exceptions (LIB2_DIVMOD_EXCEPTION_FLAGS in libgcc/Makefile.in),
# after the flags given (LIBGCC2_CFLAGS in libgcc/Makefile.in, GCC_CFLAGS
# and the libgcc.mvars rule in gcc/Makefile.in), and at -O0 those register
# with the SJLJ unwinder, which a C program then takes in after the
# libraries the unwinder needs.  libgcc's own Makefile adds -g
# (LIBGCC2_DEBUG_CFLAGS in libgcc/Makefile.in), which no variable the top
# level passes reaches, so the install phase strips the debugging
# information from what libgcc installs, as libgcc.nix strips every file it
# installs: it names the build directory, which is named for this output's
# hash, in each program that links those objects.
#
# The build phase first deletes TOPLEVEL_CONFIGURE_ARGUMENTS from the
# top-level Makefile, as ng.nix's sed does.  The Makefile hands the
# variable to gcc's configure (TOPLEVEL_CONFIGURE_ARGUMENTS and HOST_EXPORTS
# in Makefile.tpl), which writes it into configargs.h (gcc_config_arguments
# in gcc/configure.ac) as the "Configured with" line gcc -v prints, so the
# compiler would name the store paths of the headers and runtime it holds
# copies of, and of binutils.
#
# ng.nix's other flags mean here what they mean there, but for
# --enable-default-pie, left out: GCC turns PIC off for 32-bit Windows
# code (SUBTARGET_OVERRIDE_OPTIONS in gcc/config/i386/cygming.h), so the
# flag would change only the -pie the driver passes every link
# (LINK_PIE_SPEC in gcc.cc), given which binutils' ld writes an empty export
# table into a program, already relocatable without it since dynamic base is
# ld's default; and --without-included-gettext and --disable-plugins, which
# no configure of 16.2.0's tree reads.  --enable-linker-build-id has the
# driver pass every link --build-id (gcc_cv_ld_buildid and
# AC_ARG_ENABLE(linker-build-id) in gcc/configure.ac, LINK_BUILDID_SPEC in
# gcc.cc), for which ld writes a CodeView entry holding a hash of the
# program into its debug directory.  libgcc.nix's and libstdcxx.nix's
# --disable-vtable-verify, --enable-libstdcxx-visibility,
# --with-default-libstdcxx-abi=new and --disable-libstdcxx-pch reach
# libstdc++'s configure from the top level here.  Their thread models,
# single for libgcc and posix for libstdc++, are their builds' for musl;
# this target's is win32 (thread_file in the mingw case of gcc/config.gcc),
# for which libstdc++ has std::thread once _WIN32_WINNT names Vista or later
# (__GTHREADS_CXX0X in libgcc/config/i386/gthr-win32.h), as
# mingw-w64-headers-latest's does.  Their --enable-clocale=gnu names the
# locale model libstdc++ builds on glibc's locale functions
# (GLIBCXX_ENABLE_CLOCALE in libstdc++-v3/acinclude.m4), and this target's
# is generic.  The target libraries nixpkgs does not build, libgomp,
# libquadmath, libatomic, libitm, libsanitizer, libvtv and libssp, are
# turned off, as gcc10 turns them off.  As in gcc10,
# --disable-win32-registry is not nixpkgs', --disable-shared makes libgcc
# and libstdc++ static, and exceptions are SJLJ, a 32-bit MinGW target's
# default (DWARF2_UNWIND_INFO in gcc/config/i386/cygming.h, tmake_eh_file in
# the i[34567]86-*-mingw* case of libgcc/config.host).
#
# The install phase removes what ng.nix removes after install-strip: the
# install-tools directories, which hold fixincludes and what it needs to
# run again, and libstdc++'s Python pretty-printers and the manual and
# info pages, which nothing downstream reads.  It removes as well the
# script libstdc++ installs beside its library for gdb, which loads the
# printers from the directory they were removed from
# (pythondir and install-data-local in libstdc++-v3/python/Makefile.am), and
# then the directories the removals leave empty, with rmdir, which fails the
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
