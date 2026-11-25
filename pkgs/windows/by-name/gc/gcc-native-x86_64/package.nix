# GCC 16.2.0 with C and C++ that runs on x86_64 Windows and writes
# x86_64-w64-mingw32, as nixpkgs builds a GCC whose host is its target with
# a cross compiler of its own version (nixpkgs
# 5eb598da22ce76bc18ec85ecc1de66ca838b3514, gccForMajorMinorVersion in
# pkgs/development/compilers/gcc/all.nix, with gcc/default.nix and
# common/configure-flags.nix): the C and C++ drivers, cc1, cc1plus, cpp,
# collect2, gcov, libgcc and libstdc++, configured, compiled and installed
# in the seed stdenv by gcc-x86_64 and binutils-x86_64, with the programs
# the build runs itself compiled by gcc-latest, against MinGW-w64 v14.0.0's
# headers and the runtime gcc-core-x86_64 compiled.  It runs as gcc and g++,
# the drivers the package set's stdenv wraps
# (pkgs/windows/stdenv/wrap.sh), and its assembler and linker are
# binutils-native-x86_64's.
#
# The build is a Canadian cross, as binutils-native-x86_64's is: the
# stdenv's i686-pc-mingw32 is the build, and x86_64-w64-mingw32 the host
# and the target, so the programs install under their own names
# (the program_transform_name reset before AC_CANONICAL_TARGET in
# configure.ac), and the drivers and gcc-ar, gcc-nm and gcc-ranlib under the
# target's name as well (the install-driver and install-gcc-ar rules in
# gcc/Makefile.in, c++.install-common in gcc/cp/Make-lang.in).  gcc-x86_64's
# drivers and binutils-x86_64's archiver, linker and resource compiler build
# the host's programs, named by their target as in binutils-native-x86_64.
# A Canadian cross builds its target libraries with the cross compiler it
# finds installed, not the one it links (GCC_TARGET_TOOL in config/acx.m4),
# and the top level finds gcc-x86_64's under the target's name on PATH for
# libgcc and libstdc++ (the NCN_STRICT_CHECK_TARGET_TOOLS and
# ACX_CHECK_INSTALLED_TARGET_TOOL calls in configure.ac).  nixpkgs builds
# such a GCC with a cross compiler of its own version, because the target
# libraries use what the compiler that builds them knows (the stdenv
# gccForMajorMinorVersion picks in all.nix), and gcc-x86_64 is 16.2.0.  With
# the build not the host, the top level takes the build machine's tools
# under their bare names unless told otherwise (the *_FOR_BUILD defaults in
# configure.ac): busybox supplies an ar and nothing supplies gcc, g++ or
# ranlib, so CC_FOR_BUILD, CXX_FOR_BUILD, AR_FOR_BUILD and RANLIB_FOR_BUILD
# name gcc-latest's drivers and binutils' archiver and ranlib by their
# target's name, i686-w64-mingw32.  They build GCC's generator programs, the
# libiberty and libcpp those link, fixincludes, whose script for a MinGW
# target fixes nothing (the *-mingw32* case of fixincludes/mkfixinc.sh), and
# GMP's table generators, which the top level hands CC_FOR_BUILD.
#
# The build runs some of the x86_64 programs it links.  The gcc directory
# writes its specs with the new driver (the $(SPECS) rule in
# gcc/Makefile.in) and runs its self-tests through it (all.internal and
# selftest in gcc/Makefile.in), which a release compiles out, though cc1 and
# cc1plus still run to say so (toplev::run_self_tests in toplev.cc); and
# configure runs the assembler and linker --with-as and --with-ld name, to
# learn what they support: the assembler on a test source for each feature
# it checks (gcc_cv_as in gcc/configure.ac, gcc_GAS_CHECK_FEATURE in
# gcc/acinclude.m4), and the linker for its --version and --help (gcc_cv_ld,
# ld_ver and gcc_cv_ld_buildid in gcc/configure.ac).  Both CI legs run
# x86_64 programs: x64 Windows natively and ARM64 Windows through its x64
# emulator.
#
# GCC refuses --with-headers and --with-libs for a compiler whose host is
# its target (configure.ac's handling of --with-headers and --with-libs), so
# the target's headers and runtime reach it otherwise.  In a Canadian cross
# the build reads the target's headers from the build sysroot followed by
# the native header directory, or else from the tool directory's
# sys-include, which nothing fills (BUILD_SYSTEM_HEADER_DIR and
# target_header_dir, with CROSS_SYSTEM_HEADER_DIR, in gcc/configure.ac), and
# GCC's limits.h includes the system's only if the build finds limits.h
# there (LIMITS_H_TEST and the stmp-int-hdrs rule in gcc/Makefile.in).
# --with-build-sysroot and --with-native-system-header-dir are nixpkgs'
# means to that end (the --with-native-system-header-dir and
# --with-build-sysroot flags in configure-flags.nix), which joins / to the
# store path of its C library's headers; on Windows that join begins /C:,
# which no program opens, so here the build sysroot is
# mingw-w64-headers-latest's output and the header directory /include.  The
# installed compiler reads neither: MinGW's configuration fixes its native
# header directory at /mingw/include (NATIVE_SYSTEM_HEADER_DIR in
# gcc/config/mingw/mingw32.h).  It searches the tool directory's include and
# lib, x86_64-w64-mingw32/include and lib under its prefix (TOOL_INCLUDE_DIR
# in gcc/cppdefault.cc, tooldir_prefix in process_command in gcc/gcc.cc),
# where MinGW-w64's build guide lays out a toolchain's headers and runtime
# (mingw-w64-doc/howto-build/mingw-w64-howto-build.txt, "Building the crt",
# step 5), so the install phase copies mingw-w64-headers-latest's headers
# and mingw-w64-crt-x86_64's runtime there, as --with-headers and
# --with-libs copy them into gcc-x86_64, and the installed driver finds them
# with no flag.  Ahead of them a compiler whose host is its target also
# searches the local prefix's include directory, /usr/local/include by
# default (the CROSS_DIRECTORY_STRUCTURE test above cpp_include_defaults and
# its LOCAL_INCLUDE_DIR entries in gcc/cppdefault.cc, local_prefix in
# gcc/configure.ac), which on Windows is that directory of the build's
# drive.  nixpkgs takes it out of every GCC's search on a build machine
# other than Solaris or kFreeBSD (noSysDirs in top-level/stage.nix,
# all-packages.nix and patches/default.nix) with gcc-12-no-sys-dirs.patch,
# and no-local-include-dir.patch is that patch's gcc/cppdefault.cc half: its
# gcc.cc half moves defaults that MinGW's configuration replaces
# (STANDARD_STARTFILE_PREFIX_1 and STANDARD_STARTFILE_PREFIX_2 in gcc/gcc.cc
# and gcc/config/mingw/mingw32.h).  libgcc installs under lib/gcc, and
# libstdc++ its headers in include/c++/16.2.0 and its library in lib, where
# the compiler looks for them (gcc_gxx_include_dir in gcc/configure.ac,
# GLIBCXX_EXPORT_INSTALL_INFO in libstdc++-v3/acinclude.m4).  The build
# sysroot also gives gcc-x86_64 --sysroot for the target libraries
# (AC_ARG_WITH([build-sysroot]) in configure.ac), which moves none of its
# header directories, since it has no sysroot of its own (the
# CROSS_DIRECTORY_STRUCTURE test above cpp_include_defaults in
# gcc/cppdefault.cc), and which binutils-x86_64's ld takes with none
# configured (the OPTION_SYSROOT case of parse_args in ld/lexsup.c and
# get_sysroot in ld/ldmain.c in binutils 2.46).
#
# The thread model is win32 and libgcc and libstdc++ are static, as in
# gcc-x86_64, decided from what the package set's packages rely on and what
# nixpkgs does.  The packages need neither the posix thread model nor a DLL
# of GCC's: hello, sed, zlib, libgreet, greeter and zdemo are C, which the
# driver links with the static libgcc even where a shared one exists
# (SHARED_LIBGCC_SPEC in gcc/config/mingw/mingw32.h).  nixpkgs passes a
# thread model only to a GCC whose host is not its target (--enable-threads
# in configure-flags.nix's crossConfigureFlags, applied only when
# !hostIsTarget), so its GCC whose host is its target takes the target's
# default, win32 (thread_file in the mingw case of gcc/config.gcc), though
# the thread library it gives a MinGW-w64 target is mcfgthreads (threads in
# all-packages.nix); and it makes libgcc and libstdc++ shared on a target
# with shared libraries (enableShared in default.nix).
# --enable-threads=win32 names that model, which needs no library the ladder
# does not build, and under which libstdc++ has std::thread since
# mingw-w64-headers-latest names Windows 10 (__GTHREADS_CXX0X in
# libgcc/config/i386/gthr-win32.h).  It is the new driver's model: libgcc
# and libstdc++ take theirs from gcc-x86_64, which compiles them and whose
# driver says win32 too (GCC_AC_THREAD_MODEL in config/gthr.m4 and
# libgcc/configure.ac, GLIBCXX_ENABLE_THREADS in libstdc++-v3/acinclude.m4).
# posix would need winpthreads built first, gcc-x86_64 configured with it,
# and its DLL beside every program that uses a thread.  --disable-shared
# makes this compiler's libgcc and libstdc++ static, as gcc-x86_64's are, so
# a program it links imports no DLL of GCC's: the seed stdenv has no fixup
# phase to copy one beside it (pkgs/windows/seed-stdenv/setup.sh), and
# nixpkgs' shared ones would have to be on PATH wherever the program runs.
# Exceptions are SEH, the x86_64 target's (DWARF2_UNWIND_INFO in
# gcc/config/i386/cygming.h, tmake_eh_file in the x86_64-*-mingw* case of
# libgcc/config.host).
#
# The rest is gcc-x86_64's, which says why of each part: GMP, MPFR and MPC
# built in the tree from gcc-latest's archives, a build directory beside
# the source, the remaining flags, CFLAGS and LDFLAGS, the configure
# arguments deleted from the top-level Makefile, install-strip, libgcc's
# debugging information stripped, and the same files removed after the
# install, libstdc++'s gdb script from lib, where it installs here.
#
# install-strip strips every program it installs with the strip configure
# records (STRIP and EXTRA_HOST_FLAGS in Makefile.in), here the host's,
# x86_64-w64-mingw32-strip.  Without -p strip writes the time into a PE
# header, where ld wrote zero, and the builder's SOURCE_DATE_EPOCH stands
# for the time (copy_object in binutils/objcopy.c,
# _bfd_XXi_only_swap_filehdr_out in bfd/peXXigen.c and bfd_get_current_time
# in bfd/bfdio.c in binutils 2.46), so STRIP names it with -p.  It reaches
# configure from the environment, which configure takes over its own search
# (NCN_STRICT_CHECK_TOOLS in config/acx.m4), since the makes install-strip
# runs take no variable from make's command line (the install-strip rule and
# MAKEOVERRIDES in Makefile.in).
#
# nixpkgs passes --enable-__cxa_atexit and --enable-long-long only to a
# GCC whose host is not its target (configure-flags.nix's
# crossConfigureFlags, applied only when !hostIsTarget), and each is what
# configure does here without it (use_cxa_atexit in gcc/configure.ac with
# default_use_cxa_atexit in the mingw case of gcc/config.gcc,
# GLIBCXX_ENABLE_LONG_LONG in libstdc++-v3/configure.ac).
# --disable-bootstrap and --disable-plugin are nixpkgs' here as well: it
# builds no GCC off Darwin in stages unless it profiles it (disableBootstrap
# in default.nix, disableBootstrap' and --disable-bootstrap in
# configure-flags.nix), and builds plugins only where the build machine is
# the host (enablePlugin in default.nix).  Its --with-gxx-include-dir names
# the directory this compiler takes for libstdc++'s headers by default.
{
  seedStdenv,
  binutils,
  binutils-native-x86_64,
  binutils-x86_64,
  gcc-latest,
  gcc-x86_64,
  mingw-w64-crt-x86_64,
  mingw-w64-headers-latest,
}:
let
  pname = "gcc";
  inherit (builtins.parseDrvName gcc-latest.name) version;
  target = "x86_64-w64-mingw32";
  # The target of gcc-latest and binutils, the build machine's compilers
  # and tools.
  build = "i686-w64-mingw32";

  # The directory an archive unpacks to: its name less the .tar suffix.
  unpacked = archive: builtins.head (builtins.match "(.+)[.]tar[.][a-z]+" archive.name);
in
seedStdenv.mkDerivation {
  name = "${pname}-${version}";

  inherit (gcc-latest)
    src
    gmpSrc
    mpfrSrc
    mpcSrc
    ;

  buildInputs = [
    gcc-latest
    binutils
    gcc-x86_64
    binutils-x86_64
  ];

  patches = [ ./no-local-include-dir.patch ];

  preConfigure = ''
    tar xf "$gmpSrc"
    mv ${unpacked gcc-latest.gmpSrc} gmp
    tar xf "$mpfrSrc"
    mv ${unpacked gcc-latest.mpfrSrc} mpfr
    tar xf "$mpcSrc"
    mv ${unpacked gcc-latest.mpcSrc} mpc
    unset C_INCLUDE_PATH
    mkdir ../build
    cd ../build
    configureScript=../gcc-${version}/configure
  '';

  configureFlags = [
    "--host=${target}"
    "--target=${target}"
    "--with-build-sysroot=${mingw-w64-headers-latest}"
    "--with-native-system-header-dir=/include"
    "--with-as=${binutils-native-x86_64}/bin/as.exe"
    "--with-ld=${binutils-native-x86_64}/bin/ld.exe"
    "--with-gnu-as"
    "--with-gnu-ld"
    "--enable-languages=c,c++"
    "--enable-__cxa_atexit"
    "--enable-long-long"
    "--enable-threads=win32"
    "--disable-bootstrap"
    "--disable-dependency-tracking"
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
    "--disable-win32-registry"
    "--without-isl"
    "CC=${target}-gcc"
    "CXX=${target}-g++"
    "AR=${target}-ar"
    "LD=${target}-ld"
    "WINDRES=${target}-windres"
    "CC_FOR_BUILD=${build}-gcc"
    "CXX_FOR_BUILD=${build}-g++"
    "AR_FOR_BUILD=${build}-ar"
    "RANLIB_FOR_BUILD=${build}-ranlib"
    "CFLAGS=-O2"
    "CXXFLAGS=-O2"
    "CFLAGS_FOR_TARGET=-O2"
    "CXXFLAGS_FOR_TARGET=-O2"
    "LDFLAGS=-Wl,--no-insert-timestamp"
  ];

  # The strip install-strip runs, which keeps the zero time stamp ld wrote
  # (see above).
  STRIP = "${target}-strip -p";

  # make runs $NIX_BUILD_CORES jobs and installs with install-strip, as
  # gcc-x86_64's does, and strip, with -p as STRIP has it, then removes
  # the debugging information from what libgcc installs.  The store paths
  # of the headers and the runtime keep the builder's spelling in a phase
  # (pkgs/windows/seed-stdenv/setup.sh), so each is quoted: sh reads a
  # backslash outside quotes as an escape.
  enableParallelBuilding = true;
  buildPhase = ''
    sed -e '/TOPLEVEL_CONFIGURE_ARGUMENTS=/d' -i Makefile
    make ''${enableParallelBuilding:+-j$NIX_BUILD_CORES}
  '';
  installPhase = ''
    make ''${enableParallelInstalling:+-j$NIX_BUILD_CORES} install-strip
    libgccDir=$out/lib/gcc/${target}/${version}
    ${target}-strip -p --strip-debug "$libgccDir"/*.a "$libgccDir"/*.o
    rm -r "$out/libexec/gcc/${target}/${version}/install-tools" "$libgccDir/install-tools"
    rm -rf "$out/share/gcc-${version}/python" "$out/share/man" "$out/share/info"
    rm "$out/lib/libstdc++.a-gdb.py"
    rmdir "$out/share/gcc-${version}" "$out/share"
    mkdir -p "$out/${target}/include" "$out/${target}/lib"
    cp -r "${mingw-w64-headers-latest}/include/." "$out/${target}/include"
    cp -r "${mingw-w64-crt-x86_64}/lib/." "$out/${target}/lib"
  '';
}
// {
  # gcc-latest and gcc10 list the archives it builds.
  srcs = [ ];
}
