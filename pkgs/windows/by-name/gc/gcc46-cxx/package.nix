# GCC 4.6.4 with C and C++, as nixpkgs' minimal-bootstrap/gcc/4.6.cxx.nix
# builds gcc46-cxx with gcc46 (nixpkgs
# 5eb598da22ce76bc18ec85ecc1de66ca838b3514), for Windows: the C and C++
# drivers, cc1, cc1plus, cpp, collect2, libgcc and libstdc++, running on
# i686 Windows and writing assembly for i686-w64-mingw32, configured,
# compiled and installed in the seed stdenv by gcc46 and binutils, and the
# target libraries by the compiler the same make links.  GCC is written in
# C++ from 4.8 on, so the ladder's next GCC needs this one, as nixpkgs'
# gcc10 is built by its gcc46-cxx.
#
# The sources are gcc-core's, with GCC's g++ archive, which adds the C++
# front end (gcc/cp) and libstdc++-v3 to the same tree, unpacked over it as
# nixpkgs unpacks both.  As in gcc-core, the stdenv's i686-pc-mingw32 is
# the build and the host and the target is binutils', so GCC takes itself
# for a cross compiler and its programs install as
# bin/i686-w64-mingw32-<name>.exe, i686-w64-mingw32-g++.exe among them; it
# configures from a build directory beside its source; and --with-as and
# --with-ld compile in the paths of binutils' assembler and linker.
# gcc-core says why of each.
#
# GCC's installation guide has a cross compiler given its target's headers
# and libraries before it is configured, with --with-headers and
# --with-libs ("Building a cross compiler" in gcc/doc/install.texi;
# gcc-core says why not --with-sysroot, the guide's other choice).  The
# top-level configure copies each into this output, MinGW-w64's headers
# as i686-w64-mingw32/sys-include and mingw-w64-crt's libraries and start
# files as i686-w64-mingw32/lib (the --with-headers and --with-libs cases
# and copy_dirs in configure.ac), where gcc46 has them, and where the
# build points the compiler it links with -B, -L and -isystem when it
# compiles and links the target libraries (FLAGS_FOR_TARGET's
# *-w64-mingw* case and build_tooldir directories in configure.ac).
# libstdc++'s configure links its tests for a MinGW target (the
# *-mingw32* case of libstdc++-v3/crossconfig.m4), so the runtime has to
# be there before it runs; gcc-core builds libgcc alone, which links
# nothing.  The runtime's include directory, which holds the twelve
# uncompiled sources it installs, is not a library directory and is not
# copied.  libstdc++ installs its library beside the runtime's and its
# headers in i686-w64-mingw32/include/c++/4.6.4, a cross compiler's places
# for them (gxx_include_dir and glibcxx_toolexeclibdir in
# GLIBCXX_EXPORT_INSTALL_INFO in libstdc++-v3/acinclude.m4, with the
# --with-cross-host the top level gives a cross compiler's target
# libraries in configure.ac's target_configargs), so the driver links a
# C++ program with no flag naming a directory, as gcc46's links a C one.
# nixpkgs points its native compiler at musl's headers and libraries
# instead, with --with-native-system-header-dir, --with-build-sysroot and
# the C_INCLUDE_PATH, LIBRARY_PATH and -dynamic-linker its build script
# exports; its no-system-headers.patch and its edit of
# libstdc++-v3/configure.host are for that compiler and musl too.
#
# The build's compiler is gcc46's driver, and its archiver and linker are
# binutils', each named by the target's name, the only name either puts on
# PATH: configure, the build being its own host, looks the host's tools up
# under their bare names (AC_PROG_CC and the host tools'
# NCN_STRICT_CHECK_TOOLS in configure.ac), and the seed stdenv's CC, AR
# and LD are tcc's.  preConfigure unsets C_INCLUDE_PATH, which holds
# TinyCC's headers for tcc, as mingw-w64-crt's does: GCC searches it
# before its own headers and MinGW-w64's (register_include_chains in
# gcc/incpath.c).
#
# No program carries the time it was built.  binutils' ld writes the time
# of the link into a PE image's header unless it is given
# --no-insert-timestamp, which GCC's link spec for MinGW never passes, and
# the seed stdenv has no compiler wrapper to add it, as the package set's
# stdenv has (pkgs/windows/stdenv/link.specs).  LDFLAGS passes it to every
# link gcc46 makes here: the host's programs take LDFLAGS, and so do the
# build machine's, the build being the host.  It replaces the stdenv's
# LDFLAGS, whose -L flags name gcc46's and binutils' lib directories, from
# which the build links nothing.  strip writes the time again, over the
# zero ld wrote, unless it is run with -p, as nixpkgs' strip hook runs it.
# The install phase therefore gives make STRIP, binutils' strip by the
# target's name with -p, which install-strip, nixpkgs' install target,
# runs on each program it installs.  It is a make variable on the command
# line, since the stdenv splits configureFlags and makeFlags at spaces, and
# it has to be given: configure, looking for strip under its bare name,
# finds none and records true.
#
# Two builds at the output's own path are then the same bytes, but
# nova-nix build --check, which builds at a scratch path and rewrites its
# hash in what the builder wrote, finds the five drivers, cc1 and cc1plus
# differ.  Each names the output, and each carries a sum of its bytes that
# no rewrite of the hash reaches: the PE checksum ld and strip write
# (coff_apply_checksum in bfd/coffcode.h), and in cc1 and cc1plus the MD5
# of the objects they link (executable_checksum in gcc/genchecksum.c), two
# of which compile the prefix in (the prefix.o rule and
# PREPROCESSOR_DEFINES in gcc/Makefile.in).
#
# CFLAGS and CXXFLAGS are nixpkgs' -O1, and the target libraries' its -O0
# without the -dynamic-linker it adds for musl.  The target libraries'
# flags, given, replace the ones with -g configure gives them otherwise
# (CFLAGS_FOR_TARGET and CXXFLAGS_FOR_TARGET in configure.ac), whose
# debugging information would name the build directory and the store
# paths the compiler read, and LIBGCC2_DEBUG_CFLAGS=-g0 removes the -g
# gcc's Makefile adds to libgcc's compiles, as in gcc-core.
#
# --disable-win32-registry is not nixpkgs': a GCC for Windows looks its
# directories up under HKEY_LOCAL_MACHINE\SOFTWARE\Free Software
# Foundation\4.6.4 (gcc/prefix.c's opening comment, get_key_value and
# lookup_key) wherever its configure links RegOpenKeyExA
# (AC_SEARCH_LIBS(RegOpenKeyExA, advapi32) in gcc/configure.ac), and here
# it links MinGW-w64's libadvapi32.a.  nixpkgs' --disable-lib* flags mean
# here what they mean there, but for --disable-libsanitizer, left out,
# which names a library later than 4.6.4.  gcc-core's
# --disable-decimal-float and --disable-plugin, which nixpkgs' 4.6.nix,
# the GCC tinycc builds, passes, are left out as 4.6.cxx.nix leaves them
# out: decimal float is on for a MinGW target (GCC_AC_ENABLE_DECIMAL_FLOAT
# in config/dfp.m4), and configure finds neither the objdump nor the
# dlopen its plugin checks look for and builds no plugin support (the
# checks under AC_ARG_ENABLE(plugin) in gcc/configure.ac).  The thread
# model is win32 and exceptions are SJLJ, as in gcc-core.
# --disable-shared, as in nixpkgs, makes libgcc and libstdc++ static, so a
# program carries what it uses of them and imports no DLL of GCC's.
{
  seedStdenv,
  fetchurl,
  binutils,
  gcc-core,
  gcc46,
  mingw-w64-crt,
  mingw-w64-headers,
}:
let
  pname = "gcc-cxx";
  inherit (builtins.parseDrvName gcc-core.name) version;

  cxxSrc = fetchurl {
    url = "mirror://gnu/gcc/gcc-${version}/gcc-g++-${version}.tar.gz";
    hash = "sha256-aQpdT2ZBgGQNsoB540YUaBksSEw31vZx3eS1On+ZGLs=";
  };

  target = "i686-w64-mingw32";
in
seedStdenv.mkDerivation {
  name = "${pname}-${version}";

  inherit (gcc-core)
    src
    gmpSrc
    mpfrSrc
    mpcSrc
    ;
  inherit cxxSrc;

  buildInputs = [
    gcc46
    binutils
  ];

  preConfigure = ''
    tar xf "$cxxSrc" -C ..
    tar xf "$gmpSrc"
    mv gmp-${gcc-core.gmpVersion} gmp
    tar xf "$mpfrSrc"
    mv mpfr-${gcc-core.mpfrVersion} mpfr
    tar xf "$mpcSrc"
    mv mpc-${gcc-core.mpcVersion} mpc
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
    "--disable-bootstrap"
    "--disable-dependency-tracking"
    "--disable-libgomp"
    "--disable-libmudflap"
    "--disable-libquadmath"
    "--disable-libssp"
    "--disable-libstdcxx-pch"
    "--disable-lto"
    "--disable-multilib"
    "--disable-nls"
    "--disable-shared"
    "--disable-win32-registry"
    "CC=${target}-gcc"
    "AR=${target}-ar"
    "LD=${target}-ld"
    "CFLAGS=-O1"
    "CXXFLAGS=-O1"
    "CFLAGS_FOR_TARGET=-O0"
    "CXXFLAGS_FOR_TARGET=-O0"
    "LDFLAGS=-Wl,--no-insert-timestamp"
  ];

  makeFlags = [ "LIBGCC2_DEBUG_CFLAGS=-g0" ];

  # make runs $NIX_BUILD_CORES jobs and installs with install-strip, as
  # minimal-bootstrap/gcc/4.6.cxx.nix's make does, stripping with -p (see
  # above).
  enableParallelBuilding = true;
  installPhase = "make \${enableParallelInstalling:+-j$NIX_BUILD_CORES} $makeFlags 'STRIP=${target}-strip -p' install-strip";
}
// {
  # gcc-core lists the archives the two share.
  srcs = [ cxxSrc ];
}
