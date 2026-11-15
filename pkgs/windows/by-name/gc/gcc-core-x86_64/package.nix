# GCC 16.2.0's C compiler and libgcc for x86_64-w64-mingw32, the GCC
# nixpkgs builds without its target's C library to compile that library
# (nixpkgs 5eb598da22ce76bc18ec85ecc1de66ca838b3514, gccWithoutTargetLibc
# in pkgs/top-level/all-packages.nix, with pkgs/development/compilers/
# gcc/common/configure-flags.nix), for Windows: the driver, cc1, cpp,
# collect2, GCC's own headers and libgcc, running on i686 Windows and
# writing assembly for x86_64-w64-mingw32, configured, compiled and
# installed in the seed stdenv by gcc-latest and binutils, and libgcc by
# the compiler the same make links.  It is MinGW-w64's build guide's GCC
# core (mingw-w64-doc/howto-build/mingw-w64-howto-build.txt:282-306), which
# compiles the runtime, mingw-w64-crt-x86_64, the next rung; GCC with its
# libraries comes after the runtime.
#
# The sources are gcc-latest's, GCC 16.2.0 with GMP, MPFR and MPC built in
# its tree, unpacked as gcc-latest unpacks them, into the directories named
# for each archive less its suffix.  The build is gcc-latest's, which says
# why of each part, for the other target: the stdenv's
# i686-pc-mingw32 is the build and the host and x86_64-w64-mingw32 the
# target, so GCC takes itself for a cross compiler and its programs install
# as bin/x86_64-w64-mingw32-<name>.exe.  --with-as and --with-ld name
# binutils-x86_64's assembler and linker, and the build finds the target's
# other tools, the archiver libgcc's make runs among them, by the target's
# name on PATH, which binutils-x86_64 puts there.  The build's compilers
# are gcc-latest's drivers and its archiver, linker and resource compiler
# binutils', each by the name of their target, i686-w64-mingw32.
# --with-headers names mingw-w64-headers-latest's include directory, whose
# headers are the same files for either target (their configure reads the
# host only to look for widl, mingw-w64-headers/configure.ac:16-30).
#
# make builds all-gcc and all-target-libgcc and installs them, as gcc-core
# does for 4.6.4: the guide's GCC core, which compiles the runtime, and
# libgcc, which, static, links nothing (libgcc/configure.ac:219,
# GCC_NO_EXECUTABLES) and needs MinGW-w64's headers alone.  With libgcc the
# driver links a program against the runtime, which is how
# test/data/mingw-w64-x86_64.nix checks it; nixpkgs' make builds its
# target libraries whole.
#
# Of nixpkgs' flags for this compiler, --disable-threads,
# --disable-decimal-float, --disable-nls, --disable-shared, --with-gnu-as,
# --with-gnu-ld, --disable-win32-registry and --disable-bootstrap mean here
# what they mean there: libgcc is single-threaded and static, which serves
# a compiler of the runtime alone.  Its list gives --without-headers and
# then --with-headers, the later of which configure takes, and here
# --with-headers is given alone.  Its --disable-lib* and --enable-libssp
# flags name target libraries other than libgcc, which the make targets
# here never configure, as do --disable-hosted-libstdcxx,
# --disable-libstdcxx-backtrace and --enable-fully-dynamic-string,
# libstdc++'s.  --with-gcc and
# --enable-hash-synchronization are read by no configure of 16.2.0's tree,
# and --disable-debug by libffi's alone.  nixpkgs links its own GMP, MPFR,
# MPC and ISL and the system's zlib; here GMP, MPFR and MPC are built in
# the tree, as in gcc-latest, the zlib of GCC's tree is linked, and there
# is no ISL, so --without-isl leaves out the loop optimizations it serves.
# --disable-multilib is nixpkgs' too; --disable-lto and --disable-plugin
# are gcc-latest's, where nixpkgs builds both on a host with shared
# libraries.
#
# CFLAGS, CXXFLAGS and CFLAGS_FOR_TARGET are -O2, the optimization GCC's
# configure gives the compiler and libgcc, without the -g whose debugging
# information nixpkgs' strip hook removes after its install
# (pkgs/development/compilers/gcc/common/strip-attributes.nix).  libgcc's
# own Makefile adds -g all the same (libgcc/Makefile.in:246-249), so the
# install phase strips that, with the target's strip, as gcc-latest does
# with its own.  The build phase deletes TOPLEVEL_CONFIGURE_ARGUMENTS from
# the top-level Makefile, as gcc-latest's does, and install-strip-gcc
# strips each program with a STRIP that keeps the zero time stamp ld
# writes.  The install removes the install-tools directories, which hold
# fixincludes and what it needs to run again, the manual and info pages,
# and the share directory that leaves empty, with rmdir, which fails the
# build if anything else is in it, as gcc-latest's does.
{
  seedStdenv,
  binutils,
  binutils-x86_64,
  gcc-latest,
  mingw-w64-headers-latest,
}:
let
  pname = "gcc-core";
  inherit (builtins.parseDrvName gcc-latest.name) version;
  target = "x86_64-w64-mingw32";
  # The target of the compiler and tools the build runs.
  host = "i686-w64-mingw32";

  # The directory an archive unpacks to: its name less the .tar suffix.
  unpacked = archive: builtins.head (builtins.match "(.+)[.]tar[.][a-z]+" archive.name);
in
seedStdenv.mkDerivation {
  name = "${target}-${pname}-${version}";

  inherit (gcc-latest)
    src
    gmpSrc
    mpfrSrc
    mpcSrc
    ;

  buildInputs = [
    gcc-latest
    binutils
    binutils-x86_64
  ];

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
    "--target=${target}"
    "--with-headers=${mingw-w64-headers-latest}/include"
    "--with-as=${binutils-x86_64}/bin/${target}-as.exe"
    "--with-ld=${binutils-x86_64}/bin/${target}-ld.exe"
    "--with-gnu-as"
    "--with-gnu-ld"
    "--enable-languages=c"
    "--disable-bootstrap"
    "--disable-dependency-tracking"
    "--disable-decimal-float"
    "--disable-lto"
    "--disable-multilib"
    "--disable-nls"
    "--disable-plugin"
    "--disable-shared"
    "--disable-threads"
    "--disable-win32-registry"
    "--without-isl"
    "CC=${host}-gcc"
    "CXX=${host}-g++"
    "AR=${host}-ar"
    "LD=${host}-ld"
    "WINDRES=${host}-windres"
    "CFLAGS=-O2"
    "CXXFLAGS=-O2"
    "CFLAGS_FOR_TARGET=-O2"
    "LDFLAGS=-Wl,--no-insert-timestamp"
  ];

  # make runs $NIX_BUILD_CORES jobs, as nixpkgs' make does.
  enableParallelBuilding = true;
  buildPhase = ''
    sed -e '/TOPLEVEL_CONFIGURE_ARGUMENTS=/d' -i Makefile
    make ''${enableParallelBuilding:+-j$NIX_BUILD_CORES} all-gcc all-target-libgcc
  '';
  installPhase = ''
    make ''${enableParallelInstalling:+-j$NIX_BUILD_CORES} 'STRIP=${host}-strip -p' install-strip-gcc install-strip-target-libgcc
    libgccDir=$out/lib/gcc/${target}/${version}
    ${target}-strip -p --strip-debug "$libgccDir"/*.a "$libgccDir"/*.o
    rm -r "$out/libexec/gcc/${target}/${version}/install-tools" "$libgccDir/install-tools"
    rm -r "$out/share/man" "$out/share/info"
    rmdir "$out/share"
  '';
}
// {
  # gcc-latest and gcc10 list the archives it builds.
  srcs = [ ];
}
