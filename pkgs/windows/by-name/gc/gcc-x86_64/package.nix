# GCC 16.2.0 with C and C++ for x86_64-w64-mingw32, as nixpkgs builds the
# GCC of its x86_64 MinGW-w64 cross toolchain (nixpkgs
# 5eb598da22ce76bc18ec85ecc1de66ca838b3514, pkgs/development/compilers/
# gcc/default.nix with common/configure-flags.nix, for pkgsCross.mingwW64),
# for Windows: the C and C++ drivers, cc1, cc1plus, cpp, collect2, gcov,
# libgcc and libstdc++, running on i686 Windows and writing assembly for
# x86_64-w64-mingw32, configured, compiled and installed in the seed
# stdenv by gcc-latest and binutils, and the target libraries by the
# compiler the same make links, against MinGW-w64 v14.0.0's headers and
# the runtime gcc-core-x86_64 compiled.  It finishes the cross toolchain
# MinGW-w64's build guide builds (mingw-w64-doc/howto-build/
# mingw-w64-howto-build.txt:353-362).
#
# The build is gcc-latest's, which says why of each part, for the other
# target: one cross compiler build given its target's headers and runtime
# with --with-headers and --with-libs, which GCC's top level copies into
# this output and points the compiler it links at when it builds libgcc
# and libstdc++, so the installed driver finds all of them with no flag;
# GMP, MPFR and MPC built in its tree from gcc-latest's archives, unpacked
# as gcc-core-x86_64 unpacks them; a build directory beside the source;
# gcc-latest's drivers and binutils' archiver, linker and resource
# compiler as the build's tools, by the name of their target,
# i686-w64-mingw32, with the stdenv's C_INCLUDE_PATH unset; LDFLAGS giving
# every link --no-insert-timestamp; the configure arguments deleted from
# the top-level Makefile; and install-strip with a STRIP that keeps the
# zero time stamp.  --with-as and --with-ld name binutils-x86_64's
# assembler and linker, and the build finds the target's other tools by
# the target's name on PATH, which binutils-x86_64 puts there.
#
# The flags are those of nixpkgs' cross compiler, the one its
# pkgsCross.mingwW64 builds, where gcc-latest's are minimal-bootstrap's,
# which builds the bootstrap compilers alone.  --enable-__cxa_atexit and
# --enable-long-long are the defaults for this target (gcc/config.gcc:2358
# with gcc/configure.ac:2132-2154, libstdc++-v3/configure.ac:188), and
# --disable-libstdcxx-pch and --disable-bootstrap mean here what they
# mean there.  nixpkgs gives libstdc++ the mcf thread model, from
# mcfgthreads, a library the ladder does not build; --enable-threads=win32
# names the model GCC takes for this target on the Windows API alone
# (gcc/config.gcc:2360-2366), nixpkgs' own for a MinGW target with no
# thread library, as gcc-latest's is, under which libstdc++ has
# std::thread since mingw-w64-headers-latest names Windows 10.  Exceptions
# are SEH, the x86_64 target's (gcc/config/i386/cygming.h:358-369,
# libgcc/config.host:927-936).  nixpkgs points the compiler at its C
# library with --with-native-system-header-dir, --with-build-sysroot=/
# and --with-gxx-include-dir, none of which a compiler given its headers
# with --with-headers takes.
#
# Where nixpkgs' choices need what the ladder does not have, gcc-latest's
# are kept: GMP, MPFR and MPC in the tree, where nixpkgs links its own and
# ISL; the zlib of GCC's tree for its --with-system-zlib; --disable-nls
# for its gettext; --disable-shared, where nixpkgs builds libgcc and
# libstdc++ as DLLs, so a program carries what it uses of them and
# imports no DLL of GCC's; --disable-lto and --disable-plugin, where
# nixpkgs builds both on a host with shared libraries; and the target
# libraries other than libgcc and libstdc++ turned off, as gcc-latest
# turns them off.  --disable-win32-registry is not nixpkgs', for
# gcc46-cxx's reason.  gcc-latest's other flags are minimal-bootstrap's
# and not nixpkgs' cross compiler's, so the build ID, gcov, the analyzer
# and libstdc++'s visibility and ABI are configure's defaults here.
#
# CFLAGS, CXXFLAGS and the target libraries' are -O2, the optimization
# GCC's configure gives each, without the -g whose debugging information
# nixpkgs' strip hook removes after its install
# (pkgs/development/compilers/gcc/common/strip-attributes.nix).  libgcc's
# own Makefile adds -g all the same (libgcc/Makefile.in:246-249), so the
# install phase strips it with the target's strip, as gcc-core-x86_64's
# does.  The install removes the install-tools directories, the manual and
# info pages, libstdc++'s Python pretty-printers and gdb script and the
# share directories that leaves empty, as gcc-latest's does.
#
# nixpkgs' patch for a Windows target, libstdc-fix-compilation-in-
# freestanding-win32.patch, declares malloc and free where libstdc++ is
# built without its C library (!_GLIBCXX_HOSTED); this libstdc++ is
# hosted, and there the patch adds using-declarations of names
# atexit_thread.cc already reaches, as gcc-latest's build of the same file
# shows.  Of its patches for every GCC 16, gcc-12-no-sys-dirs.patch takes
# /usr/local/include, /lib and /usr/lib out of the searches of a native
# compiler or one with a sysroot, and this is neither (gcc/cppdefault.cc:
# 30-32, gcc/gcc.cc:8618-8659); mangle-NIX_STORE-in-__FILE__.patch hides
# store hashes in __FILE__ from nixpkgs' scan for references; and the rest
# change RISC-V, PowerPC, AArch64 and LoongArch code and libgomp, which
# this build turns off.
{
  seedStdenv,
  binutils,
  binutils-x86_64,
  gcc-latest,
  mingw-w64-crt-x86_64,
  mingw-w64-headers-latest,
}:
let
  pname = "gcc";
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
    "--with-libs=${mingw-w64-crt-x86_64}/lib"
    "--with-as=${binutils-x86_64}/bin/${target}-as.exe"
    "--with-ld=${binutils-x86_64}/bin/${target}-ld.exe"
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
    "CC=${host}-gcc"
    "CXX=${host}-g++"
    "AR=${host}-ar"
    "LD=${host}-ld"
    "WINDRES=${host}-windres"
    "CFLAGS=-O2"
    "CXXFLAGS=-O2"
    "CFLAGS_FOR_TARGET=-O2"
    "CXXFLAGS_FOR_TARGET=-O2"
    "LDFLAGS=-Wl,--no-insert-timestamp"
  ];

  # make runs $NIX_BUILD_CORES jobs and installs with install-strip, as
  # nixpkgs' make does, stripping with -p (see gcc10), and strip then
  # removes the debugging information from what libgcc installs.
  enableParallelBuilding = true;
  buildPhase = ''
    sed -e '/TOPLEVEL_CONFIGURE_ARGUMENTS=/d' -i Makefile
    make ''${enableParallelBuilding:+-j$NIX_BUILD_CORES}
  '';
  installPhase = ''
    make ''${enableParallelInstalling:+-j$NIX_BUILD_CORES} 'STRIP=${host}-strip -p' install-strip
    libgccDir=$out/lib/gcc/${target}/${version}
    ${target}-strip -p --strip-debug "$libgccDir"/*.a "$libgccDir"/*.o
    rm -r "$out/libexec/gcc/${target}/${version}/install-tools" "$libgccDir/install-tools"
    rm -rf "$out/share/gcc-${version}/python" "$out/share/man" "$out/share/info"
    rm "$out/${target}/lib/libstdc++.a-gdb.py"
    rmdir "$out/share/gcc-${version}" "$out/share"
  '';
}
// {
  # gcc-latest and gcc10 list the archives it builds.
  srcs = [ ];
}
