# MinGW-w64's runtime for x86_64-w64-mingw32 at mingw-w64-headers-latest's
# release, v14.0.0, as nixpkgs' mingw_w64 builds it for an x86_64 host
# (nixpkgs 5eb598da22ce76bc18ec85ecc1de66ca838b3514, pkgs/os-specific/
# windows/mingw-w64/default.nix, as pkgsCross.mingwW64 builds it with
# gccWithoutTargetLibc), compiled by gcc-core-x86_64 with
# binutils-x86_64's assembler, archiver and dlltool in the seed stdenv:
# crt2.o and the other startup objects, libmingw32.a, libmingwex.a,
# libmoldname.a, libm.a, and an import library for each DLL, libmsvcrt.a
# and libkernel32.a among them.  It is MinGW-w64's build guide's step after
# the GCC core (mingw-w64-doc/howto-build/mingw-w64-howto-build.txt:
# 309-350), and the x86_64 GCC after it is given it as its target's runtime.
#
# It is mingw-w64-crt-latest's build for the other target, and that package
# says why of the directory configured, the variables unset and the flags
# but those that choose the target.  --host is x86_64-w64-mingw32, under
# whose name configure looks up the compiler, archiver, dlltool and
# assembler, which gcc-core-x86_64 and binutils-x86_64 put on PATH.
# --disable-lib32 and --enable-lib64 are nixpkgs' for an x86_64 host, and
# the ARM runtimes are off for one (configure.ac:133-161); the 64-bit
# libraries install in lib, the directory configure names for an x86_64
# host's (configure.ac:173-178, Makefile.am:1852).  The compiles read
# mingw-w64-headers-latest's include directory, which --with-sysroot names,
# the same headers for either target.
#
# gcc-core-x86_64 has no runtime of its own, so configure's link of a
# program to learn the suffix of executables fails, which AC_NO_EXECUTABLES
# lets it do (configure.ac:34), and its test of whether ld defines
# __ImageBase links by hand with -nostdlib, which needs no runtime, for
# that reason (configure.ac:354-405).  That test decides the objects'
# symbols without failing the build, so test/data/mingw-w64-x86_64.nix
# pins crt2.o's, which name __ImageBase where ld defines it.
{
  seedStdenv,
  binutils-x86_64,
  gcc-core-x86_64,
  mingw-w64-headers-latest,
}:
let
  pname = "mingw-w64-crt";
  inherit (builtins.parseDrvName mingw-w64-headers-latest.name) version;
  target = "x86_64-w64-mingw32";
in
seedStdenv.mkDerivation {
  name = "${pname}-${target}-${version}";

  inherit (mingw-w64-headers-latest) src;

  buildInputs = [
    gcc-core-x86_64
    binutils-x86_64
  ];

  preConfigure = ''
    unset CC AR C_INCLUDE_PATH
    mkdir ../build
    cd ../build
    configureScript=../mingw-w64-v${version}/mingw-w64-crt/configure
  '';

  configureFlags = [
    "--host=${target}"
    "--with-sysroot=${mingw-w64-headers-latest}"
    "--with-default-msvcrt=msvcrt"
    "--disable-lib32"
    "--enable-lib64"
    "--disable-dependency-tracking"
    "CFLAGS=-O2"
  ];

  enableParallelBuilding = true;
}
// {
  srcs = [ ];
}
