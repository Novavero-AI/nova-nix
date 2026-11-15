# MinGW-w64's runtime for i686-w64-mingw32 at mingw-w64-headers-latest's
# release, v14.0.0, as nixpkgs' mingw_w64 builds it (nixpkgs
# 5eb598da22ce76bc18ec85ecc1de66ca838b3514, pkgs/os-specific/windows/
# mingw-w64/default.nix), compiled by gcc10 with binutils' assembler,
# archiver and dlltool in the seed stdenv, for the compilers after gcc10.
# It is mingw-w64-crt's build at a newer release, and that package says
# why of the directory configured, the variables unset and each flag;
# what the release changes is said here.
#
# gcc10 compiles it, since the runtime calls three builtins GCC 4.6.4
# lacks: __atomic_load_n (misc/gettimeofday.c:48-49),
# __builtin_mul_overflow (misc/_recalloc.c:15) and __builtin_bswap16
# (misc/winbs_ushort.c:5).  gcc10 keeps v8.0.3's runtime in its own
# prefix, which the runtime does not take in: its compiles read
# mingw-w64-headers-latest's include directory, which --with-sysroot
# names, with -I, before any directory of the compiler's
# (Makefile.am:14-22), and it links no program.  configure links two:
# one against gcc10's runtime, to learn the suffix of executables, which
# no file the runtime builds takes (AC_NO_EXECUTABLES links it,
# configure.ac:34), and one with -nostdlib, which asks whether ld
# defines __ImageBase (configure.ac:354-405), as binutils' does.  If that
# link failed, every object would be compiled to name __image_base__ in
# its place, and nothing would fail; test/data/mingw-w64-latest.nix pins
# crt2.o's symbols for that reason.  configure looks a C++ compiler up
# under the host's name too (configure.ac:38) and finds gcc10's g++; the
# runtime compiles no C++.
#
# --with-default-msvcrt=msvcrt has the build make libmsvcrt-os.a, the
# import library for msvcrt.dll with the functions it lacks compiled in,
# and install libmsvcrt.a as a copy of it (configure.ac:256-267,
# Makefile.am:1468-1477).  The runtime is compiled for __MSVCRT_VERSION__
# 0x600 (Makefile.am:23), the version mingw-w64-headers-latest writes
# for msvcrt.dll.  nixpkgs' --enable-secure-api is read by no configure
# at this release, and its --enable-idl by the headers' alone
# (mingw-w64-headers/configure.ac:115-125), which this does not
# configure.  nixpkgs regenerates the build files with autoreconfHook,
# where this uses the release's own, as mingw-w64-crt does, and turns
# off hardening flags its compiler wrapper adds, where the seed stdenv
# has no wrapper.
#
# One rule installs each file (lib32/Makefile.am:228 leaves libntoskrnl.a
# to the rule of Makefile.am:1604-1607), so make installs with
# $NIX_BUILD_CORES jobs too, as nixpkgs' enableParallelBuilding has it.
{
  seedStdenv,
  binutils,
  gcc10,
  mingw-w64-headers-latest,
}:
let
  pname = "mingw-w64-crt";
  inherit (builtins.parseDrvName mingw-w64-headers-latest.name) version;
  target = "i686-w64-mingw32";
in
seedStdenv.mkDerivation {
  name = "${pname}-${version}";

  inherit (mingw-w64-headers-latest) src;

  buildInputs = [
    gcc10
    binutils
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
    "--enable-lib32"
    "--disable-lib64"
    "--disable-dependency-tracking"
    "CFLAGS=-O2"
  ];

  enableParallelBuilding = true;
}
// {
  srcs = [ ];
}
