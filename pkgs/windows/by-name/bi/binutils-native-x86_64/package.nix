# GNU binutils that run on x86_64 Windows and write x86_64-w64-mingw32, as
# nixpkgs builds the binutils of a platform whose host is its target
# (nixpkgs 5eb598da22ce76bc18ec85ecc1de66ca838b3514,
# pkgs/development/tools/misc/binutils/default.nix, whose targetPrefix is
# empty when the host is the target): the assembler, linker, archiver and
# object tools, configured, compiled and installed in the seed stdenv by
# gcc-x86_64 and binutils-x86_64.
#
# The package set's stdenv runs these tools on x86_64 Windows under their
# own names (pkgs/windows/stdenv/default.nix), and binutils-x86_64 runs
# on i686 and is named for its target.  This is a Canadian cross, as
# gcc-native-x86_64 is: the stdenv's i686-pc-mingw32 is the build, and
# x86_64-w64-mingw32 the host and the target.  With the host and the target
# the same, the top level gives the programs no prefix, so they install as
# bin/<name>.exe, and ar, as, dlltool, ld, ld.bfd, nm, objcopy, objdump,
# ranlib, readelf, strip and windres again in x86_64-w64-mingw32/bin, as
# binutils-x86_64's do under their prefixed names.
#
# The archive, the flags and what the build leaves out are
# binutils-x86_64's, which says why of each, with --host added.  The
# build's compiler is gcc-x86_64's driver and its archiver and linker
# binutils-x86_64's, each by the name of their target, x86_64-w64-mingw32,
# since the stdenv's CC, AR and LD are tcc's; configure finds the host's
# other tools, ranlib and strip among them, under that name on PATH
# (the host tools' NCN_STRICT_CHECK_TOOLS in configure.ac,
# _NCN_TOOL_PREFIXES and NCN_STRICT_CHECK_TOOLS in config/acx.m4).  The
# libraries are static, as in binutils-x86_64, and so is gcc-x86_64's
# libgcc, so the programs import no DLL of binutils' or GCC's.
#
# With the build not the host, the top level and the configure scripts of
# bfd, binutils and opcodes take a compiler named gcc for the build machine
# unless CC_FOR_BUILD names one (the *_FOR_BUILD defaults for a build that
# is not the host in configure.ac, BFD_CC_FOR_BUILD in bfd/acinclude.m4),
# and the seed stdenv has no program by that name.  CC_FOR_BUILD names
# gcc-latest's driver, by its target's name, i686-w64-mingw32, as nixpkgs
# gives every binutils the build machine's compiler (depsBuildBuild in
# default.nix).  configure runs it to learn the build machine's suffix for
# programs; for this target the build compiles none of its own, since bin2c
# and sysinfo serve SPU and HMS targets (BUILD_SRCONV and BUILD_MISC in
# binutils/configure.ac) and opcodes' table generators run in maintainer
# mode alone (the @MAINT@ prerequisite of the $(srcdir)/i386%tbl.h rule in
# opcodes/Makefile.am).
{
  seedStdenv,
  binutils-x86_64,
  gcc-latest,
  gcc-x86_64,
}:
let
  pname = "binutils";
  inherit (builtins.parseDrvName binutils-x86_64.name) version;
  target = "x86_64-w64-mingw32";
  # The target of gcc-latest, the build machine's compiler.
  build = "i686-w64-mingw32";
in
seedStdenv.mkDerivation {
  name = "${pname}-${version}";

  inherit (binutils-x86_64) src;

  buildInputs = [
    gcc-latest
    gcc-x86_64
    binutils-x86_64
  ];

  preConfigure = ''
    unset C_INCLUDE_PATH
    mkdir ../build
    cd ../build
    configureScript=../binutils-with-gold-${version}/configure
  '';

  configureFlags = [
    "--host=${target}"
    "--target=${target}"
    "--enable-64-bit-bfd"
    "--enable-deterministic-archives"
    "--disable-werror"
    "--disable-nls"
    "--disable-dependency-tracking"
    "--with-lib-path=:"
    "CC=${target}-gcc"
    "AR=${target}-ar"
    "LD=${target}-ld"
    "CC_FOR_BUILD=${build}-gcc"
    "CFLAGS=-O2"
    "LDFLAGS=-Wl,--no-insert-timestamp"
  ];

  makeFlags = [ "MAKEINFO=true" ];

  enableParallelBuilding = true;
}
// {
  # binutils-x86_64 lists the archive it builds.
  srcs = [ ];
}
