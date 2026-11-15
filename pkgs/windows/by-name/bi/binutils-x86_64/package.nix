# GNU binutils for x86_64-w64-mingw32, as nixpkgs builds the binutils of a
# cross toolchain (nixpkgs 5eb598da22ce76bc18ec85ecc1de66ca838b3514,
# pkgs/development/tools/misc/binutils/default.nix, whose targetPrefix
# names each program for the target, as pkgsCross.mingwW64 builds it): the
# assembler, linker, archiver and object tools, running on i686 Windows
# and writing x86_64 PE and COFF, configured, compiled and installed in the
# seed stdenv by gcc-latest and binutils.
#
# The ladder reaches x86_64 through a cross toolchain, as MinGW-w64's build
# guide builds one (mingw-w64-doc/howto-build/mingw-w64-howto-build.txt:
# 208-362) and nixpkgs builds pkgsCross.mingwW64's: these tools, a GCC
# core that compiles the runtime, the runtime, and GCC with its libraries.
# Everything before it is i686, and an i686 compiler cannot write x86_64
# code without a 64-bit assembler and linker for it to run.
#
# This is a rung of its own, and binutils' i686 rung stays as it is.  GCC
# for x86_64-w64-mingw32 and MinGW-w64's runtime look their tools up under
# the target's name (x86_64-w64-mingw32-as, -ar, -dlltool, -ranlib), which
# a binutils for i686-w64-mingw32 does not install even with
# --enable-targets, and that binutils is tcc's, with patches and flags for
# tcc and msvcrt.dll; nixpkgs builds one binutils for each target.  The
# x86_64 target's own formats include i386 PE and its linker the i386pe
# emulation (bfd/config.bfd:715-720, ld/configure.tgt:1009-1012), so these
# tools read and write both.
#
# The release is nixpkgs', 2.46, from the archive it fetches, which also
# holds gold; configure builds gold only when asked (configure.ac:343-347),
# and gold writes ELF alone.  GCC 16.2.0 needs nothing newer than 2.41
# for this target: its configure tests the assembler for each feature it
# would use and does without one 2.41 lacks, such as the .base64 directive
# it writes long strings with (gcc/configure.ac:3218-3221).
#
# The stdenv's i686-pc-mingw32 is the build and the host, so configure
# names the programs for the target, bin/x86_64-w64-mingw32-<name>.exe,
# and installs ar, as, dlltool, ld, ld.bfd, nm, objcopy, objdump, ranlib,
# readelf, strip and windres again under their own names in
# x86_64-w64-mingw32/bin, as nixpkgs' --program-prefix has it.  The
# build's compiler is gcc-latest's driver, and its archiver and linker
# binutils', each by the name of their target, i686-w64-mingw32, the only
# name either puts on PATH; the stdenv's CC, AR and LD are tcc's.
# preConfigure unsets C_INCLUDE_PATH, which holds TinyCC's headers for tcc
# and which GCC searches first, as gcc-latest's does, and configures from
# a build directory beside the source, as nixpkgs' preConfigure does.
#
# --enable-64-bit-bfd, --enable-deterministic-archives, --disable-werror,
# --with-lib-path=: and MAKEINFO=true are nixpkgs', and mean here what they
# mean there; the x86_64 target asks for the 64-bit BFD itself
# (bfd/config.bfd:718), and binutils' rung says why of the last two.  Of
# nixpkgs' other flags, --enable-new-dtags chooses between two ELF tags,
# --enable-fix-loongson2f-nop is an option of the MIPS assembler,
# --disable-gprofng names what configure builds for a Linux target alone
# (configure.ac:412-424), and --with-system-zlib a zlib the ladder does
# not have, so the tools link the zlib of binutils' tree.  nixpkgs makes
# the libraries shared for a host that has them; here they are static,
# bfd's default, so the programs import no DLL of binutils'.  Plugin
# support is on, bfd's default where windows.h is found
# (config/plugins.m4:1-17), as in nixpkgs, and ld installs its libdep
# plugin, a DLL ar, nm and ld load from lib/bfd-plugins.
# --disable-nls is new: nixpkgs gives its build gettext, which the ladder
# does not have, and the tools print their messages untranslated.
# --disable-dependency-tracking is what nixpkgs' configure phase adds to
# every configure that has it, and make runs $NIX_BUILD_CORES jobs, as
# nixpkgs' enableParallelBuilding has it.
#
# CFLAGS is -O2, configure's default for GCC, without the -g that nixpkgs'
# strip hook removes after its install, as mingw-w64-crt-latest's is.
# LDFLAGS gives every link --no-insert-timestamp, for gcc46-cxx's reason:
# ld writes the time of the link into a PE image unless it is given that
# (ld/emultempl/pep.em:188 and 1549-1552, bfd/peXXigen.c:850-858).
#
# None of nixpkgs' patches is applied.  deterministic.patch sets a flag the
# PE writer does not read, for the reason binutils' rung gives.
# always-search-rpath.patch adds the library path to the scripts of a
# cross linker, which --with-lib-path=: leaves empty.  plugins-no-BINDIR.patch
# serves nixpkgs' separate lib output.  Its three edits of libtool.m4 change
# configure only where it is regenerated, which nixpkgs does on Darwin
# alone, and avr-size.patch gives size an AVR format.  windres-locate-gcc.patch
# has windres, run by a path, look for the gcc of its own prefix on PATH;
# run by its prefixed name, as GCC's build runs it, windres looks there
# first already (binutils/resrc.c:520-527).
{
  seedStdenv,
  fetchurl,
  binutils,
  gcc-latest,
}:
let
  pname = "binutils";
  version = "2.46";
  target = "x86_64-w64-mingw32";
  # The target of the compiler and tools the build runs.
  host = "i686-w64-mingw32";
in
seedStdenv.mkDerivation {
  name = "${target}-${pname}-${version}";

  src = fetchurl {
    url = "mirror://gnu/binutils/binutils-with-gold-${version}.tar.bz2";
    hash = "sha256-uMmj15bcCw6OqTXcJMXI1vNF6xd62lrGpbESLHfWIVg=";
  };

  buildInputs = [
    gcc-latest
    binutils
  ];

  preConfigure = ''
    unset C_INCLUDE_PATH
    mkdir ../build
    cd ../build
    configureScript=../binutils-with-gold-${version}/configure
  '';

  configureFlags = [
    "--target=${target}"
    "--enable-64-bit-bfd"
    "--enable-deterministic-archives"
    "--disable-werror"
    "--disable-nls"
    "--disable-dependency-tracking"
    "--with-lib-path=:"
    "CC=${host}-gcc"
    "AR=${host}-ar"
    "LD=${host}-ld"
    "CFLAGS=-O2"
    "LDFLAGS=-Wl,--no-insert-timestamp"
  ];

  makeFlags = [ "MAKEINFO=true" ];

  enableParallelBuilding = true;
}
