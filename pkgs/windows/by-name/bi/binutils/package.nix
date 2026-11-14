# GNU binutils, as nixpkgs' minimal-bootstrap/binutils builds it with tinycc
# (nixpkgs 60cbef83e025a167470f66fae093dc9f261cb8fc), for Windows: the
# assembler, linker, archiver and object tools, running on i686 Windows and
# writing i686 PE and COFF, configured, compiled and installed in the seed
# stdenv by tinycc-native.
#
# The target is i686-w64-mingw32, the triple MinGW-w64's own build guide
# configures binutils for (mingw-w64-doc/howto-build) and the one under
# which GCC 4.6.4 takes MinGW-w64's feature set (the *-w64-* cases of its
# gcc/config.gcc; any other vendor is mingw.org's).  The stdenv's triple,
# i686-pc-mingw32, stays the build and the host, so configure takes the
# tools for a cross toolchain's: they install as
# bin/i686-w64-mingw32-<name>.exe, and ar, as, dlltool, ld, nm, objcopy,
# objdump, ranlib, readelf and strip again under those names in
# i686-w64-mingw32/bin.
#
# nixpkgs' flags that describe ELF or Linux are not passed.  --with-sysroot=/
# names the root of the current drive, which no derivation names;
# --enable-new-dtags chooses between two ELF dynamic tags; and the top-level
# configure adds gprofng for a Linux target alone.  CFLAGS does not define
# __LITTLE_ENDIAN__, which nixpkgs gives include/plugin-api.h, since that
# header takes the byte order from _WIN32.  nixpkgs' edits of /bin/sh in
# missing, install-sh and mkinstalldirs have nothing to do: busybox's sh
# and GNU make each run a script with the interpreter its first line names,
# found by base name.  Its makeinfo that is true is the MAKEINFO make reads
# here, there being no symbolic link to name true with.  make runs it for
# bfd's manual, whose stamp the build writes, in the build and again in
# install, and the manual the archive ships is the one installed; without
# it the missing script fails the build.  make runs $NIX_BUILD_CORES jobs,
# as in nixpkgs, in one run.  minimal-bootstrap/binutils builds ld on its
# own, for a race between jobs over ld/.deps/ldwrite.Po; that file is
# dependency tracking's (ld/Makefile.in:1581), and with
# --disable-dependency-tracking the compile rule writes none.  nixpkgs' two
# settings for libtool have nothing to do either: the command length
# libtool would measure is 8192 on a MinGW build host, unmeasured and many
# times the longest archive command here, and the edit of ltmain.sh sorts
# the members libtool unpacks from an archive, where this configuration
# unpacks none.
#
# nixpkgs' deterministic.patch is not applied.  It sets
# BFD_DETERMINISTIC_OUTPUT on the linker's output, which the archive and
# XCOFF writers read and the PE writer does not: ld stamps a PE image with
# the time unless it is run with --no-insert-timestamp.
#
# fix-tinycc-attribute.patch is nixpkgs' own, from its later recipe
# (nixpkgs 3cd60b6a1b2be2e4ce51b1ae3e58e0946be42368), unchanged.
# include/ansidecl.h defines __attribute__ away for a compiler that is not
# GCC 2.7 or later, which tcc is not.  TinyCC's _mingw.h spells __stdcall
# as an attribute, so a source that reads ansidecl.h before windows.h
# declares every Windows API function cdecl, and pops the arguments the
# function has already popped: bfd's bfdio.c, on each file it opens,
# libiberty's pex-win32.c and lrealpath.c, ld's ldbuildid.c and windres's
# winduni.c.  The structures gas's CodeView and ld's PDB writers declare
# packed lose their layout the same way.  With the patch tcc reads every
# attribute, keeps those it knows and passes over the rest.
#
# temp-file-binary.patch opens in binary mode the temporary file that ar
# and ranlib, and objcopy and strip rewriting a file in place, write their
# output to.  Without mkstemp, which msvcrt.dll lacks, make_tempname opens
# it with no O_BINARY (binutils/bucomm.c:553), and msvcrt treats the
# descriptor as text: bfd's writes through it put a CR before each LF, and
# the copy into place (binutils/rename.c: smart_rename) reads it back
# through it, which takes those CRs out again but ends at the first 0x1A
# byte.  An archive of libgcc's objects is cut short, and ranlib then finds
# no archive in it.  MinGW-w64's runtime supplies a mkstemp that opens in
# binary (mingw-w64-crt/misc/mkstemp.c), so its builds never reach the
# open the patch changes.
#
# What follows is for TinyCC and its headers:
#
#   --disable-nls: the bundled libintl declares alloca as returning char *
#   (intl/loadmsgcat.c), which tcc, declaring it itself, rejects.
#
#   --disable-plugins: bfd's plugin.c reads the plugin directory with
#   opendir, which MinGW-w64's runtime supplies and msvcrt.dll does not.
#
#   --disable-libctf: libctf includes sys/param.h, which TinyCC's headers
#   lack.  Without it ld drops a .ctf section with a warning, and objdump
#   and readelf have no --ctf; GCC 4.6.4 writes no CTF.
#
#   --enable-install-libbfd: libtool unpacks a library it does not install
#   into the one that links it, member by member, and tcc -ar keeps the
#   first 15 characters of a member's name, which libsframe's three objects
#   share.  bfd's default for a cross toolchain leaves libsframe out of
#   the install; with this flag libsframe.a stays a library of its own,
#   which each link names after libbfd.a, as in nixpkgs' native build.
#   tcc -ar also writes an archive anew each time it runs, so it cannot
#   add to one in steps, as libtool does past its command length.  The
#   flag goes once the archiver is this package's own ar.
#
#   compat/include: alloca as a macro and rpcdce.h, each file naming the
#   MinGW-w64 lines it follows.
#
# The build is static, bfd's default.  --with-lib-path=: leaves ld no
# built-in search path, as in nixpkgs, and for a second reason here: ld's
# genscripts.sh splits the path at each colon, a drive letter's included.
# A one-time build reads no dependency file, and with tracking on automake
# runs each compile through depcomp, a sh of its own.
#
# The programs import from msvcrt.dll and kernel32.dll alone, and need the
# msvcrt.dll of Windows Vista or later, as busybox.exe does: here for
# mbrtowc, strnlen and utime (mingw-w64-crt/lib-common/msvcrt.def.in:
# 1285-1833).
{
  seedStdenv,
  fetchurl,
}:
let
  pname = "binutils";
  version = "2.41";
in
seedStdenv.mkDerivation {
  name = "${pname}-${version}";

  src = fetchurl {
    url = "mirror://gnu/binutils/binutils-${version}.tar.xz";
    hash = "sha256-rppXieI0WeWWBuZxRyPy0//DHAMXQZHvDQFb3wYAdFA=";
  };

  patches = [
    ./fix-tinycc-attribute.patch
    ./temp-file-binary.patch
  ];

  buildInputs = [ ./compat ];

  configureFlags = [
    "--target=i686-w64-mingw32"
    "--disable-nls"
    "--disable-plugins"
    "--disable-libctf"
    "--enable-install-libbfd"
    "--enable-deterministic-archives"
    "--disable-dependency-tracking"
    "--with-lib-path=:"
  ];

  makeFlags = [ "MAKEINFO=true" ];

  enableParallelBuilding = true;
}
