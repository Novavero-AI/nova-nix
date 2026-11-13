# MinGW-w64's headers, as nixpkgs' mingw_w64_headers installs them (nixpkgs
# 5eb598da22ce76bc18ec85ecc1de66ca838b3514, pkgs/os-specific/windows/
# mingw-w64/headers.nix): the C library's and the Windows API's declarations
# for i686-w64-mingw32, which GCC's build reads before any of MinGW-w64's
# runtime exists.  Its configure checks for no compiler and make compiles
# nothing: the seed stdenv's sh, make and applets copy the files.
#
# The release is v8.0.3, where nixpkgs packages v14.0.0 for a current GCC:
# the newest whose runtime calls none of three builtins GCC 4.6.4 lacks,
# __atomic_load_n (mingw-w64-crt/misc/gettimeofday.c, from v9.0.0), and
# __builtin_mul_overflow (misc/_recalloc.c) and __builtin_bswap16
# (misc/winbs_ushort.c), both from v13.0.0.  The archive is the project's
# release file on SourceForge, which nixpkgs fetches for its release and
# the project signs.  It unpacks to the tree GitHub's archive of the v8.0.3
# tag holds, which that host generates on request.
#
# --with-default-msvcrt=msvcrt is nixpkgs' flag, with the runtime the
# ladder links.  It writes __MSVCRT_VERSION__ 0x700 into _mingw.h, the
# default at this release (mingw-w64-headers/configure.ac:144-175) and not
# at v14.0.0, whose default is the UCRT.
#
# The headers install in include under the output, configure's default:
# the directory GCC's --with-headers is given, and the one MinGW-w64's own
# runtime reads below the directory its --with-sysroot names
# (mingw-w64-crt/Makefile.am:15).
#
# The guide's --host=i686-w64-mingw32 is not passed.  The host names the
# widl configure looks for under --with-widl and nothing else: with it and
# without, the same 1,514 files are installed.
{
  seedStdenv,
  fetchurl,
}:
let
  pname = "mingw-w64-headers";
  version = "8.0.3";
in
seedStdenv.mkDerivation {
  name = "${pname}-${version}";

  src = fetchurl {
    url = "mirror://sourceforge/mingw-w64/mingw-w64-v${version}.tar.bz2";
    hash = "sha256-1wLvcSyPIgNfgd32CJVCrfa8tupmnU6/TjEfhyQogvA=";
  };

  preConfigure = "cd mingw-w64-headers";

  configureFlags = [ "--with-default-msvcrt=msvcrt" ];
}
