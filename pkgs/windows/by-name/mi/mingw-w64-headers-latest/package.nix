# MinGW-w64's headers at its newest release, as nixpkgs' mingw_w64_headers
# installs them (nixpkgs 5eb598da22ce76bc18ec85ecc1de66ca838b3514,
# pkgs/os-specific/windows/mingw-w64/headers.nix): the C library's and the
# Windows API's declarations for i686-w64-mingw32 that
# mingw-w64-crt-latest is compiled against and the compilers after gcc10
# are given.  Its configure checks for no compiler and make compiles
# nothing, as in mingw-w64-headers, which says why of the archive, the
# directory configured and the flags left out.
#
# The release is v14.0.0, the newest the project has made (its release
# files on SourceForge and the v14.0.0 tag of its repository), and the one
# nixpkgs packages.  mingw-w64-headers' v8.0.3 is the newest whose runtime
# GCC 4.6.4 compiles; this release's runtime calls the three builtins that
# compiler lacks, which gcc10 has, so gcc10 compiles it
# (mingw-w64-crt-latest).
#
# --with-default-msvcrt=msvcrt is nixpkgs' flag for this target, whose C
# library nixpkgs names msvcrt (lib/systems/examples.nix,
# mingw-msvcrt-i686), and the runtime the ladder links.  At this release
# the flag writes __MSVCRT_VERSION__ 0x600 into _mingw.h, and the default
# is the UCRT (mingw-w64-headers/configure.ac:138-183).  _WIN32_WINNT is
# configure's default, 0xa00, Windows 10 (configure.ac:127-135), as in
# nixpkgs, which passes no --with-default-win32-winnt.
{
  seedStdenv,
  fetchurl,
}:
let
  pname = "mingw-w64-headers";
  version = "14.0.0";
in
seedStdenv.mkDerivation {
  name = "${pname}-${version}";

  src = fetchurl {
    url = "mirror://sourceforge/mingw-w64/mingw-w64-v${version}.tar.bz2";
    hash = "sha256-bq+SHZ65h9OCCzZOqXdbwZuWXsgUkLb91xZSbCjhmVw=";
  };

  preConfigure = "cd mingw-w64-headers";

  configureFlags = [ "--with-default-msvcrt=msvcrt" ];
}
