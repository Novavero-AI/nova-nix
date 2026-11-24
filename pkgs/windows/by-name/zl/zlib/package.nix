# zlib, built from source as a Windows DLL.  zlib's ./configure names its
# shared library as Unix does, so this uses zlib's own win32/Makefile.gcc,
# whose install with SHARED_MODE=1 gives zlib1.dll, its import library
# libz.dll.a and the static libz.a.  A consumer linking -lz takes the
# import library first and so links zlib1.dll dynamically, and the
# stdenv's fixup phase bundles the DLL beside it.
#
# The makefile names ar, windres and strip by those names, which neither
# a stdenv that keeps its toolchain off PATH nor busybox's applets
# provide, so each is given as the stdenv names it, AR, WINDRES and STRIP,
# where it does, and by its name otherwise.
{ stdenv, fetchurl }:
stdenv.mkDerivation {
  name = "zlib";
  src = fetchurl {
    urls = [
      "https://github.com/madler/zlib/releases/download/v1.3.2/zlib-1.3.2.tar.gz"
      "https://zlib.net/fossils/zlib-1.3.2.tar.gz"
    ];
    sha256 = "bb329a0a2cd0274d05519d61c667c062e06990d72e125ee2dfa8de64f0119d16";
  };
  dontConfigure = true;
  buildPhase = ''
    make -f win32/Makefile.gcc AR="''${AR:-ar}" RC="''${WINDRES:-windres}" STRIP="''${STRIP:-strip}"
  '';
  installPhase = ''
    mkdir -p "$prefix/bin" "$prefix/include" "$prefix/lib"
    make install -f win32/Makefile.gcc SHARED_MODE=1 AR="''${AR:-ar}" RC="''${WINDRES:-windres}" STRIP="''${STRIP:-strip}" BINARY_PATH="$prefix/bin" INCLUDE_PATH="$prefix/include" LIBRARY_PATH="$prefix/lib"
  '';
}
