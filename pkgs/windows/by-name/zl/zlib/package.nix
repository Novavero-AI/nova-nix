# zlib, built from source through the stage-1 stdenv as a proper Windows DLL.
# zlib's ./configure emits Unix .so naming under MSYS2, so we use zlib's bundled
# win32/Makefile.gcc (with SHARED_MODE=1) instead, which yields zlib1.dll plus
# the libz.dll.a import lib and libz.a.  A consumer linking -lz then resolves the
# import lib and links zlib1.dll *dynamically*, and fixupPhase bundles the DLL --
# the dynamic-link + real-library DLL-bundling pattern.  The
# dontConfigure/buildPhase/installPhase attrs are the stdenv's build-style
# overrides.
{ stdenv, fetchurl }:
stdenv.mkDerivation {
  name = "zlib";
  src = fetchurl {
    url = "https://github.com/madler/zlib/releases/download/v1.3.2/zlib-1.3.2.tar.gz";
    sha256 = "bb329a0a2cd0274d05519d61c667c062e06990d72e125ee2dfa8de64f0119d16";
  };
  dontConfigure = true;
  buildPhase = "make -f win32/Makefile.gcc";
  installPhase = ''
    mkdir -p "$prefix/bin" "$prefix/include" "$prefix/lib"
    make install -f win32/Makefile.gcc SHARED_MODE=1 BINARY_PATH="$prefix/bin" INCLUDE_PATH="$prefix/include" LIBRARY_PATH="$prefix/lib"
  '';
}
