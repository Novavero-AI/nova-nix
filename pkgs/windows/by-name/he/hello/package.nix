# GNU hello, built through the stage-1 stdenv.  The whole recipe is now just a
# name and a source -- setup.sh handles unpack/configure/build/install.
#
# pkgs/windows/hello.nix points `nova-nix build` at this package: build takes
# a derivation-valued expression, and this is a function (it takes
# { stdenv, fetchurl }), so something has to call it.
{ stdenv, fetchurl }:
stdenv.mkDerivation {
  name = "hello";
  src = fetchurl {
    url = "https://mirrors.kernel.org/gnu/hello/hello-2.12.3.tar.gz";
    sha256 = "0d5f60154382fee10b114a1c34e785d8b1f492073ae2d3a6f7b147687b366aa0";
  };
}
