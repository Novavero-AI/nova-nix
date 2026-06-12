# GNU hello, built through the stage-1 stdenv.  The whole recipe is now just a
# name and a source -- setup.sh handles unpack/configure/build/install.  This is
# the same build the hand-written hello-build.nix proved, factored into the
# reusable mkDerivation.
let
  stdenv = import ./stdenv.nix;
  fetchurl = import <nix/fetchurl.nix>;
in
stdenv.mkDerivation {
  name = "hello";
  src = fetchurl {
    url = "https://mirrors.kernel.org/gnu/hello/hello-2.12.3.tar.gz";
    sha256 = "0d5f60154382fee10b114a1c34e785d8b1f492073ae2d3a6f7b147687b366aa0";
  };
}
