# A tiny static library built from source, used as a buildInputs dependency by
# greeter.nix.  The source is a local directory (not a tarball), which exercises
# setup.sh's directory-source path and its "no ./configure, just make" path.
let
  stdenv = import ./stdenv.nix;
in
stdenv.mkDerivation {
  name = "libgreet";
  src = ./libgreet;
}
