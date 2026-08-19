# A program that links zlib -- the buildInputs proof on a real third-party
# library.  nova-nix builds zlib first, then builds zdemo with zlib's include/ on
# CPPFLAGS and lib/ on LDFLAGS, so the compiler finds zlib.h and the linker
# resolves -lz against libz.a (static).  zdemo compresses and round-trips a
# string and prints the linked zlib's version.
{ stdenv, zlib }:
stdenv.mkDerivation {
  name = "zdemo";
  src = ./src;
  buildInputs = [ zlib ];
}
