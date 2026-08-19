# A program that links libgreet -- the buildInputs proof.  nova-nix must build
# libgreet first, then build greeter with libgreet's include/ on CPPFLAGS and
# lib/ on LDFLAGS so the compiler finds greet.h and the linker resolves -lgreet.
# This is one package depending on another package nova-nix built from source.
{ stdenv, libgreet }:
stdenv.mkDerivation {
  name = "greeter";
  src = ./src;
  buildInputs = [ libgreet ];
}
