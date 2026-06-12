# Step 4 of the stdenv plan: the first package built natively on Windows
# through a Nix derivation.  The compiler comes from the stage-0 seed (a
# store path), the source is copied into the store, and the output lands in
# the store with scanned references and a real NAR hash.
#
# cmd.exe is the one ambient tool here - the OS-guaranteed bootstrap shell,
# used only until stage 1 rebuilds bash from source.  It expands %out% and
# %src% from the build environment, the same role /bin/sh plays on Linux.
let
  seed = import ./seed.nix;
in
derivation {
  name = "hello";
  system = "x86_64-windows";
  builder = "cmd.exe";
  # --no-insert-timestamp: ld writes link time into the PE header (two
  # timestamps; 4 bytes total), the one source of nondeterminism between
  # otherwise bit-identical cold builds.  Zeroing it makes the output
  # byte-for-byte reproducible.
  args = [
    "/c"
    "mkdir %out% && ${seed}/mingw64/bin/gcc.exe %src% -Wl,--no-insert-timestamp -o %out%/hello.exe"
  ];
  src = ./hello.c;
  # Windows resolves a subprocess's DLLs via PATH (no RPATH): gcc.exe spawns
  # cc1.exe from lib/gcc/, whose gmp/mpfr/isl/zstd DLLs live in the seed's
  # bin.  Dependencies' bin dirs on the build PATH is the Windows analogue of
  # Linux stdenv's PATH setup - stage 1 will automate this.
  PATH = "${seed}/mingw64/bin";
}
