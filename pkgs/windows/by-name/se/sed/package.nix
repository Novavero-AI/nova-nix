# GNU sed through the stage-1 stdenv -- the generalization proof.  A second,
# different package (and a .tar.xz, exercising the seed's xz decompression),
# built with the same two-line mkDerivation as hello.  If this works with no new
# machinery, nova-nix has a real stdenv, not a hello special-case.
{ stdenv, fetchurl }:
stdenv.mkDerivation {
  name = "sed";
  src = fetchurl {
    url = "https://mirrors.kernel.org/gnu/sed/sed-4.10.tar.xz";
    sha256 = "b8e72182b2ec96a3574e2998c47b7aaa64cc20ce000d8e9ac313cc07cecf28c7";
  };
}
