# NYACC, the parser modules MesCC loads, as nixpkgs'
# minimal-bootstrap/mes/nyacc.nix unpacks it.  guilePath is the directory
# that goes on GUILE_LOAD_PATH.
{ fetchurl, kaem }:
let
  pname = "nyacc";
  # The version nixpkgs pairs with Mes 0.27.1, whose interpreter cannot parse
  # the block comments outside top level that NYACC 1.09.2 and later have.
  # Mes 0.28 asks for 0.99 or later (configure).
  version = "1.09.1";

  src = fetchurl {
    url = "mirror://savannah/nyacc/nyacc-${version}.tar.gz";
    hash = "sha256-DsmuU34NlReBpQ3jx5KayXqFwdS16F5dUVQuN1ECJxc=";
  };

  nyacc = kaem.runCommand "${pname}-${version}" { inherit pname version; } ''
    ungz --file ${src} --output nyacc.tar
    mkdir -p ''${out}/share
    cd ''${out}/share
    untar --file ''${NIX_BUILD_TOP}/nyacc.tar
  '';
in
nyacc
// {
  inherit src;
  guilePath = "${nyacc}/share/${pname}-${version}/module";
}
