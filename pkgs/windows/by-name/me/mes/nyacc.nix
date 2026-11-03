# NYACC, the parser modules MesCC loads, as nixpkgs'
# minimal-bootstrap/mes/nyacc.nix unpacks it.  guilePath is the directory
# that goes on GUILE_LOAD_PATH.
{ fetchurl, kaem }:
let
  pname = "nyacc";
  # The version nixpkgs pairs with Mes 0.27.1: Mes's interpreter cannot parse
  # the block comments outside top level that NYACC 1.09.2 and later have.
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
nyacc // { guilePath = "${nyacc}/share/${pname}-${version}/module"; }
