# Parity fixture: a dependent derivation whose drvPath must byte-match
# what upstream nix-instantiate computes (see workflows/parity.yml).
# Anchors the input-derivation-modulo substitution against upstream: the
# root's hash substitutes dep's modulo hash, and dep has two outputs so
# the output-name sorting and the multi-output reference both land in
# the ATerm under test.
let
  dep = derivation {
    name = "parity-dep";
    system = "x86_64-linux";
    builder = "/bin/sh";
    args = [ "-c" "echo dep > $out; echo dev > $dev" ];
    outputs = [ "out" "dev" ];
  };
in
derivation {
  name = "parity-root";
  system = "x86_64-linux";
  builder = "/bin/sh";
  args = [ "-c" "echo root > $out" ];
  inherit dep;
  devInput = dep.dev;
}
