# A derivation whose environment carries another's drvPath.  Upstream adds
# that .drv's whole closure to inputSrcs and inputDrvs (derivationStrict,
# primops.cc at 2.24.9): here a chain of derivations, a toFile whose text
# refers to a copied source, and a multi-output derivation that one output
# also refers to.
let
  note = builtins.toFile "note" "refers to ${./tofile.nix}";
  a = derivation {
    name = "a";
    system = "x86_64-linux";
    builder = "/bin/sh";
    outputs = [ "out" "dev" ];
    s = note;
  };
  m = derivation {
    name = "m";
    system = "x86_64-linux";
    builder = "/bin/sh";
    dep = a;
  };
in
derivation {
  name = "deep-context";
  system = "x86_64-linux";
  builder = "/bin/sh";
  closure = "${m.drvPath}";
  dev = a.dev;
}
