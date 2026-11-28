# Parity fixture: a user environment in the shape nix-env evaluates from
# its buildenv.nix, over the fixture packages profile-cases.sh makes: the
# packages' paths in `derivations` as strings with no context, and a
# manifest whose references bring the packages into the closure.  gamma is
# in the manifest because delta propagates it, so a sandboxed build sees it.
{ fixtures }:
let
  fixture = name: builtins.path { path = /. + "${fixtures}/${name}"; };
  packages = map fixture [
    "alpha-1.0"
    "delta-1.0"
  ];
  manifest = builtins.toFile "env-manifest.nix" (
    toString (packages ++ [ (fixture "gamma-1.0") ])
  );
in
derivation {
  name = "user-environment";
  system = "builtin";
  builder = "builtin:buildenv";
  inherit manifest;
  derivations = map (path: [
    "true"
    5
    1
    (builtins.unsafeDiscardStringContext path)
  ]) packages;
  preferLocalBuild = true;
  allowSubstitutes = false;
}
