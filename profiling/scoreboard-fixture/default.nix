# A package set shaped like nixpkgs' top level, for checking the scoreboard
# walk (../scoreboard.nix) without evaluating nixpkgs: one attribute for
# each way a unit settles.  expected.txt beside this is the walk's result,
# which both evaluators must produce.
{
  system,
  config ? { },
  overlays ? [ ],
}:
let
  package =
    name:
    derivation {
      inherit name system;
      builder = "/bin/sh";
    };
  hello = package "hello";
in
{
  inherit hello;
  # Reached a second time, and counted again.
  alias = hello;

  # Removed packages in nixpkgs are throws: not packages.
  removed = throw "removed has been removed";
  # A derivation whose name throws is not a package either.
  nameless = package "nameless" // {
    name = throw "no name";
  };

  # nixpkgs refuses an unfree or broken package at its drvPath.
  unfree = package "unfree" // {
    drvPath = throw "unfree";
  };
  asserted = package "asserted" // {
    drvPath = assert false; "";
  };

  # Failures tryEval cannot catch, each ending its process.
  typeError = package "typeError" // {
    drvPath = 1 + "a";
  };
  aborted = abort "aborted";
  loop = let x = x; in x;
  # nix-env's queryName forces a derivation's name to a string.
  badName = package "badName" // {
    name = 1;
  };
  badRecurse = {
    recurseForDerivations = "yes";
  };

  # Not packages, and not descended into.
  callPackage = f: f;
  lib = {
    inner = package "lib-inner";
  };
  # Names outside [A-Za-z_][A-Za-z0-9_+-]* are not visited at all.
  "9lives" = package "9lives";
  "with.dot" = package "with-dot";

  python3Packages = {
    recurseForDerivations = true;
    requests = package "requests";
    missing = { }.attribute;
    nested = {
      recurseForDerivations = true;
      leaf = package "leaf";
    };
  };
}
