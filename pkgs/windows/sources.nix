# Every source the package set fetches, as one list: each package's `srcs`,
# the fixed-output derivations it is built from.
#
# A package names its sources inside its build script, where no walk over
# attributes finds them, as nixpkgs' maintainers/scripts/find-tarballs.nix
# walks: so each package states them in `srcs`, and the test suite checks
# this list against the fetches in the closure of every derivation in the
# set.  CI realises the list to publish each source to the public cache.
let
  pkgs = import ./default.nix;

  srcsOf =
    name:
    let
      package = pkgs.${name};
    in
    if builtins.isAttrs package then package.srcs or [ ] else [ ];
in
builtins.concatMap srcsOf (builtins.attrNames pkgs)
