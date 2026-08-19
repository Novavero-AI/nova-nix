# The Windows package set.
#
# Layout follows nixpkgs:
#
#   by-name/<shard>/<pname>/package.nix   one package per directory, shard =
#                                         the first two characters of <pname>
#   bootstrap/                            the sha256-pinned MSYS2 seeds
#   stdenv/                               mkDerivation, setup.sh, cc-wrapper.sh
#   lib.nix                               callPackage and friends
#
# The set is a fixpoint: callPackage reads each package.nix's formal parameters
# and supplies them from the set being defined, so a package names its
# dependencies -- { stdenv, zlib }: ... -- rather than reaching across the tree
# with a relative import.  Nothing enumerates packages; adding one is adding a
# directory.
let
  lib = import ./lib.nix;

  callPackage = lib.callPackageWith self;

  # by-name is sharded to keep any one directory small, so the two levels are
  # walked here rather than by packagesFromDirectoryRecursive: the shard is a
  # storage detail and must not show up as an attribute.
  byName =
    let
      root = ./by-name;
      inShard =
        shard:
        map (pname: {
          name = pname;
          value = callPackage (root + "/${shard}/${pname}/package.nix") { };
        }) (builtins.attrNames (builtins.readDir (root + "/${shard}")));
    in
    builtins.listToAttrs (builtins.concatMap inShard (builtins.attrNames (builtins.readDir root)));

  self = byName // {
    inherit lib;

    stdenv = import ./stdenv;
    fetchurl = import <nix/fetchurl.nix>;
  };
in
self
