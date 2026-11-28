# The Windows package set.
#
# Layout follows nixpkgs:
#
#   by-name/<shard>/<pname>/package.nix   one package per directory, shard =
#                                         the first two characters of <pname>
#   seed-stdenv/                          the seed ladder's stdenv, on tools
#                                         built from the seed: mkDerivation,
#                                         setup.sh
#   stdenv/                               the package set's stdenv: the
#                                         seed stdenv's tools with the
#                                         ladder's native x86_64 toolchain,
#                                         through compiler and linker
#                                         wrappers
#   lib.nix                               callPackage and friends
#   unused-prebuilt.json                  the prebuilt files in the source
#                                         archives that no build uses, each
#                                         with why: the builds that unpack
#                                         an archive remove its files, and
#                                         CI's trust check fails on a
#                                         prebuilt file the list lacks
#
# The set is a fixpoint: callPackage reads each package.nix's formal parameters
# and supplies them from the set being defined, so a package names its
# dependencies -- { stdenv, zlib }: ... -- rather than reaching across the tree
# with a relative import.  Nothing enumerates packages; adding one is adding a
# directory.
let
  lib = import ./lib.nix;

  callPackage = lib.callPackageWith self;

  unusedPrebuiltList = builtins.fromJSON (builtins.readFile ./unused-prebuilt.json);

  # by-name is sharded to keep any one directory small, so the two levels are
  # walked directly here: the shard is a storage detail and must not show up
  # as an attribute.
  byName =
    let
      root = ./by-name;
      # readDir answers name -> type, and every entry at both levels is a
      # directory by construction.  Reading the type it already returned is
      # what keeps a stray file from being taken for a shard or a package:
      # unfiltered, an editor swapfile or a .DS_Store is opened as a directory
      # and takes the whole set down with "inappropriate type", rather than
      # being passed over.
      subdirectories =
        dir:
        let
          entries = builtins.readDir dir;
        in
        builtins.filter (name: entries.${name} == "directory") (builtins.attrNames entries);
      inShard =
        shard:
        map (pname: {
          name = pname;
          value = callPackage (root + "/${shard}/${pname}/package.nix") { };
        }) (subdirectories (root + "/${shard}"));
    in
    builtins.listToAttrs (builtins.concatMap inShard (subdirectories root));

  self = byName // {
    inherit lib;

    stdenv = callPackage ./stdenv { };
    seedStdenv = callPackage ./seed-stdenv { };
    fetchurl = import ./fetchurl.nix;

    # The files unused-prebuilt.json lists in a source, as its archive names
    # them, for each build that unpacks the archive to remove: none for a
    # source that is not a fetched archive or that the list does not name.
    unusedPrebuilt =
      src:
      if src ? outputHash then
        builtins.concatMap (
          group: if group.archive == src.name then group.paths else [ ]
        ) unusedPrebuiltList
      else
        [ ];
  };
in
self
