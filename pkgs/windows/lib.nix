# The small amount of lib this package set needs.
#
# nixpkgs gets these from `lib`; nova-nix has no nixpkgs to draw on while
# bootstrapping itself, so the two functions that make a package set a package
# set are defined here, in terms of builtins only.
rec {
  # callPackageWith autoArgs fn args
  #
  # Import `fn` and call it with the arguments it names, taken from `autoArgs`,
  # with `args` overriding.  This is what lets a package.nix declare its
  # dependencies as formal parameters -- { stdenv, zlib }: ... -- instead of
  # reaching across the tree with a relative import.  Modelled on nixpkgs'
  # lib.customisation.callPackageWith, minus the override machinery and the
  # spelling suggestions.
  callPackageWith =
    autoArgs: fn: args:
    let
      f = if builtins.isFunction fn then fn else import fn;
      fargs = builtins.functionArgs f;

      # Everything the function will receive: the automatic arguments it named,
      # plus the explicit ones.
      allArgs = builtins.intersectAttrs fargs autoArgs // args;

      # Formals we could not supply.  The attribute's value is true when the
      # formal has a default, so an unsupplied one is only an error if false.
      unpassed = builtins.removeAttrs fargs (builtins.attrNames allArgs);
      required = builtins.filter (name: !unpassed.${name}) (builtins.attrNames unpassed);
    in
    if required == [ ] then
      f allArgs
    else
      throw "callPackage: ${toString fn} called without required argument(s): ${builtins.concatStringsSep ", " required}";

  # packagesFromDirectoryRecursive { callPackage, directory }
  #
  # A directory holding a package.nix IS a package; any other directory is a
  # namespace of them.  Adding a package is adding a directory -- nothing
  # enumerates them.  Modelled on nixpkgs'
  # lib.filesystem.packagesFromDirectoryRecursive.
  packagesFromDirectoryRecursive =
    {
      callPackage,
      directory,
    }:
    let
      entries = builtins.readDir directory;

      entryFor =
        name:
        let
          path = directory + "/${name}";
          type = entries.${name};
        in
        if type == "directory" then
          [
            {
              inherit name;
              value = packagesFromDirectoryRecursive {
                inherit callPackage;
                directory = path;
              };
            }
          ]
        else if type == "regular" && hasSuffix ".nix" name then
          [
            {
              name = removeSuffix ".nix" name;
              value = callPackage path { };
            }
          ]
        else
          [ ];
    in
    if builtins.pathExists (directory + "/package.nix") then
      callPackage (directory + "/package.nix") { }
    else
      builtins.listToAttrs (builtins.concatMap entryFor (builtins.attrNames entries));

  hasSuffix =
    suffix: s:
    let
      ls = builtins.stringLength s;
      lf = builtins.stringLength suffix;
    in
    ls >= lf && builtins.substring (ls - lf) lf s == suffix;

  removeSuffix =
    suffix: s:
    if hasSuffix suffix s then builtins.substring 0 (builtins.stringLength s - builtins.stringLength suffix) s else s;
}
