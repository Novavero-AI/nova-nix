# The small amount of lib this package set needs.
#
# nixpkgs gets these from `lib`; nova-nix has no nixpkgs to draw on while
# bootstrapping itself, so the one function that makes a package set a package
# set is defined here, in terms of builtins only.
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
}
