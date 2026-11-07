# pkgs/windows' seed stdenv, as the Bootstrap (Windows) CI jobs build it:
# hello, pkgs/windows' own GNU Hello package called with the seed stdenv in
# place of the stage-1 one, so that the one recipe builds with either.
let
  pkgs = import ../../pkgs/windows;
in
{
  hello = pkgs.lib.callPackageWith pkgs ../../pkgs/windows/by-name/he/hello/package.nix {
    stdenv = pkgs.seedStdenv;
  };
}
