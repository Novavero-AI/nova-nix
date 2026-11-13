# pkgs/windows' seed stdenv, as the Bootstrap (Windows) CI jobs build it:
# hello, pkgs/windows' own GNU Hello package called with the seed stdenv in
# place of the stage-1 one, so that the one recipe builds with either.  It
# forbids substitutes, as the checks kaem builds do (see check.nix), so the
# stdenv configures, compiles and installs it on every run.
let
  pkgs = import ../../pkgs/windows;

  building = pkgs.seedStdenv // {
    mkDerivation = attrs: pkgs.seedStdenv.mkDerivation (attrs // { allowSubstitutes = false; });
  };
in
{
  hello = pkgs.lib.callPackageWith pkgs ../../pkgs/windows/by-name/he/hello/package.nix {
    stdenv = building;
  };
}
