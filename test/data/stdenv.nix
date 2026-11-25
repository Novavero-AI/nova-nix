# pkgs/windows' stdenv, as the Stdenv (Windows) CI jobs build it.  Each
# check forbids substitutes, as the checks kaem builds do (see check.nix),
# so the stdenv builds it on every run.
#
# hello and the five packages after it are pkgs/windows' own, each called
# with that stdenv, and with each other where one is another's
# buildInputs.  flags has the stdenv's commands build the programs of
# stdenv-flags and writes what flags.sh there says, which CI compares
# with stdenv-flags-answer.txt.
let
  pkgs = import ../../pkgs/windows;

  building = pkgs.stdenv // {
    mkDerivation = attrs: pkgs.stdenv.mkDerivation (attrs // { allowSubstitutes = false; });
  };

  # A package of pkgs/windows/by-name called with the stdenv, and with
  # what else it takes from the builds here.
  withStdenv = path: args: pkgs.lib.callPackageWith pkgs (../../pkgs/windows/by-name + path) ({ stdenv = building; } // args);

  zlib = withStdenv "/zl/zlib/package.nix" { };
  libgreet = withStdenv "/li/libgreet/package.nix" { };
in
{
  hello = withStdenv "/he/hello/package.nix" { };
  sed = withStdenv "/se/sed/package.nix" { };
  inherit zlib libgreet;
  zdemo = withStdenv "/zd/zdemo/package.nix" { inherit zlib; };
  greeter = withStdenv "/gr/greeter/package.nix" { inherit libgreet; };

  flags = building.mkDerivation {
    name = "stdenv-flags";
    src = ./stdenv-flags;
    dontConfigure = true;
    # The drivers the stdenv's wrap, which flags.sh runs by their paths to
    # see what the wrappers keep from them.
    compilers = "${pkgs.gcc-native-x86_64}/bin";
    buildPhase = "sh ./flags.sh";
    installPhase = ''cp flags.txt "$out"'';
  };
}
