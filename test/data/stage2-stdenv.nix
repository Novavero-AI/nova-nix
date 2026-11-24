# pkgs/windows' stage-2 stdenv, as the Stage-2 stdenv (Windows) CI jobs
# build it.  Each check forbids substitutes, as the checks kaem builds do
# (see check.nix), so the stdenv builds it on every run.
#
# hello and the five packages after it are pkgs/windows' own, each called
# with the stage-2 stdenv in place of the stage-1 one, so that one recipe
# builds with either, and with each other where one is another's
# buildInputs.  flags has the stdenv's commands build the programs of
# stage2-stdenv-flags and writes what flags.sh there says, which CI
# compares with stage2-stdenv-flags-answer.txt.
let
  pkgs = import ../../pkgs/windows;

  building = pkgs.stage2Stdenv // {
    mkDerivation = attrs: pkgs.stage2Stdenv.mkDerivation (attrs // { allowSubstitutes = false; });
  };

  # A package of pkgs/windows/by-name called with the stage-2 stdenv, and
  # with what else it takes from the stage-2 builds here.
  withStage2 = path: args: pkgs.lib.callPackageWith pkgs (../../pkgs/windows/by-name + path) ({ stdenv = building; } // args);

  zlib = withStage2 "/zl/zlib/package.nix" { };
  libgreet = withStage2 "/li/libgreet/package.nix" { };
in
{
  hello = withStage2 "/he/hello/package.nix" { };
  sed = withStage2 "/se/sed/package.nix" { };
  inherit zlib libgreet;
  zdemo = withStage2 "/zd/zdemo/package.nix" { inherit zlib; };
  greeter = withStage2 "/gr/greeter/package.nix" { inherit libgreet; };

  flags = building.mkDerivation {
    name = "stage2-stdenv-flags";
    src = ./stage2-stdenv-flags;
    dontConfigure = true;
    # The drivers the stdenv's wrap, which flags.sh runs by their paths to
    # see what the wrappers keep from them.
    compilers = "${pkgs.gcc-native-x86_64}/bin";
    buildPhase = "sh ./flags.sh";
    installPhase = ''cp flags.txt "$out"'';
  };
}
