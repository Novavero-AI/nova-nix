# pkgs/windows' stage-2 stdenv, as the Stage-2 stdenv (Windows) CI jobs
# build it.  Each check forbids substitutes, as the checks kaem builds do
# (see check.nix), so the stdenv builds it on every run.
#
# hello is pkgs/windows' own GNU Hello package called with the stage-2
# stdenv in place of the stage-1 one, so that the one recipe builds with
# either.  flags has the stdenv's commands build the programs of
# stage2-stdenv-flags and writes what flags.sh there says, which CI
# compares with stage2-stdenv-flags-answer.txt.
let
  pkgs = import ../../pkgs/windows;

  building = pkgs.stage2Stdenv // {
    mkDerivation = attrs: pkgs.stage2Stdenv.mkDerivation (attrs // { allowSubstitutes = false; });
  };
in
{
  hello = pkgs.lib.callPackageWith pkgs ../../pkgs/windows/by-name/he/hello/package.nix {
    stdenv = building;
  };

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
