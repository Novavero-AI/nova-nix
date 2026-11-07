# nova-nix seed stdenv: mkDerivation on tools built from the seed, the
# replacement for the stage-1 stdenv (../stdenv), whose header says when
# that one is deleted.
#
# The stage-1 stdenv's interface, so a package builds with either:
#
#   { stdenv }:
#   stdenv.mkDerivation { name = "foo"; src = ...; }
#
# with every tool it runs built from stage0-pe32's seed and nothing from the
# MSYS2 or MinGW-w64 packages the stage-1 stdenv is made of: busybox-w32's
# sh is the builder, its applets are the coreutils, sed, grep and awk, GNU
# make is gnumake, and the C compiler is tinycc-native, on msvcrt.dll.
# setup.sh says where its genericBuild differs from the stage-1 stdenv's.
#
# Everything on the ladder is i686, and so is everything this builds.
{
  busybox-w32,
  gnumake,
  tinycc-native,
}:
let
  system = "i686-windows";

  # tcc compiles and links, and finds its runtime and import libraries in
  # the directory -B names.
  tcc = "${tinycc-native.compiler}/bin/tcc.exe";
  cc = "${tcc} -B ${tinycc-native.libs}/lib";

  # busybox-w32's applets as commands, each a launcher named for the applet
  # beside a copy of busybox.exe (see applet.c), for programs other than
  # busybox to find on PATH: GNU make runs a recipe line with no shell
  # syntax itself, through a PATH search for its first word.  Every applet
  # but make, which is gnumake's; busybox --list names busybox too, whose
  # file is busybox.exe itself.
  applets = derivation {
    name = "${busybox-w32.name}-applets";
    inherit system;
    builder = "${busybox-w32}/bin/busybox.exe";
    args = [
      "sh"
      (builtins.toFile "busybox-w32-applets.sh" ''
        set -e
        mkdir -p "$out/bin"
        cp "$busybox" "$out/bin/busybox.exe"
        $cc -Wall -o applet.exe "$launcher"
        names=$("$busybox" --list)
        for applet in $names; do
          case $applet in
            busybox | make) ;;
            *) cp applet.exe "$out/bin/$applet.exe" ;;
          esac
        done
      '')
    ];
    busybox = "${busybox-w32}/bin/busybox.exe";
    inherit cc;
    launcher = ./applet.c;
    inherit (tinycc-native.env) C_INCLUDE_PATH;
  };
in
{
  inherit applets;

  mkDerivation =
    attrs:
    derivation (
      attrs
      // {
        inherit system;
        builder = "${busybox-w32}/bin/busybox.exe";
        args = [
          "sh"
          ./setup.sh
        ];
        initialPath = [
          applets
          gnumake
          tinycc-native.compiler
        ];
        CC = cc;
        # TinyCC is its own linker.  The iconv and gettext checks look for
        # an ld on PATH unless LD names one (lib-ld.m4's AC_LIB_PROG_LD),
        # and fail without; nixpkgs' minimal-bootstrap sets LD to tcc too.
        LD = cc;
        AR = "${tcc} -ar";
        # TinyCC's headers, then what MinGW-w64's have and TinyCC's lack.
        C_INCLUDE_PATH = "${tinycc-native.env.C_INCLUDE_PATH};${./include}";
        CONFIG_SHELL = "${applets}/bin/sh.exe";
        CONFIG_SITE = ./config.site;
        # What config.guess answers for an i686 MinGW environment
        # (*:MINGW*:*), which it cannot work out from busybox's uname, whose
        # system name is Windows_NT.
        buildTriple = "i686-pc-mingw32";
      }
    );
}
