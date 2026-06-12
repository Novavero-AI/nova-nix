# nova-nix stage-1 stdenv: mkDerivation.
#
# Wraps the low-level `derivation` builtin so a Windows package builds with just
#
#   (import ./stdenv.nix).mkDerivation { name = "foo"; src = ...; }
#
# instead of a hand-written builder script.  It supplies the seed bash as the
# builder, runs setup.sh (the genericBuild), and exposes the mingw toolchain via
# $ccPath.  Extra attrs (configureFlags, makeFlags, ...) flow through to the
# build environment.
let
  msysSeed = import ./msys-seed.nix;
  mingwSeed = import ./seed.nix;
  setup = ./setup.sh;
  ccWrapper = ./cc-wrapper.sh;
in
{
  inherit msysSeed mingwSeed;

  mkDerivation =
    attrs:
    derivation (
      attrs
      // {
        system = "x86_64-windows";
        builder = "${msysSeed}/usr/bin/bash.exe";
        # The setup script is a store path (canonical /nix/store); bash reads it
        # via the drive-mounted form.
        args = [ "/cygdrive/c${setup}" ];
        # The mingw toolchain bin, already translated for the build's bash PATH.
        ccPath = "/cygdrive/c${mingwSeed}/mingw64/bin";
        # The cc-wrapper source (a store path); setup.sh installs it on PATH
        # ahead of the toolchain so every compiler call flows through it.
        ccWrapperSrc = ccWrapper;
      }
    );
}
