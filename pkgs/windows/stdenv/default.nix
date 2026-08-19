# nova-nix stage-1 stdenv: mkDerivation.
#
# Wraps the low-level `derivation` builtin so a Windows package builds with just
#
#   { stdenv }:
#   stdenv.mkDerivation { name = "foo"; src = ...; }
#
# callPackage supplies that `stdenv` argument from the package set.
# instead of a hand-written builder script.  It supplies the seed bash as the
# builder, runs setup.sh (the genericBuild), and exposes the mingw toolchain via
# $ccPath.  Package attrs (buildInputs, configureFlags, makeFlags, buildPhase,
# ...) flow through to setup.sh as $-prefixed environment variables; see its
# header for the full set it reads.
let
  msysSeed = import ../bootstrap/msys-seed.nix;
  mingwSeed = import ../bootstrap/seed.nix;
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
        # The mingw toolchain bin as a canonical store path; setup.sh maps it
        # to the build machine's drive-mounted form (the physical drive is a
        # build-time fact, not derivation text).
        ccPath = "${mingwSeed}/mingw64/bin";
        # The cc-wrapper source (a store path); setup.sh installs it on PATH
        # ahead of the toolchain so every compiler call flows through it.
        ccWrapperSrc = ccWrapper;
      }
    );
}
