# nova-nix stage-1 stdenv: mkDerivation.
#
# Wraps the low-level `derivation` builtin so a Windows package builds with just
#
#   (import ./stdenv.nix).mkDerivation { name = "foo"; src = ...; }
#
# instead of a hand-written builder script.  It supplies the seed bash as the
# builder, runs setup.sh (the genericBuild), and exposes the mingw toolchain via
# $ccPath.  Package attrs (buildInputs, configureFlags, makeFlags, buildPhase,
# ...) flow through to setup.sh as $-prefixed environment variables; see its
# header for the full set it reads.
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
        # Canonical /nix/store, like every other store path in derivation
        # text: the builder renders it to the machine's real store at the
        # spawn boundary.  A drive letter written here would be baked into
        # the derivation's hash, and it would be wrong on any other store.
        args = [ "${setup}" ];
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
