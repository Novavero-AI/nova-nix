# nova-nix stage-1 stdenv: mkDerivation on prebuilt MSYS2 and MinGW-w64
# packages.
#
# Transitional.  The stage-2 stdenv (../stage2-stdenv), on the seed
# ladder's toolchain, is the package set's stdenv and builds every package
# this one builds; this is stage1Stdenv there, which no package takes by
# default, and this directory and ../bootstrap are deleted with it.
#
# Wraps the low-level `derivation` builtin so a Windows package builds with just
#
#   { stdenv }:
#   stdenv.mkDerivation { name = "foo"; src = ...; }
#
# instead of a hand-written builder script; callPackage supplies that
# `stdenv` argument from the package set.  It supplies the MSYS2 seed's bash
# as the builder, runs setup.sh (the genericBuild), and exposes the mingw
# toolchain via $ccPath.  Package attrs (buildInputs, configureFlags, makeFlags, buildPhase,
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

  # The packages the two seeds are unpacked from.
  srcs = msysSeed.srcs ++ mingwSeed.srcs;

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
    )
    // {
      # What the package fetches: its src, unless that is a path in the
      # tree.
      srcs = if (attrs.src or null) ? outputHash then [ attrs.src ] else [ ];
    };
}
