{ pkgs ? import <nixpkgs> {} }:

pkgs.haskellPackages.callCabal2nix "nova-nix" ./. {}
