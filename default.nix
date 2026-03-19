{ pkgs ? import <nixpkgs> {} }:

let
  nova-cache-src = builtins.fetchGit {
    url = "https://github.com/Novavero-AI/nova-cache.git";
    ref = "main";
  };
  haskellPackages = pkgs.haskellPackages.override {
    overrides = self: super: {
      nova-cache = self.callCabal2nix "nova-cache" nova-cache-src {};
    };
  };
in
  haskellPackages.callCabal2nix "nova-nix" ./. {}
