{
  pkgs ? import (builtins.fetchTarball
    "https://github.com/NixOS/nixpkgs/archive/nixos-24.11.tar.gz") {}
}:

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
