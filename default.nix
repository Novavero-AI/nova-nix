{
  pkgs ? import (builtins.fetchTarball
    "https://github.com/NixOS/nixpkgs/archive/nixos-24.11.tar.gz") {}
}:

let
  nova-cache-src = builtins.fetchGit {
    url = "https://github.com/Novavero-AI/nova-cache.git";
    ref = "main";
  };
  ram-src = builtins.fetchGit {
    url = "https://github.com/jappeace/ram.git";
    ref = "main";
  };
  haskellPackages = pkgs.haskellPackages.override {
    overrides = self: super: {
      ram = self.callCabal2nix "ram" ram-src {};
      nova-cache = self.callCabal2nix "nova-cache" nova-cache-src {};
    };
  };
in
  haskellPackages.callCabal2nix "nova-nix" ./. {}
