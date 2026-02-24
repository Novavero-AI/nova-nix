# nova-nix demo: evaluates on Windows, macOS, and Linux
#
# Run with:
#   cabal run nova-nix -- eval test.nix

let
  pkgs = {
    hello = derivation {
      name = "hello";
      system = builtins.currentSystem;
      builder = "/bin/sh";
      args = ["-c" "mkdir -p $out && echo 'Hello from nova-nix!' > $out/greeting"];
    };
  };
in {
  greeting = "nova-nix is alive";
  count = 1 + 2 + 3;
  items = map (x: x * 2) [1 2 3 4 5];
  hasDerivation = pkgs.hello.type;
}
