# nova-nix demo: evaluates on Windows, macOS, and Linux
#
# Run with:
#   cabal run nova-nix -- --strict eval test.nix

let
  double = x: x * 2;
  greet = name: "Hello, ${name}!";
in {
  greeting = greet "nova-nix";
  count = 1 + 2 + 3;
  items = map double [1 2 3 4 5];
  nested = rec {
    a = 1;
    b = a + 1;
    c = b * 2;
  };
  types = {
    int = builtins.typeOf 42;
    string = builtins.typeOf "hello";
    list = builtins.typeOf [1 2 3];
    attrs = builtins.typeOf {};
  };
}
