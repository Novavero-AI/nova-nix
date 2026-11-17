# Stress test: function calls, attribute selections and thunk forces
#
# Millions of each, with no file imports.
#
# Run: cabal run nova-nix -- eval stress.nix +RTS -s -M2G

let
  # --- Layer 1: Function call chains ---

  # A plain argument: x: body
  apply1 = f: x: f x;
  apply2 = f: x: f (f x);
  compose = f: g: x: f (g x);

  # A set pattern: { a, b, ... }: body
  addFields = { a, b, ... }: a + b;
  mergeWith = f: { a, b, ... }: f a b;

  # --- Layer 2: Recursive computation (deep thunk chains) ---

  # Each call makes an argument thunk and an environment
  sum = n: if n <= 0 then 0 else n + sum (n - 1);
  fib = n: if n <= 1 then n else fib (n - 1) + fib (n - 2);

  # --- Layer 3: Attribute set construction and selection ---

  mkPkg = i: rec {
    pname = "pkg-${builtins.toString i}";
    version = "1.${builtins.toString i}";
    name = pname + "-" + version;
    meta = {
      inherit pname version;
      pos = i;
      broken = i > 50000;
    };
    # Nested attr selection chain
    fullName = name + (if meta.broken then "-BROKEN" else "");
  };

  # --- Layer 4: List operations (genList + map + filter = lots of thunks) ---

  n = 100000;

  # genList makes n deferred applications
  packages = builtins.genList mkPkg n;

  # map makes n deferred applications
  names = map (p: p.fullName) packages;

  # filter forces its predicate for each element
  shortNames = builtins.filter (name: builtins.stringLength name < 20) names;

  # --- Layer 5: Recursive attr set (the nixpkgs-like pattern) ---

  pkgSet = builtins.listToAttrs (builtins.genList (i: {
    name = "pkg-${builtins.toString i}";
    value = mkPkg i;
  }) 10000);

  # mapAttrs over the set (overlay pattern)
  overlaid = builtins.mapAttrs (name: pkg:
    pkg // { overlaid = true; desc = "overlaid-" + name; }
  ) pkgSet;

in
  # Force everything
  (builtins.length shortNames) + (builtins.length (builtins.attrNames overlaid)) + (sum 5000)
