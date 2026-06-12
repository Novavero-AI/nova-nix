# Stress test: pure function-application + thunk pressure
#
# Targets the top allocators from cost-center profiling:
#   evalApp (19.3%), forceThunk (11.4%), evalSelect (8.0%), evalIf (6.5%)
#
# Creates millions of function calls, attr selections, and thunk forces
# without needing file imports.
#
# Run: cabal run nova-nix -- eval stress.nix +RTS -s -M2G

let
  # --- Layer 1: Function call chains (evalApp + matchFormals) ---

  # Simple lambda: x: body - tests FormalName path
  apply1 = f: x: f x;
  apply2 = f: x: f (f x);
  compose = f: g: x: f (g x);

  # Set-pattern lambda: { a, b, ... }: body - tests FormalSet path
  addFields = { a, b, ... }: a + b;
  mergeWith = f: { a, b, ... }: f a b;

  # --- Layer 2: Recursive computation (deep thunk chains) ---

  # Each call creates: argThunk (IORef), Env, SmallArray
  sum = n: if n <= 0 then 0 else n + sum (n - 1);
  fib = n: if n <= 1 then n else fib (n - 1) + fib (n - 2);

  # --- Layer 3: Attr set construction + selection (evalSelect) ---

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

  # genList creates n thunks via mkThunk
  packages = builtins.genList mkPkg n;

  # map creates n thunks via deferApply
  names = map (p: p.fullName) packages;

  # filter creates thunks + forces evalIf per element
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
  # Force everything: this triggers millions of evalApp + forceThunk + evalSelect
  (builtins.length shortNames) + (builtins.length (builtins.attrNames overlaid)) + (sum 5000)
