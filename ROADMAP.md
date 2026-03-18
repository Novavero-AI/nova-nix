# nova-nix Roadmap

## Current State (v0.1.4.0)

- **Parser**: Complete. Full Nix syntax including string interpolation, set patterns, inherit, indented strings, paths, URIs, search paths, dynamic attribute keys.
- **Evaluator**: 106 builtins, polymorphic via `MonadEval` typeclass. `PureEval` for tests, `EvalIO` for real filesystem access. Per-thunk IORef memoization matching real Nix. Case-dispatch builtin lookup. `__functor` callable sets. Lazy non-rec attrsets, lazy `//`, lazy with-scopes.
- **Memory Optimization**: Env scope chain (parent pointers, not Map.union — 956MB to ~40MB). ThunkCell release after forcing. Lazy AttrSet defers thunk allocation. Per-binding Env in LazyBinding. Batched formal set matching.
- **nixpkgs Compatibility**: Module system (`lib.evalModules`, `lib.mkOption`, `lib.mkIf`, `lib.types.*`), functional patterns (`lib.makeOverridable`, `lib.makeExtensible`, `lib.fix`), `lib.systems.elaborate`. nixpkgs lib layer evaluates correctly.
- **String Contexts**: Full tracking on `VStr` — `SCPlain`, `SCDrvOutput`, `SCAllOutputs`. Propagated through interpolation, operators, and all string builtins. `derivation` collects contexts into `drvInputDrvs`/`drvInputSrcs`.
- **Store**: Real SQLite-backed database (ValidPaths + Refs tables, WAL mode). Full store operations: `addToStore`, `scanReferences`, `setReadOnly`, `writeDrv`, `parseStorePath`.
- **Builder**: Full build loop with recursive dependency resolution — topological sort, binary cache substitution, local build fallback, output registration.
- **Dependency Graph**: BFS construction with `Data.Sequence` (O(V+E)), Kahn's algorithm topological sort, cycle detection.
- **Substituter**: HTTP binary cache protocol — narinfo fetch/parse, Ed25519 signature verification, NAR download/decompress/unpack, store registration. Multi-cache with priority ordering.
- **Derivation**: ATerm round-trip (`toATerm`/`fromATerm`), `builtinDerivation` populates `drvOutputs` with context-derived inputs.
- **Search Paths**: `<nixpkgs>` desugars to `builtins.findFile builtins.nixPath name`. NIX_PATH parsed at startup. `--nix-path` CLI flag. Bundled `<nix/fetchurl.nix>` for bootstrap.
- **Dynamic Attribute Keys**: `{ ${expr} = val; }` in all contexts. Two-phase resolution preserving knot-tying. Null dynamic keys skip bindings (matching real Nix).
- **Directory Imports**: `import ./dir` resolves to `./dir/default.nix`.
- **CLI**: `nova-nix eval FILE.nix`, `nova-nix eval --expr EXPR`, `nova-nix build FILE.nix`, `--nix-path NAME=PATH`, `--strict`.
- **Tests**: 511 tests, zero framework dependencies.

## Next: Full nixpkgs Performance

The lib layer evaluates correctly. `import <nixpkgs> {}` gets into stdenv bootstrap but consumes all available memory and OOMs regardless of machine (8 GB Mac, 16 GB Windows). Must run with `-M4G` cap to avoid killing other processes. Root cause is GHC per-object overhead (~3x vs C) compounded by nixpkgs being inherently memory-hungry (C++ Nix itself uses 5-10+ GB for full eval — NixOS/nix#8621).

- C FFI data layer to move hot-path structures to C (see #4)
- Profile the eval hot path — identify remaining allocation sources
- Missing builtins that nixpkgs exercises (discovered during real eval)

## Future: CLI + Store Bootstrap

- `nova-nix shell` — enter a development shell
- `nova-nix repl` — interactive evaluator
- `nova-nix run` — build and execute
- **Store bootstrap**: Ship a prebuilt bash + coreutils derivation so nova-nix can build on a fresh Windows machine without external dependencies
- Nix daemon protocol compatibility
- XZ decompression for real binary cache downloads
