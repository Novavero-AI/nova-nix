# nova-nix Roadmap

## Current State

- **Parser**: Complete. Full Nix syntax including string interpolation, set patterns, inherit, indented strings, paths, URIs.
- **Evaluator**: 85 builtins, polymorphic via `MonadEval` typeclass. `PureEval` for tests, `EvalIO` for real filesystem access.
- **Store**: Real SQLite-backed database (ValidPaths + Refs tables, WAL mode). Full store operations: `addToStore`, `scanReferences`, `setReadOnly`, `writeDrv`, `parseStorePath`.
- **Builder**: Full build loop — input validation, environment setup, process execution, reference scanning, output registration.
- **Derivation**: ATerm round-trip (`toATerm`/`fromATerm`), `builtinDerivation` populates `drvOutputs`.
- **CLI**: `nova-nix eval FILE.nix` and `nova-nix build FILE.nix`.
- **Tests**: 426 tests, zero framework dependencies.
- **Substituter**: Module exists, not yet wired up.

## Phase 3: Dependency Resolution + Substituter

### Dependency Graph Resolution

The builder currently validates that inputs exist but does not recursively build them. Phase 3 adds:

- Walk the derivation dependency graph (input drvs)
- Build (or substitute) missing dependencies before building dependents
- Topological sort for correct build order
- Parallel build support (independent derivations can build concurrently)

### String Contexts

Track store path references through string operations:

- Every string carries a set of referenced store paths
- String concatenation merges context sets
- `builtins.unsafeDiscardStringContext` / `builtins.getContext`
- Context propagates through interpolation, `replaceStrings`, etc.

### Substituter Integration

Check binary caches before building:

- Query narinfo from nova-cache / cache.nixos.org
- Download and verify NAR archives (via nova-cache library)
- Unpack into store, register in DB
- Fall back to building on cache miss

## Phase 4: nixpkgs Compatibility

The ultimate test: `import <nixpkgs> {}` evaluates correctly.

- NIX_PATH / `<nixpkgs>` search path resolution
- Evaluate real nixpkgs (80,000+ packages as one recursive attrset)
- Performance target: ~2-5 seconds for full nixpkgs eval
- Handle all edge cases in builtins, lazy evaluation, and string contexts

## Phase 5: CLI + Developer Experience

- `nova-nix repl` — interactive evaluator
- `nova-nix run` — build and execute
- `nova-nix shell` — enter a development shell
- `nova-nix flake` — flake support
- Nix daemon protocol compatibility
- Package set for Windows-native builds (no MSYS2)
