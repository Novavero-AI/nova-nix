# nova-nix Roadmap

## Current State (Phase 3 Complete)

- **Parser**: Complete. Full Nix syntax including string interpolation, set patterns, inherit, indented strings, paths, URIs.
- **Evaluator**: 88 builtins, polymorphic via `MonadEval` typeclass. `PureEval` for tests, `EvalIO` for real filesystem access.
- **String Contexts**: Full tracking on `VStr` — `SCPlain`, `SCDrvOutput`, `SCAllOutputs`. Propagated through interpolation, operators, and all string builtins. `derivation` collects contexts into `drvInputDrvs`/`drvInputSrcs`.
- **Store**: Real SQLite-backed database (ValidPaths + Refs tables, WAL mode). Full store operations: `addToStore`, `scanReferences`, `setReadOnly`, `writeDrv`, `parseStorePath`.
- **Builder**: Full build loop with recursive dependency resolution — topological sort, binary cache substitution, local build fallback, output registration.
- **Dependency Graph**: BFS construction with `Data.Sequence` (O(V+E)), Kahn's algorithm topological sort, cycle detection.
- **Substituter**: HTTP binary cache protocol — narinfo fetch/parse, Ed25519 signature verification, NAR download/decompress/unpack, store registration. Multi-cache with priority ordering.
- **Derivation**: ATerm round-trip (`toATerm`/`fromATerm`), `builtinDerivation` populates `drvOutputs` with context-derived inputs.
- **CLI**: `nova-nix eval FILE.nix` and `nova-nix build FILE.nix`.
- **Tests**: 494 tests, zero framework dependencies.

## Phase 4: nixpkgs Compatibility

The ultimate test: `import <nixpkgs> {}` evaluates correctly.

- NIX_PATH / `<nixpkgs>` search path resolution for real nixpkgs imports
- Evaluate real nixpkgs (80,000+ packages as one recursive attrset)
- Performance target: ~2-5 seconds for full nixpkgs eval
- Handle all edge cases in builtins, lazy evaluation, and string contexts
- Missing builtins that nixpkgs exercises (discovered during real eval)
- Profile and optimize the eval hot path

## Phase 5: Store Bootstrap + CLI

- **Store bootstrap**: Ship a prebuilt bash + coreutils derivation so nova-nix can build on a fresh Windows machine without Git for Windows. Currently `findTestShell` discovers bash from Git for Windows; bootstrap replaces this with a store path (`/nix/store/xxx-bash-5.2/bin/bash`), matching real Nix's model.
- `nova-nix shell` — enter a development shell (like `nix shell`)
- `nova-nix repl` — interactive evaluator
- `nova-nix run` — build and execute
- `nova-nix flake` — flake support
- Nix daemon protocol compatibility
- Package set for Windows-native builds (no MSYS2)
- XZ decompression (enable nova-cache compression flag for real binary cache downloads)
