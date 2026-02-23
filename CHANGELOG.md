# Changelog

## 0.1.0.0 — 2026-02-23

### Phase 2: Store + Builder

- Real SQLite-backed store database (`ValidPaths` + `Refs` tables, WAL mode)
- Store operations: `addToStore` (cross-device safe), `scanReferences` (byte-scan for store path references), `setReadOnly` (recursive), `writeDrv`
- `parseStorePath`: parse full store path strings into `StorePath` values
- ATerm parser (`fromATerm`): hand-rolled recursive descent, full round-trip with `toATerm`
- `builtinDerivation` now populates `drvOutputs` with `DerivationOutput` records
- Full `buildDerivation` loop: input validation, temp directory setup, environment construction, process execution via `System.Process`, reference scanning, output registration in store DB
- CLI `nova-nix build FILE.nix`: evaluate, extract derivation, write `.drv`, build, print output path
- 426 tests (45 new: 10 store DB, 13 store ops, 10 ATerm parser, 8 builder, 4 CLI end-to-end)

### Phase 1: Parser + Evaluator + Builtins

- Full Nix expression parser (hand-rolled recursive descent, 13 precedence levels)
- Lazy evaluator with thunk-based evaluation, knot-tying for recursive bindings
- 85 builtins: type checks, arithmetic, bitwise, strings, lists, attrsets, higher-order, JSON, hashing, version parsing, tryEval, deepSeq, genericClosure, all IO builtins, derivation
- MonadEval typeclass — evaluator is polymorphic in its effect monad (PureEval for tests, EvalIO for real evaluation)
- IO builtins: import, readFile, pathExists, readDir, getEnv, toPath, toFile, findFile, scopedImport, fetchurl, fetchTarball, fetchGit, currentTime
- derivation builtin: attrset to .drv build recipe with computed drvPath and outPath
- ATerm serialization with string escaping, sorted environments
- placeholder and storePath builtins via nova-cache hashing
- Content-addressed store path types with Windows/Unix support
- Derivation types, platform detection, and textToPlatform/platformToText
- Shared hash utilities in Nix.Hash (SHA-256 hex, truncated base-32, byteToHex)
- CLI: `nova-nix eval FILE.nix` evaluates a .nix file and prints the result
- 381 tests, zero framework dependencies
- CI pipeline: HLint, Ormolu, build with -Werror, test, Hackage publish on tags

### Security

- Total functions only — no `read`, `head`, `tail`, `!!`, `fromJust`, or `Map.!`
- Argument injection prevention in fetch builtins (`--` separator before user URLs)
- Path traversal validation in writeToStore (rejects `/`, `..`, null bytes)
- Content-hashed temp directories for fetchGit (no predictable paths)
- Store paths set read-only after registration (immutability enforcement)

### Architecture

- Store paths parameterized via `StoreDir` — no hardcoded `/nix/store/` strings
- Cross-device safe directory moves (rename with copy+remove fallback)
- Hash utilities deduplicated into Nix.Hash (single source of truth)
- currentSystem is a constant (not a function), matching real Nix semantics
- Platform-aware environment setup (HOME vs USERPROFILE, Unix vs Windows PATH)
