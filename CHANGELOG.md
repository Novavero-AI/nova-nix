# Changelog

## 0.1.0.0 — 2026-02-23

### Added
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

### Architecture
- Store paths parameterized via `defaultStoreDirText` — no hardcoded `/nix/store/` strings
- Hash utilities deduplicated into Nix.Hash (single source of truth)
- currentSystem is a constant (not a function), matching real Nix semantics
- textToPlatform lives alongside platformToText in Nix.Derivation
