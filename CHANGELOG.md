# Changelog

## 0.1.0.0 — 2026-02-24

### Cross-Platform Fixes

- Cross-platform ATerm serialization: `storePathToText` always uses `/` for store paths regardless of OS, `parseStorePath` accepts both `/` and `\`
- Builder inherits system environment, overlays build env via `Map.union` — fixes silent process failures on Windows (missing `SYSTEMROOT`)
- Builder PATH derived from builder location (`buildPath`) — includes builder dir and MSYS2 sibling `usr/bin` for coreutils discovery
- `findTestShell` prefers known Git for Windows bash over WSL launcher (`System32\bash.exe`)
- Parser strips UTF-8 BOM — Windows editors (Notepad, PowerShell) commonly add byte order marks
- Demo `test.nix` included: derivations, lambdas, builtins.map, arithmetic — runs on all platforms
- 16 builtins exposed at top level without `builtins.` prefix (`toString`, `map`, `throw`, `import`, `derivation`, `abort`, `baseNameOf`, `dirOf`, `isNull`, `removeAttrs`, `placeholder`, `scopedImport`, `fetchTarball`, `fetchGit`, `fetchurl`, `toFile`) — matches real Nix language spec, required for nixpkgs compatibility

### Phase 3: String Contexts + Dependency Resolution + Substituter

- String context tracking on all `VStr` values: `SCPlain`, `SCDrvOutput`, `SCAllOutputs`
- Context propagation through interpolation, string concatenation, `replaceStrings`, `substring`, `concatStringsSep`, and all string builtins
- `Nix.Eval.Context` module: pure helpers for context construction, queries, and extraction
- `derivation` builtin now collects string contexts into `drvInputDrvs` and `drvInputSrcs`
- New builtins: `hasContext`, `getContext`, `appendContext`
- `Nix.DependencyGraph`: BFS graph construction with `Data.Sequence` (O(V+E)), topological sort via Kahn's algorithm, cycle detection
- `Nix.Substituter`: full HTTP binary cache protocol — narinfo fetch/parse, Ed25519 signature verification, NAR download/decompress/unpack, store DB registration, priority-ordered multi-cache
- `Nix.Builder.buildWithDeps`: recursive dependency resolution — topo sort, cache check, binary substitution, local build fallback
- CLI `nova-nix build` now builds full dependency trees, not just single derivations
- Cleanup pass: eliminated all partial functions (`T.head`/`T.tail` to `T.uncons`, `!!` to safe lookup, `last` to pattern match), flattened deeply nested code into composed functions, `Data.Sequence` BFS queues throughout, semantic section organization
- 494 tests (68 new: string context, context propagation, dependency graph, substituter, build orchestrator)

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
