# nova-nix Roadmap

## Current State

- **Parser**: Complete. Handles full Nix syntax including string interpolation, set patterns, inherit, indented strings, paths, URIs.
- **Evaluator**: 73 pure builtins, 306 tests. Covers type checks, arithmetic, bitwise, strings, lists, attrsets, JSON serialization, hashing (SHA256/SHA512/SHA1/MD5), version parsing, `tryEval`, `deepSeq`, `genericClosure`. All pure — no IO builtins yet.
- **Store**: Path computation and SQLite DB schema in place. No write operations yet.
- **Builder**: Derivation type defined. No execution yet.
- **Substituter**: Module exists, not wired up.

## Phase 1: Effect Abstraction + IO Builtins

### MonadEval Refactor — DONE (2026-02-23)

- `MonadEval` typeclass: `throwEvalError`, `catchEvalError`, `readFileText`, `doesPathExist`, `listDirectory`
- `eval :: (MonadEval m) => Env -> Expr -> m NixValue`
- `PureEval` (newtype over `Either Text`) — all 306 tests stay pure, no IO
- IO instance: next step, enables `import`, `readFile`, `pathExists`

### IO Builtins (~27 remaining)

| Builtin | Complexity | Notes |
|---------|-----------|-------|
| `import` | Medium | Parse file + eval in caller's env. Import cache to avoid re-evaluation |
| `readFile` | Trivial | Read file, return VStr |
| `readDir` | Trivial | List directory, classify entries as "regular"/"directory"/"symlink" |
| `pathExists` | Trivial | `doesPathExist` |
| `fetchurl` | Medium | HTTP GET + hash verification. New dep: http-client |
| `fetchTarball` | Medium | Download + extract + hash. Needs tar/gzip deps |
| `fetchGit` | Medium | Shell out to git or use git library |
| `derivation` | Large | Construct Derivation from attrset, compute store path. ~100+ lines of validation |
| `toFile` | Small | Write string to store, return store path |
| `toPath` | Small | Convert string to path |
| `placeholder` | Small | Hash computation for output paths |
| `scopedImport` | Medium | Like import but with modified scope |
| `findFile` | Small | Search NIX_PATH for a file |
| `getEnv` | Trivial | Read environment variable |
| `currentTime` | Trivial | Current Unix timestamp |
| `storePath` | Small | Validate and return a store path |

## Phase 2: Derivation Evaluation

Make `derivation` work end-to-end: attrset in, `VDerivation` out with computed store path.

- Validate required attrs: `name`, `system`, `builder`
- Compute input hash from all dependencies
- Generate `.drv` file content
- Return attrset with `outPath`, `drvPath`, `type = "derivation"`

## Phase 3: Builder

Execute derivations to produce store outputs.

- Sandbox setup (chroot on Linux, job objects on Windows)
- Environment construction (PATH, outputs, inputs)
- Process execution via `CreateProcess`
- Output registration in store DB
- `MonadBuild` typeclass for testability

## Phase 4: Substituter Integration

Check binary caches before building.

- Query narinfo from nova-cache / cache.nixos.org
- Download and verify NAR archives
- Unpack into store
- Fall back to building on cache miss

## Phase 5: CLI + Real-World Evaluation

- `nova-nix eval` — evaluate expressions, print results
- `nova-nix build` — evaluate + build derivations
- `nova-nix repl` — interactive evaluator
- Evaluate real nixpkgs (the ultimate test)

## Long-Term

- Flake support
- Nix daemon protocol compatibility
- Package set for Windows-native builds (no MSYS2)
