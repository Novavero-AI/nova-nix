# Changelog

## 0.1.7.0 — 2026-03-07

### Build Fix: drvPath Resolution + cmd.exe Quoting

- **Fix: `nova-nix build` drvPath resolution** — `builtinDerivation` now writes `drvPath` and output paths (`out`, `dev`, etc.) into `drvEnv` on the `Derivation` struct. Previously these were only present in the eval result attr set, so `buildAndRegister` could not find the `.drv` store path for dependency resolution (error: "no drvPath available for dependency resolution").
- **Fix: `extractDerivation` returns `StorePath`** — The CLI `build` command now extracts the `drvPath` `StorePath` directly from the eval result alongside the `Derivation`, passing it explicitly to `buildWithDeps`. Eliminates the fragile `Map.lookup "drvPath" drvEnv` fallback.
- **Fix: cmd.exe builder quoting on Windows** — New `mkBuilderProcess` detects when the builder is `cmd.exe` with `/c` and uses `Proc.shell` instead of `Proc.proc`. GHC's `proc` wraps each argument in double-quotes for `CommandLineToArgvW`, but cmd.exe doesn't use that convention — `args = [ "/c" "echo Hello" ]` would fail because cmd.exe tried to find an executable literally named `"echo Hello"`.
- **Fix: UTF-16 auto-detection** — New `readFileAutoEncoding` detects UTF-16 LE/BE and UTF-8 BOM at the byte level before decoding. PowerShell's `>` operator writes UTF-16 LE by default — a Windows-first Nix implementation must handle this at the input boundary. Wired into all 5 file-read sites (Parser, Eval/IO, Main).
- **nova-cache `>= 0.3.0`** — Bumped lower bound to track nova-cache 0.3.x series.
- **`crypton < 1.1` pin** — `http-client-tls` still uses `memory`'s `ByteArrayAccess`; `crypton >= 1.1` switched to `ram`, causing instance mismatches. Pinned in `cabal.project` until `http-client-tls` migrates.
- 108 builtins, 524 tests, -Werror clean, ormolu clean, hlint clean

## 0.1.6.0 — 2026-02-26

### De Bruijn-Style Positional Env + SmallArray Slots

- **De Bruijn-style variable resolution** — New `Nix.Expr.Resolve` pass replaces `EVar` with `EResolvedVar level index` for lambda-bound variables. Called at parse time; all subsequent evaluation uses positional indices instead of name-based `Map` lookups.
- **Array-based Env** — `envSlots :: SmallArray Thunk` replaces `envBindings :: Map Text Thunk`. Lambda formals get O(1) positional indexing via `indexSmallArray` instead of O(log n) `Map.lookup`. Let/rec bindings and builtins remain in name-based `envLazyScope` (already zero Map.Bin overhead).
- **Scope chain** — `Env` uses parent pointer chain instead of `Map.union`. Variable lookup walks the chain: positional slots, then `envLazyScope`, then parent, then with-scopes. Avoids O(n) `Map.union` when extending large envs.
- **Inherit desugaring** — `inherit x;` is desugared to `x = x;` in the resolution pass so inherited lambda formals resolve to `EResolvedVar` (lambda slots have no names at runtime).
- **Heap savings** — Eliminates 29.9M Map.Bin nodes (1.37 GB) from lambda formals. Replaced by SmallArray (one heap object per env, O(1) index) + scope chain parent pointers.
- `primitive` dependency added for `Data.Primitive.SmallArray`
- 108 builtins, 511 tests, -Werror clean, ormolu clean, hlint clean

## 0.1.5.0 — 2026-02-26

### New Builtins, Coercion Fixes, CI Cleanup

- **`builtins.warn`** — prints warning to stderr, returns second arg
- **`builtins.path`** — copies file/dir to store with `SCPlain` context
- **`builtins.seq` fix** — now forces first arg to WHNF (was silent no-op)
- **`builtins.trace`/`traceVerbose` fix** — print to stderr via `MonadEval.traceMessage`
- **`coerceToString` fix** — handle `VAttrs` with `__toString` and `outPath`, enabling `"${pkgs.hello}/bin/hello"` interpolation (was type error on all attrsets)
- **`fetchTarball` fix** — downloads and extracts via curl|tar pipeline (was returning raw .tar.gz)
- **`MonadEval`** — added `traceMessage`, `copyPathToStore` methods
- CI: switched to `haskell-actions/run-ormolu@v16` (matching all Novavero repos)
- 108 builtins, 511 tests, -Werror clean, ormolu clean, hlint clean

## 0.1.4.0 — 2026-02-26

### Memory Optimization + Lazy Non-Rec Attrsets

- **Lazy non-rec attrsets** — `evalNonRecAttrs` now uses `LazyAttrs` to defer thunk+IORef allocation until first access. For `all-packages.nix` (~15k entries per stdenv stage), only accessed packages allocate thunks.
- **Per-binding Env in `LazyBinding`** — Each `LazyExpr`/`LazyInherit`/`LazyInheritFrom` carries its own `Env` instead of sharing one per-attrset. Fixes a bug where `//` merges of `LazyAttrs` from different scopes lost variables — broke `lib.extends` overlay pattern used by `makeExtensibleWithCustomName`.
- **Batched formal set matching** — `matchFormalSet` creates ONE `Env` with all formals instead of N singleton `Env`s. Uses knot-tying so default expressions can reference other formals, including forward references (`{ a ? b, b ? 1 }: a` → `1`).
- **Env scope chain** — `Env` now uses a parent pointer chain instead of `Map.union`. Variable lookup walks the chain: local bindings, then parent, then with-scopes. Avoids O(n) `Map.union` when extending large envs (e.g. 30k-entry nixpkgs rec set). Peak Map.Bin heap: 956MB → ~40MB.
- **ThunkCell release** — On force, `Pending expr env` is overwritten to `Computed val`, dropping all references to `Expr` and `Env` (matching C++ Nix in-place mutation). Previously retained dead closures indefinitely.
- **Lazy `//` operator** — `mergeAttrSets` preserves `LazyAttrs` when possible, merging binding recipe maps instead of materializing all thunks.
- **Lazy with-scopes** — `pushWithScope` accepts `AttrSet` directly so `LazyAttrs` with-scopes stay lazy. `lookupWithScopes` uses `attrSetLookup` to materialize only the accessed key.
- **Key-driven `intersectAttrs`** — Only touches keys present in both sets instead of materializing entire attrsets.
- **New builtins** — `min`, `max`, `mod`, base64 `encode`/`decode` (via nova-cache 0.2.4)
- `nova-cache >= 0.2.4` dependency bump
- 511 tests, -Werror clean, ormolu 0.7.7.0 clean, hlint clean

## 0.1.3.0 — 2026-02-25

### nixpkgs Compatibility: Module System, Callable Sets, Parser Fixes

- **`__functor` support** — Attribute sets with `__functor` are now callable, matching real Nix. `(set.__functor set) arg` dispatch enables nixpkgs patterns like `lib.makeOverridable` and `lib.setFunctionArgs`.
- **`builtins.setFunctionArgs`** — Wraps a function in a callable attrset with `__functor` and `__functionArgs` metadata. Used by `lib.mirrorFunctionArgs`, `lib.makeOverridable`, and the entire nixpkgs override system.
- **Null dynamic keys** — `{ ${null} = val; }` now skips the binding instead of erroring (matching real Nix). Used extensively by the nixpkgs module system for conditional attributes like `${if cond then "key" else null}`.
- **Indented string interpolation fix** — `''${expr}''` now parses correctly. The lexer had an overzealous guard (`isIndStringEscape`) that blocked `''` from opening an indented string when followed by `$`. This broke `lib.generators` and many nixpkgs files.
- **`splitVersion` fix** — Dots are separators, not components. `splitVersion "1.2.3"` now returns `["1" "2" "3"]` (was `["1" "." "2" "." "3"]`). Fixes `lib.versions.majorMinor` and all version comparison logic.
- **`builtins.functionArgs` on callable sets** — Now inspects `__functionArgs` metadata on attrsets produced by `setFunctionArgs`, enabling `lib.functionArgs` to work on overridable functions.
- **CLI eval sub-parser** — `--strict` and `--nix-path` flags now work after `eval` command (e.g. `nova-nix eval --strict --expr '...'`). Previously only worked before the command.
- **Bundled `<nix/fetchurl.nix>`** — Ships as a Cabal data-file, added to search paths automatically. nixpkgs stdenv bootstrap imports this file; no system Nix install needed.
- **nixpkgs module system working** — `lib.evalModules`, `lib.mkOption`, `lib.mkIf`, `lib.types.*` all evaluate correctly.
- **nixpkgs functional patterns working** — `lib.makeOverridable`, `lib.makeExtensible`, `lib.fix`, `lib.generators.toKeyValue` all correct.
- **`lib.systems.elaborate` instant** — Was taking minutes before lazy builtins; now returns immediately for any system string.
- 101 builtins (up from 91), 511 tests

## 0.1.2.0 — 2026-02-24

### Lazy Builtins + Correctness Fixes

- **Lazy `map`** — returns deferred thunks instead of eagerly forcing all elements. `map f [80000 items]` is now O(1) until elements are demanded.
- **Lazy `genList`** — each element is a deferred `f(i)` thunk, forced only on demand
- **Lazy `mapAttrs`** — each attr value is a deferred `f key val` thunk. Critical for nixpkgs which `mapAttrs` over the entire package set.
- **Semi-lazy `concatMap`** — forces applications to discover list structure for concatenation, but element thunks within sub-lists stay lazy
- **`@`-pattern scoping fix** — `{ system ? args.system or ..., ... }@args:` now correctly binds the `@`-name before evaluating defaults, matching real Nix. Previously, default expressions couldn't reference the `@`-binding.
- **Synthetic thunk memo cell fix** — `mkSyntheticThunk` uses env bindings (not expr) for IORef uniqueness, preventing GHC's full-laziness transform from sharing one memo cell across all deferred thunks. Without this, `map f [a b c]` would return `[f(a) f(a) f(a)]`.
- `deferApply` helper: builds thunks wrapping `EApp (EResolvedVar 0 0) (EResolvedVar 0 1)` in a self-contained env with positional slots, reusing existing eval + memoization machinery
- 511 tests (3 new: map laziness, genList laziness, mapAttrs laziness)

## 0.1.1.0 — 2026-02-24

- **Thunk memoization** — Per-thunk `IORef` memo cells (matching real Nix in-place mutation). `forceThunk` is a `MonadEval` method: IO evaluators memoize per-allocation, pure evaluators re-evaluate. GC reclaims dead thunks naturally — no unbounded global cache. `unsafePerformIO` CAF float-out prevented via `NOINLINE` + `seq` pattern.
- **8.4x builtin dispatch speedup** — Case dispatch replacing polymorphic `Map` reconstruction on every call. Direct pattern match on builtin name for zero-allocation dispatch in the hot path.
- **Regex builtins** — `builtins.match` and `builtins.split` via `regex-tdfa` (pure Haskell POSIX ERE, cross-platform)
- `ESearchPath !Text` AST constructor — `<nixpkgs>` is now its own node, desugared at eval time to `builtins.findFile builtins.nixPath "nixpkgs"` (matching real Nix semantics)
- NIX_PATH parsing: `parseNixPath` converts colon-separated `name=path` entries into `{ prefix, path }` attrset thunks
- `builtinEnv` now accepts search paths: `builtinEnv :: Integer -> [Thunk] -> Env`
- `EvalState.esSearchPaths` populated from `NIX_PATH` environment variable at startup
- Directory imports: `import ./dir` automatically resolves to `./dir/default.nix`
- Dynamic attribute keys: `{ ${expr} = val; }` fully supported in all contexts (non-rec, rec, let, select, hasAttr)
- Two-phase binding resolution for knot-tying: `resolveBindingKeys` (monadic, evaluates dynamic keys) then `buildResolvedBindingsMap` (pure, enables recursive self-reference)
- Monadic `resolveKey` replaces pure `resolveStaticKey` — handles both `StaticKey` and `DynamicKey` uniformly
- CLI `--nix-path NAME=PATH` flag (repeatable, merged with NIX_PATH)
- CLI `--expr EXPR` for inline expression evaluation
- CLI deep-force and pretty-printing for eval output
- `README.md` added to `extra-doc-files` in cabal (shows on Hackage)
- Parser fix: `TokInterpOpen` in expression context expects `TokRBrace` (not `TokInterpClose`) for closing brace
- 91 builtins (up from 88)
- 508 tests (14 new: parseNixPath, search path parsing/eval, dynamic keys, directory import, populated search path resolution)

## 0.1.0.0 — 2026-02-24

### Cross-Platform Fixes

- Cross-platform ATerm serialization: `storePathToText` always uses `/` for store paths regardless of OS, `parseStorePath` accepts both `/` and `\`
- Builder inherits system environment, overlays build env via `Map.union` — fixes silent process failures on Windows (missing `SYSTEMROOT`)
- Builder PATH derived from builder location (`buildPath`) — includes builder dir and MSYS2 sibling `usr/bin` for coreutils discovery
- `findTestShell` prefers known Git for Windows bash over WSL launcher (`System32\bash.exe`)
- Parser strips UTF-8 BOM — Windows editors (Notepad, PowerShell) commonly add byte order marks
- Demo `test.nix` included: derivations, lambdas, builtins.map, arithmetic — runs on all platforms
- 16 builtins exposed at top level without `builtins.` prefix (`toString`, `map`, `throw`, `import`, `derivation`, `abort`, `baseNameOf`, `dirOf`, `isNull`, `removeAttrs`, `placeholder`, `scopedImport`, `fetchTarball`, `fetchGit`, `fetchurl`, `toFile`) — matches real Nix language spec, required for nixpkgs compatibility

### String Contexts, Dependency Resolution, Binary Substituter

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

### Content-Addressed Store + Derivation Builder

- Real SQLite-backed store database (`ValidPaths` + `Refs` tables, WAL mode)
- Store operations: `addToStore` (cross-device safe), `scanReferences` (byte-scan for store path references), `setReadOnly` (recursive), `writeDrv`
- `parseStorePath`: parse full store path strings into `StorePath` values
- ATerm parser (`fromATerm`): hand-rolled recursive descent, full round-trip with `toATerm`
- `builtinDerivation` now populates `drvOutputs` with `DerivationOutput` records
- Full `buildDerivation` loop: input validation, temp directory setup, environment construction, process execution via `System.Process`, reference scanning, output registration in store DB
- CLI `nova-nix build FILE.nix`: evaluate, extract derivation, write `.drv`, build, print output path
- 426 tests (45 new: 10 store DB, 13 store ops, 10 ATerm parser, 8 builder, 4 CLI end-to-end)

### Parser, Lazy Evaluator, 85 Builtins

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
