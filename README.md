<div align="center">
<h1>nova-nix</h1>
<p><strong>Windows-Native Nix</strong></p>
<p>A from-scratch Nix implementation in Haskell + C99. Parser, lazy evaluator, content-addressed store, derivation builder, binary substituter. Runs natively on Windows, macOS, and Linux. No WSL. No Cygwin.</p>
<p><a href="#try-it">Try It</a> · <a href="#cli">CLI</a> · <a href="#architecture">Architecture</a> · <a href="#performance">Performance</a> · <a href="#modules">Modules</a> · <a href="#roadmap">Roadmap</a></p>
<p>

[![CI](https://github.com/Novavero-AI/nova-nix/actions/workflows/ci.yml/badge.svg)](https://github.com/Novavero-AI/nova-nix/actions/workflows/ci.yml)
[![Hackage](https://img.shields.io/hackage/v/nova-nix.svg)](https://hackage.haskell.org/package/nova-nix)
![Haskell](https://img.shields.io/badge/haskell-GHC%209.8-purple)
![License](https://img.shields.io/badge/license-BSD--3--Clause-blue)

</p>
</div>

---

## What is nova-nix?

| Layer | What it does |
|-------|-------------|
| **Parser** | Hand-rolled recursive descent. 14 precedence levels, 19 AST constructors, all Nix syntax including `<nixpkgs>`, `${expr}` keys, indented strings. |
| **Evaluator** | Bytecode-compiled lazy evaluation. Thunk memoization with blackhole detection. Knot-tying for recursive `let`/`rec`. Polymorphic via `MonadEval`. |
| **108 Builtins** | Arithmetic, strings, lists, attrsets, higher-order (`map`, `filter`, `foldl'`, `sort`, `genList`, `concatMap`, `mapAttrs`), JSON, hashing, regex, version parsing, `tryEval`, `deepSeq`, `genericClosure`, string contexts, IO (`import`, `readFile`, `pathExists`, `derivation`, `fetchurl`, `fetchTarball`, `fetchGit`), and more. |
| **C99 Data Layer** | 9 arena-allocated C modules for interned symbols, sorted attrsets, thunks, envs, lists, bytecode, lambdas. All eval data off the GHC heap. |
| **Store** | Content-addressed `/nix/store` (or `C:\nix\store`). SQLite metadata, reference scanning, read-only enforcement. |
| **Builder** | Dependency graph via Kahn's toposort. Binary cache substitution before local builds. Multi-output, reference scanning, store registration. |
| **Substituter** | HTTP binary cache protocol. Narinfo parsing, Ed25519 verification, NAR download/unpack. Priority-ordered multi-cache. Built on [nova-cache](https://github.com/Novavero-AI/nova-cache). |

Every module is pure by default. IO at the boundaries only.

---

## Try It

```bash
git clone https://github.com/Novavero-AI/nova-nix.git
cd nova-nix
cabal run nova-nix -- --strict eval test.nix
```

```
{ count = 6; greeting = "Hello, nova-nix!"; items = [ 2 4 6 8 10 ]; nested = { a = 1; b = 2; c = 4; }; types = { attrs = "set"; int = "int"; list = "list"; string = "string"; }; }
```

That's `let` bindings, `rec` attrs, lambdas, `builtins.map`, `builtins.typeOf`, and arithmetic — parsed, lazily evaluated, and printed. On Windows, macOS, or Linux.

---

## CLI

```bash
nova-nix eval FILE.nix                        # Evaluate a .nix file
nova-nix eval --expr 'EXPR'                   # Evaluate an inline expression
nova-nix build FILE.nix                       # Build a derivation
nova-nix --nix-path nixpkgs=/path eval FILE   # Add search paths (repeatable)
```

```bash
$ nova-nix eval --expr '1 + 2'
3

$ nova-nix eval --expr 'builtins.map (x: x * x) [1 2 3 4 5]'
[ 1 4 9 16 25 ]

$ NIX_PATH=nixpkgs=/path/to/nixpkgs nova-nix eval --expr '(import <nixpkgs/lib>).trivial.version'
"24.11pre-git"
```

```bash
$ nova-nix build hello.nix
/nix/store/abc...-hello
```

The `build` command evaluates, extracts the derivation, builds the dependency graph, checks binary caches, builds locally, and registers outputs.

---

## Architecture

```
                    Pure Core (no IO)
  +------------------------------------------------+
  |                                                |
  |  Parser --> Expr.Types --> Eval --> Builtins    |
  |                 |           |                   |
  |          Expr.Resolve    Eval.Types             |
  |          Expr.ClosureTrim Eval.Operator         |
  |          Parser.Lexer    Eval.Compile           |
  |          Parser.Expr     Eval.StringInterp      |
  |                             |                   |
  |                        Derivation --> Hash       |
  |                             |                   |
  |                    Store.Path  DependencyGraph   |
  |                                                |
  +------------------------------------------------+
                       |
              IO Boundary (thin)
  +------------------------------------------------+
  |  Eval.IO  Store.DB  Store  Builder  Substituter |
  +------------------------------------------------+
                       |
           C99 Data Layer (off GHC heap)
  +------------------------------------------------+
  |  nn_symbol  nn_attrset  nn_thunk  nn_env       |
  |  nn_list    nn_ctxstr   nn_bytecode nn_lambda  |
  |  nn_arena                                      |
  +------------------------------------------------+
```

**Key design decisions:**

- **Haskell owns eval logic, C99 owns data layout.** The evaluator stays in Haskell. Data structures (attr sets, thunks, envs, values) live in C. Haskell calls C to create, query, and mutate data. C never calls back into Haskell.
- **Bytecode-compiled evaluation.** The 19-constructor Expr AST compiles to a flat 24-opcode bytecode array. The bytecode evaluator (`evalBytecode`) is the sole dispatch path. The AST is GC'd after compilation.
- **MonadEval typeclass.** `PureEval` (newtype over `Either Text`) for deterministic testing. `EvalIO` for real filesystem access. Same `eval` function, different effects.
- **Arena allocation.** Thunks, attr sets, envs, and lambdas are arena-allocated in C. Bulk free at eval end, zero per-object GC overhead. The GHC heap holds only control flow and `StablePtr` handles.
- **Knot-tying via Haskell laziness.** Recursive `let` and `rec {}` use two-phase construction: allocate slots, create env, fill slots. Thunks capture environments lazily via `StablePtr` so they can reference not-yet-filled arrays.
- **String context propagation.** Every `VStr` carries a `StringContext` tracking store path references. The `derivation` builtin collects all context into `drvInputDrvs`/`drvInputSrcs`.

---

## Performance

The C99 data layer moves all evaluation data off the GHC heap:

| Metric | Pure Haskell | After C Data Layer | Change |
|--------|-------------|-------------------|--------|
| Max residency | 69.7 MB | 6.25 MB | **-91%** |
| GC productivity | 1.6% | 56.3% | **+35x** |
| Total memory | ~200 MB | 26 MB | **-87%** |

Measured on a stress test with 100k attribute sets, recursive computations, list operations, and overlay patterns.

**nixpkgs status:** `import <nixpkgs/lib>` evaluates correctly (450 attributes). `lib.fix`, `lib.extends`, `lib.makeExtensible`, `lib.evalModules`, and `lib.systems.elaborate` all work. The stdenv bootstrap stages compute successfully. Full `import <nixpkgs> {}` evaluation is in progress — currently past derivation construction and overlay composition.

---

## Windows Native

nova-nix runs natively on Windows — no compatibility layers, no translation.

| Platform Difference | How nova-nix Handles It |
|---------------------|------------------------|
| No `fork`/`exec` | Win32 `CreateProcess` via `System.Process` |
| No symlinks by default | Developer Mode symlinks; junction point / copy fallback |
| No `/nix/store` | `C:\nix\store` — all paths parameterized, never hardcoded |
| Case-insensitive FS | Content-addressed hashes make collisions impossible |
| 260-char path limit | `\\?\` extended-length prefix (32K chars) |
| No bash | Builder ships `bash.exe` in the store (from MSYS2, same as Git for Windows) |
| stdenv bootstrap | Windows stdenv with MinGW GCC + MSYS2 coreutils in `C:\nix\store` |

---

## Modules

### Parser

| Module | Purpose |
|--------|---------|
| `Nix.Expr.Types` | Complete Nix AST — 18 expression constructors, atoms, formals, operators, string parts |
| `Nix.Expr.Resolve` | De Bruijn-style variable resolution — replaces `EVar` with `EResolvedVar` at parse time |
| `Nix.Expr.ClosureTrim` | Closure trimming — determines free variables per lambda/with to minimize captured env size |
| `Nix.Parser` | Hand-rolled recursive descent parser + lexer, source position tracking |
| `Nix.Parser.Lexer` | Tokenizer — integers, floats, strings with interpolation, paths, URIs, search paths, operators/keywords |
| `Nix.Parser.Expr` | Expression parser — 14 precedence levels, left/right/non-associative operators |

### Evaluator

| Module | Purpose |
|--------|---------|
| `Nix.Eval` | Bytecode evaluator — 24-opcode dispatch, thunk forcing, env operations, 108-builtin dispatch. Polymorphic via `MonadEval` |
| `Nix.Eval.Types` | `NixValue` (12 constructors), `Thunk` (C arena cell), `Env` (C-native struct), `AttrSet` (C sorted arrays), `MonadEval` typeclass |
| `Nix.Eval.Compile` | Bytecode compiler — Expr AST to flat `nn_bytecode` instruction array |
| `Nix.Eval.Operator` | Arithmetic with float promotion, deep structural equality, floored division |
| `Nix.Eval.StringInterp` | String interpolation, `coerceToString`, indented string stripping, float formatting |
| `Nix.Eval.Context` | String context construction, queries, and extraction for derivation building |
| `Nix.Eval.IO` | IO evaluation monad — filesystem access, import cache, process execution, per-thunk memoization with blackhole detection |
| `Nix.Builtins` | 108 builtins, search path plumbing, top-level builtin exposure |

### C99 Data Layer

| Module | Purpose |
|--------|---------|
| `nn_symbol` | FNV-1a hash table for string interning — O(1) comparison |
| `nn_attrset` | Sorted arrays with binary search — O(log n) lookup, merge-join union |
| `nn_thunk` | 16-byte arena cells — 10 value tags for inline scalars and C-native types, blackhole detection |
| `nn_env` | 40-byte arena structs — slots, lazy scope, parent chain, with-scopes, resolved variable lookup |
| `nn_list` | Contiguous thunk pointer arrays |
| `nn_ctxstr` | Context-bearing strings with interned StorePath fields |
| `nn_bytecode` | Flat instruction array — 24 opcodes, 16-byte `nn_op_t` + data buffer |
| `nn_lambda` | Lambda closures — env ptr, body bytecode index, formals |
| `nn_arena` | Unified lifecycle — batch StablePtr cleanup, coordinated init/destroy |

### Store + Builder

| Module | Purpose |
|--------|---------|
| `Nix.Derivation` | Derivation type, ATerm serialization + parsing, platform detection |
| `Nix.Store.Path` | Store path types — `StoreDir`, `StorePath`, Windows/Unix support |
| `Nix.Store.DB` | SQLite store database — WAL mode, path registration, reference queries |
| `Nix.Store` | `addToStore`, `scanReferences`, `setReadOnly`, `writeDrv` |
| `Nix.Builder` | Dependency graph, topological sort, binary cache substitution, local build |
| `Nix.DependencyGraph` | BFS graph construction, Kahn's toposort, cycle detection |
| `Nix.Hash` | Derivation hashing (`DrvHash`), SHA-256, truncated base-32, shared hash utilities |
| `Nix.Substituter` | HTTP binary cache — narinfo, Ed25519 verification, NAR download/unpack |

---

## Roadmap

### Done

- [x] Full Nix parser (14 precedence levels, all syntax forms)
- [x] Lazy bytecode evaluator (24 opcodes, thunk memoization, blackhole detection)
- [x] 108 builtins (matching real Nix spec)
- [x] C99 data layer (9 modules, all eval data off GHC heap)
- [x] Content-addressed store with SQLite metadata
- [x] Derivation builder with dependency resolution
- [x] Binary cache substituter with Ed25519 verification
- [x] String context tracking and propagation
- [x] nixpkgs lib layer evaluation (`lib.fix`, `lib.extends`, `lib.evalModules`)

### Next

- [ ] Full `import <nixpkgs> {}` — remaining fixpoint laziness bug in overlay composition
- [ ] `--system` flag — override `builtins.currentSystem` for cross-platform evaluation
- [ ] Windows stdenv — MinGW GCC + MSYS2 coreutils bootstrap for native Windows builds

### Long-Term

- [ ] `nova-nix shell` — enter a development shell
- [ ] `nova-nix repl` — interactive evaluator
- [ ] Nix daemon protocol compatibility
- [ ] XZ decompression for binary cache downloads

---

## Build & Test

```bash
cabal build                              # Build library + CLI
cabal test                               # Run all 593 tests
cabal build --ghc-options="-Werror"      # Warnings as errors (CI default)
```

Requires GHC 9.8+ and cabal-install 3.10+.

---

## Library Usage

```haskell
import Nix.Parser (parseNix)
import Nix.Eval (eval, NixValue(..), PureEval(..))
import Nix.Builtins (builtinEnv)

main :: IO ()
main = do
  case parseNix "<stdin>" "let x = 5; y = x * 2; in y + 1" of
    Left err -> print err
    Right expr -> case runPureEval (eval (builtinEnv 0 []) expr) of
      Left err  -> putStrLn ("Error: " ++ show err)
      Right val -> print val  -- VInt 11
```

The evaluator is polymorphic via `MonadEval` — `PureEval` for pure tests, `EvalIO` for filesystem access.

---

<p align="center">
  <sub>BSD-3-Clause · <a href="https://github.com/Novavero-AI">Novavero AI</a></sub>
</p>
