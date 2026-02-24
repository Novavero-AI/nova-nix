<div align="center">
<h1>nova-nix</h1>
<p><strong>Windows-Native Nix in Pure Haskell</strong></p>
<p>A from-scratch implementation of the Nix package manager — parser, lazy evaluator, content-addressed store, derivation builder, binary substituter — running natively on Windows, macOS, and Linux. No WSL. No Cygwin. No MSYS2.</p>
<p><a href="#quick-start">Quick Start</a> · <a href="#cli">CLI</a> · <a href="#modules">Modules</a> · <a href="#architecture">Architecture</a> · <a href="#the-hard-problems">Hard Problems</a> · <a href="#roadmap">Roadmap</a> · <a href="#build--test">Build & Test</a></p>
<p>

[![CI](https://github.com/Novavero-AI/nova-nix/actions/workflows/ci.yml/badge.svg)](https://github.com/Novavero-AI/nova-nix/actions/workflows/ci.yml)
[![Hackage](https://img.shields.io/hackage/v/nova-nix.svg)](https://hackage.haskell.org/package/nova-nix)
![Haskell](https://img.shields.io/badge/haskell-GHC%209.6-purple)
![License](https://img.shields.io/badge/license-MIT-blue)

</p>
</div>

---

## What is nova-nix?

A pure Haskell implementation of Nix that treats Windows as a first-class target:

- **Parser** — Hand-rolled recursive descent parser for the full Nix expression language. 13 precedence levels, all syntax forms. Direct `Text` consumption for maximum throughput.
- **Lazy Evaluator** — Thunk-based evaluation with environment closures, knot-tying for recursive bindings via Haskell laziness. All 16 AST constructors handled: literals, strings with interpolation, attribute sets (recursive and non-recursive), let bindings, lambdas with formal parameters, if/then/else, with, assert, unary/binary operators, function application, list construction, attribute selection, and has-attribute checks.
- **88 Built-in Functions** — Type checks, arithmetic, bitwise, strings, lists, attribute sets, higher-order (`map`, `filter`, `foldl'`, `sort`, `genList`, `concatMap`), JSON (`toJSON`/`fromJSON`), hashing (SHA-256/SHA-512/SHA-1/MD5), version parsing, `replaceStrings`, `tryEval`, `deepSeq`, `genericClosure`, string context introspection (`hasContext`, `getContext`, `appendContext`), IO builtins (`import`, `readFile`, `pathExists`, `readDir`, `getEnv`, `toPath`, `toFile`, `findFile`, `scopedImport`, `fetchurl`, `fetchTarball`, `fetchGit`), `derivation`, `placeholder`, `storePath`, and more. 16 builtins available at top level without `builtins.` prefix (`toString`, `map`, `throw`, `import`, `derivation`, `abort`, `baseNameOf`, `dirOf`, `isNull`, `removeAttrs`, `placeholder`, `scopedImport`, `fetchTarball`, `fetchGit`, `fetchurl`, `toFile`) — matching the real Nix language spec.
- **String Context Tracking** — Every string carries invisible metadata tracking which store paths it references. Context propagates through interpolation, concatenation, `replaceStrings`, and all string operations. The `derivation` builtin collects contexts into `drvInputDrvs` and `drvInputSrcs` — matching real Nix semantics.
- **Content-Addressed Store** — `/nix/store` on Unix, `C:\nix\store` on Windows, with real SQLite metadata tracking (ValidPaths + Refs tables, WAL mode)
- **Derivation Builder** — Full build loop with recursive dependency resolution: topological sort via Kahn's algorithm, binary cache substitution before local builds, input validation, reference scanning, output registration
- **Binary Substituter** — HTTP binary cache protocol: narinfo fetch + parse, Ed25519 signature verification, NAR download/decompress/unpack, store registration. Priority-ordered multi-cache support. Built on [nova-cache](https://github.com/Novavero-AI/nova-cache).
- **ATerm Serialization** — Full round-trip `.drv` serialization and parsing with string escape handling

Every module is pure by default. IO lives at the boundaries only.

---

## Try It

```bash
git clone https://github.com/Novavero-AI/nova-nix.git
cd nova-nix
cabal run nova-nix -- eval test.nix
```

Output:

```
{ count = 6; greeting = "nova-nix is alive"; hasDerivation = "derivation"; items = [ 2 4 6 8 10 ]; }
```

That's a Nix expression with `let` bindings, `derivation`, `builtins.map`, lambda functions, and arithmetic — parsed, lazily evaluated, and pretty-printed. On Windows, macOS, or Linux.

---

## CLI

```bash
nova-nix eval FILE.nix          # Evaluate a .nix file, print result
nova-nix build FILE.nix         # Build a derivation from a .nix file
```

### Evaluate

```bash
$ nova-nix eval test.nix
{ count = 6; greeting = "nova-nix is alive"; hasDerivation = "derivation"; items = [ 2 4 6 8 10 ]; }
```

More examples:

```bash
$ echo '1 + 2' > simple.nix
$ nova-nix eval simple.nix
3

$ echo '{ x = 1; y = 2; }.x + { x = 1; y = 2; }.y' > attrs.nix
$ nova-nix eval attrs.nix
3

$ echo 'builtins.map (x: x * x) [1 2 3 4 5]' > squares.nix
$ nova-nix eval squares.nix
[ 1 4 9 16 25 ]
```

### Build

```bash
$ cat > hello.nix <<'EOF'
derivation {
  name = "hello";
  system = builtins.currentSystem;
  builder = "/bin/sh";
  args = [ "-c" "mkdir -p $out && echo 'Hello from nova-nix!' > $out/greeting.txt" ];
}
EOF

$ nova-nix build hello.nix
/nix/store/abc...-hello
```

The `build` command evaluates the `.nix` file, extracts the derivation, builds the full dependency graph, topologically sorts it, checks binary caches for substitutes, builds anything missing locally, and registers all outputs in the store DB.

---

## Quick Start

Add to your `.cabal` file:

```cabal
build-depends: nova-nix
```

### Parse a Nix Expression

```haskell
import Nix.Parser (parseNix)
import Nix.Expr.Types

main :: IO ()
main = do
  case parseNix "<stdin>" "let x = 1 + 2; in x" of
    Left err   -> print err
    Right expr -> print expr
    -- ELet [NamedBinding [StaticKey "x"]
    --        (EBinary OpAdd (ELit (NixInt 1)) (ELit (NixInt 2)))]
    --      (EVar "x")
```

### Evaluate an Expression

```haskell
import Nix.Parser (parseNix)
import Nix.Eval (eval, PureEval(..), NixValue(..))
import Nix.Builtins (builtinEnv)

main :: IO ()
main = do
  case parseNix "<stdin>" "let x = 5; y = x * 2; in y + 1" of
    Left err -> print err
    Right expr -> case runPureEval (eval (builtinEnv 0) expr) of
      Left err  -> putStrLn ("Error: " ++ show err)
      Right val -> print val  -- VInt 11
```

The evaluator is polymorphic via `MonadEval` — `PureEval` runs without IO, while an IO instance can access the filesystem for `import`, `readFile`, etc.

### Lazy Evaluation in Action

```haskell
-- Nix is lazy: unused bindings are never evaluated
-- runPureEval (eval builtinEnv expr) where expr parses:
--   "let unused = builtins.throw \"boom\"; x = 42; in x"
-- Right (VInt 42)  —  "boom" is never triggered

-- Recursive attribute sets with self-reference
--   "rec { a = 1; b = a + 1; c = b * 2; }.c"
-- Right (VInt 4)

-- Lambda closures, set patterns with defaults
--   "({ name, greeting ? \"Hello\" }: \"${greeting}, ${name}!\") { name = \"Nix\"; }"
-- Right (VStr "Hello, Nix!")
```

---

## Modules

### Parser

| Module | Purpose | Status |
|--------|---------|--------|
| `Nix.Expr.Types` | Complete Nix AST — 16 expression constructors, atoms, formals, operators, string parts, source locations | Done |
| `Nix.Parser` | Hand-rolled recursive descent parser + lexer. Direct `Text` consumption, source position tracking | Done |
| `Nix.Parser.Lexer` | Tokenizer — integers, floats, strings with interpolation, paths, URIs, search paths, all operators/keywords | Done |
| `Nix.Parser.Expr` | Expression parser — 13 precedence levels, left/right/non-associative operators, application, selection | Done |
| `Nix.Parser.Internal` | Parser state and combinator internals | Done |
| `Nix.Parser.ParseError` | Structured parse errors with source positions | Done |

### Evaluator

| Module | Purpose | Status |
|--------|---------|--------|
| `Nix.Eval` | Lazy evaluator — all 16 AST constructors, thunk forcing, env operations, 88-builtin dispatch. Polymorphic via `MonadEval` | Done |
| `Nix.Eval.Types` | Shared types — `NixValue` (11 constructors), `Thunk` (lazy env for knot-tying), `Env` (lexical + with-scope chain), `StringContext` (store path tracking), `MonadEval` typeclass, `PureEval` runner | Done |
| `Nix.Eval.Operator` | Binary/unary operators — arithmetic with float promotion, deep structural equality, division-by-zero checks | Done |
| `Nix.Eval.StringInterp` | String interpolation — value coercion with context propagation, indented string whitespace stripping | Done |
| `Nix.Eval.Context` | String context construction, queries, extraction — pure helpers for building and inspecting store path references | Done |
| `Nix.Eval.IO` | IO evaluation monad — real filesystem access, import cache, process execution, store writes | Done |
| `Nix.Builtins` | Built-in function environment — 88 builtins registered as `VBuiltin` values, dispatched in eval | Done |

### Store + Builder

| Module | Purpose | Status |
|--------|---------|--------|
| `Nix.Derivation` | Derivation type, ATerm serialization + parsing (`toATerm`/`fromATerm`), platform detection | Done |
| `Nix.Hash` | Derivation hashing, store path computation, shared hex/base-32 utilities | Done |
| `Nix.Store.Path` | Store path types — `StoreDir`, `StorePath`, `parseStorePath`, Windows/Unix support | Done |
| `Nix.Store.DB` | SQLite store database — `ValidPaths` + `Refs` tables, WAL mode, path registration, reference/deriver queries | Done |
| `Nix.Store` | High-level store operations — `addToStore`, `scanReferences`, `setReadOnly`, `writeDrv` | Done |
| `Nix.Builder` | Derivation builder — dependency graph construction, topological sort, binary cache substitution, local build with output registration | Done |
| `Nix.DependencyGraph` | Dependency graph construction (BFS with `Seq` queue) and topological sort (Kahn's algorithm, O(V+E)), cycle detection | Done |
| `Nix.Substituter` | Binary cache substituter — HTTP narinfo fetch, signature verification, NAR download/decompress/unpack, store registration. Multi-cache with priority ordering | Done |

---

## Architecture

```
                     Pure Core (no IO)
  ┌───────────────────────────────────────────────────┐
  │                                                   │
  │  Parser ──→ Expr.Types ──→ Eval ──→ Builtins      │
  │                 │           │                      │
  │          Parser.Lexer    Eval.Types                │
  │          Parser.Expr     Eval.Operator             │
  │          Parser.Internal Eval.StringInterp         │
  │          ParseError      Eval.Context              │
  │                             │                      │
  │                        Derivation ──→ Hash         │
  │                             │                      │
  │                    Store.Path  DependencyGraph      │
  │                                                   │
  └───────────────────────────────────────────────────┘
                        │
               IO Boundary (thin)
  ┌───────────────────────────────────────────────────┐
  │  Eval.IO   Store.DB   Store   Builder   Substituter│
  └───────────────────────────────────────────────────┘
```

**Evaluator design:**

- **MonadEval typeclass** — The evaluator is `eval :: (MonadEval m) => Env -> Expr -> m NixValue`, polymorphic in its effect monad. `PureEval` (newtype over `Either Text`) runs all pure tests with no IO. `EvalIO` provides `readFileText`, `doesPathExist`, `listDirectory`, `getEnvVar`, `getCurrentTime`, `writeToStore`, `scopedImportFile`, `runProcess` for IO builtins.
- **Thunk-based lazy evaluation** — List elements and attribute set values are stored as unevaluated thunks (`Thunk Expr Env`). Only forced when a value is demanded. `(x: 1) (throw "boom")` returns `1` because `x` is never referenced.
- **Knot-tying via Haskell laziness** — Recursive `let` and `rec { }` create self-referential environments. The `Thunk` type has a lazy `Env` field so thunks can capture environments that include themselves. Haskell's own laziness resolves the recursion.
- **With-scope chain** — `Env` has lexical bindings (always win) plus a stack of with-scopes walked innermost-first. `let a = 1; in with { a = 2; }; a` correctly returns `1` because lexical scope takes priority.
- **Short-circuit operators** — `&&`, `||`, and `->` are handled directly in eval (not delegated to Operator) because they must not evaluate both operands.
- **String context propagation** — Every `VStr` carries a `StringContext` tracking store path references (`SCPlain`, `SCDrvOutput`, `SCAllOutputs`). Context merges through interpolation, concatenation, and string builtins. The `derivation` builtin collects all context into `drvInputDrvs`/`drvInputSrcs`.

**Build pipeline:**

1. Evaluate `.nix` file to extract derivation
2. Build dependency graph by reading `.drv` files from the store (BFS traversal)
3. Topologically sort via Kahn's algorithm — leaves first, cycle detection
4. For each dependency in build order: check store cache, try binary substitution, build locally
5. Build execution: validate inputs, set up environment, run builder process, scan references, register outputs in SQLite DB

**Key numbers:**

- **22 modules** — all implemented
- **494 tests** — hand-rolled harness, no framework dependencies
- **Zero partial functions** — total by construction, `T.uncons` over `T.head`/`T.tail`
- **Strict by default** — bang patterns on all data fields (except Thunk's Env, which is lazy for knot-tying)

---

## The Hard Problems

Building Nix on Windows means solving problems nobody has fully solved before:

| Problem | Solution |
|---------|----------|
| **No `fork`/`exec`** | `System.Process.createProcess` maps to Win32 `CreateProcess` natively |
| **No symlinks (sometimes)** | Developer Mode enables symlinks; fallback to junction points / copies |
| **`/nix/store` doesn't exist** | `C:\nix\store` as `StoreDir` — all paths parameterized, never hardcoded |
| **Case-insensitive filesystem** | Nix store paths are case-sensitive by content hash — collisions impossible |
| **260-char path limit** | `\\?\` extended-length prefix (32K chars), already used by cargo/node |
| **No bash** | Ship `bash.exe` from MSYS2 (same as Git for Windows) |
| **Sandboxing** | Unsandboxed initially (macOS did this for years); future: Win32 Job Objects + App Containers |
| **stdenv bootstrap** | Cross-compile from Linux, or bootstrap from MSYS2 MinGW toolchain |
| **Cross-device moves** | `renameDirectory` can fail across devices; fallback to recursive copy + remove |

The biggest challenge isn't any single feature — it's **nixpkgs compatibility**. nixpkgs is 80,000+ packages defined as one massive recursive attrset. It exercises every builtin, every edge case in string context tracking, and every lazy evaluation pattern. The evaluator must handle all of this correctly and fast enough (~2-5 seconds for full nixpkgs eval).

---

## Roadmap

### Done

- [x] **Lexer** — Full Nix tokenization (integers, floats, strings with interpolation, paths, URIs, search paths, operators, keywords)
- [x] **Parser** — 13 precedence levels, all Nix syntax, structured error reporting
- [x] **Evaluator** — All 16 AST constructors, lazy thunks, recursive let/rec via knot-tying, with-scope chain
- [x] **88 builtins** — Type checks, arithmetic, bitwise, strings, lists, attrsets, higher-order, JSON, hashing, version parsing, tryEval, deepSeq, genericClosure, string context introspection, all IO builtins, derivation
- [x] **MonadEval refactor** — Evaluator polymorphic in effect monad (`PureEval` for tests, `EvalIO` for real evaluation)
- [x] **IO builtins** — `import`, `readFile`, `pathExists`, `readDir`, `getEnv`, `toPath`, `toFile`, `findFile`, `scopedImport`, `fetchurl`, `fetchTarball`, `fetchGit`, `currentTime`
- [x] **`derivation`** — Attrset to `.drv` build recipe with computed `drvPath` and `outPath`, context-aware input population
- [x] **String context tracking** — `SCPlain`, `SCDrvOutput`, `SCAllOutputs` on every `VStr`, propagated through interpolation, operators, and all string builtins. `hasContext`, `getContext`, `appendContext` introspection builtins.
- [x] **ATerm serialization + parsing** — Full `.drv` round-trip with `toATerm`/`fromATerm`, string escaping, sorted environments
- [x] **SQLite store DB** — `ValidPaths` + `Refs` tables, WAL mode, registration, validity checks, reference/deriver queries
- [x] **Store operations** — `parseStorePath`, `addToStore` (cross-device safe), `scanReferences` (byte-scan), `setReadOnly`, `writeDrv`
- [x] **Dependency graph** — BFS construction with `Data.Sequence` (O(V+E)), topological sort via Kahn's algorithm, cycle detection
- [x] **Builder** — Full build loop with recursive dependency resolution: topo sort, cache check, binary substitution, local build, output registration
- [x] **Binary substituter** — HTTP binary cache protocol: narinfo fetch/parse, Ed25519 signature verification via nova-cache, NAR download/decompress/unpack, store DB registration. Multi-cache with priority ordering.
- [x] **CLI** — `nova-nix eval FILE.nix` and `nova-nix build FILE.nix`
- [x] **494 tests** — parser, evaluator, store, builder, substituter, dependency graph, CLI end-to-end

### Next (Phase 4)

- [ ] **nixpkgs evaluation** — The ultimate test: `import <nixpkgs> {}` evaluates correctly
- [ ] **NIX_PATH / `<nixpkgs>` search path resolution** — Wire up the search path mechanism for real nixpkgs imports
- [ ] **Performance profiling** — Target ~2-5 seconds for full nixpkgs eval
- [ ] **Missing builtins** — Any builtins that nixpkgs exercises which we haven't implemented yet

### Long-Term

- [ ] **`nova-nix shell`** — Enter a development shell (like `nix shell`)
- [ ] **`nova-nix repl`** — Interactive evaluator
- [ ] **Flake support**
- [ ] **Nix daemon protocol compatibility**
- [ ] **Package set for Windows-native builds** (no MSYS2)
- [ ] **XZ decompression** — Enable nova-cache compression flag for real binary cache downloads

---

## Build & Test

```bash
cabal build                              # Build library + CLI
cabal test                               # Run all 494 tests
cabal build --ghc-options="-Werror"      # Warnings as errors (CI default)
cabal haddock                            # Generate API docs
```

Requires GHC 9.6 and cabal-install 3.10+.

---

<p align="center">
  <sub>MIT License · <a href="https://github.com/Novavero-AI">Novavero AI</a></sub>
</p>
