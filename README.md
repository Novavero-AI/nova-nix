<div align="center">
<h1>nova-nix</h1>
<p><strong>Windows-Native Nix — Haskell Logic, C99 Data</strong></p>
<p>A from-scratch implementation of the Nix package manager — parser, lazy evaluator, content-addressed store, derivation builder, binary substituter — running natively on Windows, macOS, and Linux. No WSL. No Cygwin. No MSYS2.</p>
<p><a href="#quick-start">Quick Start</a> · <a href="#cli">CLI</a> · <a href="#modules">Modules</a> · <a href="#architecture">Architecture</a> · <a href="#the-hard-problems">Hard Problems</a> · <a href="#roadmap">Roadmap</a> · <a href="#build--test">Build & Test</a></p>
<p>

[![CI](https://github.com/Novavero-AI/nova-nix/actions/workflows/ci.yml/badge.svg)](https://github.com/Novavero-AI/nova-nix/actions/workflows/ci.yml)
[![Hackage](https://img.shields.io/hackage/v/nova-nix.svg)](https://hackage.haskell.org/package/nova-nix)
![Haskell](https://img.shields.io/badge/haskell-GHC%209.8-purple)
![License](https://img.shields.io/badge/license-BSD--3--Clause-blue)

</p>
</div>

---

## What is nova-nix?

A pure Haskell implementation of Nix that treats Windows as a first-class target:

- **Parser** — Hand-rolled recursive descent parser for the full Nix expression language. 13 precedence levels, 18 AST constructors, all syntax forms including search paths (`<nixpkgs>`) and dynamic attribute keys (`{ ${expr} = val; }`). Direct `Text` consumption for maximum throughput.
- **Lazy Evaluator** — Thunk-based evaluation with environment closures, knot-tying for recursive bindings via Haskell laziness. All 18 AST constructors handled: literals, strings with interpolation, attribute sets (recursive and non-recursive), let bindings, lambdas with formal parameters, if/then/else, with, assert, unary/binary operators, function application, list construction, attribute selection, has-attribute checks, and search path resolution.
- **108 Built-in Functions** — Type checks, arithmetic (`min`, `max`, `mod`), bitwise, strings, lists, attribute sets, higher-order (`map`, `filter`, `foldl'`, `sort`, `genList`, `concatMap`, `mapAttrs`), JSON (`toJSON`/`fromJSON`), hashing (SHA-256/SHA-512/SHA-1/MD5), base64 (`encode`/`decode`), version parsing, `replaceStrings`, `tryEval`, `deepSeq`, `genericClosure`, `setFunctionArgs`/`functionArgs`, string context introspection (`hasContext`, `getContext`, `appendContext`), IO builtins (`import`, `readFile`, `pathExists`, `readDir`, `getEnv`, `toPath`, `toFile`, `findFile`, `scopedImport`, `fetchurl`, `fetchTarball`, `fetchGit`), `derivation`, `placeholder`, `storePath`, and more. 16 builtins available at top level without `builtins.` prefix (`toString`, `map`, `throw`, `import`, `derivation`, `abort`, `baseNameOf`, `dirOf`, `isNull`, `removeAttrs`, `placeholder`, `scopedImport`, `fetchTarball`, `fetchGit`, `fetchurl`, `toFile`) — matching the real Nix language spec.
- **Search Path Resolution** — `<nixpkgs>` desugars to `builtins.findFile builtins.nixPath "nixpkgs"` — matching real Nix semantics. `NIX_PATH` environment variable is parsed at startup, and `--nix-path` CLI flags merge with it. Directory imports (`import ./dir`) resolve to `dir/default.nix` automatically.
- **Dynamic Attribute Keys** — `{ ${expr} = val; }` fully supported in all contexts: non-recursive attrs, recursive attrs, let bindings, attribute selection, and has-attribute checks. Key resolution is cleanly separated from value thunk construction to preserve knot-tying in recursive bindings.
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
cabal run nova-nix -- --strict eval test.nix
```

Output:

```
{ count = 6; greeting = "Hello, nova-nix!"; items = [ 2 4 6 8 10 ]; nested = { a = 1; b = 2; c = 4; }; types = { attrs = "set"; int = "int"; list = "list"; string = "string"; }; }
```

That's a Nix expression with `let` bindings, `rec` attrs, lambdas, `builtins.map`, `builtins.typeOf`, and arithmetic — parsed, lazily evaluated, and pretty-printed. On Windows, macOS, or Linux.

---

## CLI

```bash
nova-nix eval FILE.nix                        # Evaluate a .nix file, print result
nova-nix eval --expr 'EXPR'                   # Evaluate an inline expression
nova-nix build FILE.nix                       # Build a derivation from a .nix file
nova-nix --nix-path nixpkgs=/path eval FILE   # Add search paths (repeatable)
```

### Evaluate

```bash
$ nova-nix --strict eval test.nix
{ count = 6; greeting = "Hello, nova-nix!"; items = [ 2 4 6 8 10 ]; nested = { a = 1; b = 2; c = 4; }; types = { attrs = "set"; int = "int"; list = "list"; string = "string"; }; }
```

Inline expressions:

```bash
$ nova-nix eval --expr '1 + 2'
3

$ nova-nix eval --expr 'builtins.map (x: x * x) [1 2 3 4 5]'
[ 1 4 9 16 25 ]

$ nova-nix eval --expr '{ x = 1; y = 2; }.x + { x = 1; y = 2; }.y'
3
```

Search paths:

```bash
$ nova-nix --nix-path nixpkgs=/path/to/nixpkgs eval --expr 'import <nixpkgs> {}'

$ NIX_PATH=nixpkgs=/path/to/nixpkgs nova-nix eval --expr 'import <nixpkgs> {}'
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
    Right expr -> case runPureEval (eval (builtinEnv 0 []) expr) of
      Left err  -> putStrLn ("Error: " ++ show err)
      Right val -> print val  -- VInt 11
```

The evaluator is polymorphic via `MonadEval` — `PureEval` runs without IO, while `EvalIO` can access the filesystem for `import`, `readFile`, etc.

### Lazy Evaluation in Action

```haskell
-- Nix is lazy: unused bindings are never evaluated
-- runPureEval (eval (builtinEnv 0 []) expr) where expr parses:
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

| Module | Purpose |
|--------|---------|
| `Nix.Expr` | Re-exports from `Nix.Expr.Types` |
| `Nix.Expr.Types` | Complete Nix AST — 18 expression constructors (including `ESearchPath`, `EResolvedVar`), atoms, formals, operators, string parts |
| `Nix.Expr.Resolve` | De Bruijn-style variable resolution pass — replaces `EVar` with `EResolvedVar` for lambda-bound variables at parse time |
| `Nix.Expr.ClosureTrim` | Closure trimming — statically determines free variables per lambda/with to minimize captured environment size |
| `Nix.Parser` | Hand-rolled recursive descent parser + lexer. Direct `Text` consumption, source position tracking |
| `Nix.Parser.Lexer` | Tokenizer — integers, floats, strings with interpolation, paths, URIs, search paths, all operators/keywords |
| `Nix.Parser.Expr` | Expression parser — 13 precedence levels, left/right/non-associative operators, application, selection, dynamic keys |
| `Nix.Parser.Internal` | Parser state and combinator internals |
| `Nix.Parser.ParseError` | Structured parse errors with source positions |

### Evaluator

| Module | Purpose |
|--------|---------|
| `Nix.Eval` | Lazy evaluator — all 18 AST constructors, thunk forcing, env operations, 108-builtin dispatch, `__functor` callable sets, search path resolution, dynamic attribute keys. Polymorphic via `MonadEval` |
| `Nix.Eval.Types` | Shared types — `NixValue` (12 constructors), `Thunk` (C arena-allocated memoization cell), `Env` (C-allocated positional slots + scope chain), `AttrSet` (C-backed sorted arrays), `StringContext` (store path tracking), `MonadEval` typeclass, `PureEval` runner |
| `Nix.Eval.Operator` | Binary/unary operators — arithmetic with float promotion, deep structural equality, division-by-zero checks |
| `Nix.Eval.StringInterp` | String interpolation — value coercion with context propagation, indented string whitespace stripping |
| `Nix.Eval.Context` | String context construction, queries, extraction — pure helpers for building and inspecting store path references |
| `Nix.Eval.IO` | IO evaluation monad — real filesystem access, import cache (with directory import), process execution, store writes, NIX_PATH parsing, per-thunk C arena memoization with blackhole detection |
| `Nix.Builtins` | Built-in function environment — 108 builtins, search path plumbing (`parseNixPath`), top-level builtin exposure |

### C99 Data Layer

| Module | Purpose |
|--------|---------|
| `Nix.Eval.Symbol` | Interned string symbols via `nn_symbol` — FNV-1a hash table, O(1) string comparison |
| `Nix.Eval.CAttrSet` | C-backed sorted attribute arrays via `nn_attrset` — O(log n) binary search, merge-join operations |
| `Nix.Eval.CThunk` | Arena-allocated 16-byte thunk cells via `nn_thunk` — 9 value tags for inline scalars and C-native types, blackhole detection |
| `Nix.Eval.CEnv` | Page-based slot allocator via `nn_env` — bump allocation for lexical scope arrays |
| `Nix.Eval.CList` | Contiguous thunk pointer arrays via `nn_list` — replaces Haskell cons cells |
| `Nix.Eval.CCtxStr` | Context-bearing strings via `nn_ctxstr` — interned StorePath fields, flexible array of context entries |
| `Nix.Eval.Arena` | Unified arena lifecycle — batch StablePtr collection, coordinated init/destroy of all C sub-arenas |

### Store + Builder

| Module | Purpose |
|--------|---------|
| `Nix.Derivation` | Derivation type, ATerm serialization + parsing (`toATerm`/`fromATerm`), platform detection |
| `Nix.Hash` | Derivation hashing, store path computation, shared hex/base-32 utilities |
| `Nix.Store.Path` | Store path types — `StoreDir`, `StorePath`, `parseStorePath`, Windows/Unix support |
| `Nix.Store.DB` | SQLite store database — `ValidPaths` + `Refs` tables, WAL mode, path registration, reference/deriver queries |
| `Nix.Store` | High-level store operations — `addToStore`, `scanReferences`, `setReadOnly`, `writeDrv` |
| `Nix.Builder` | Derivation builder — dependency graph construction, topological sort, binary cache substitution, local build with output registration |
| `Nix.DependencyGraph` | Dependency graph construction (BFS with `Seq` queue) and topological sort (Kahn's algorithm, O(V+E)), cycle detection |
| `Nix.Substituter` | Binary cache substituter — HTTP narinfo fetch, signature verification, NAR download/decompress/unpack, store registration. Multi-cache with priority ordering |

---

## Architecture

```
                     Pure Core (no IO)
  +-------------------------------------------------+
  |                                                 |
  |  Parser --> Expr.Types --> Eval --> Builtins     |
  |                 |           |                    |
  |          Expr.Resolve    Eval.Types              |
  |          Parser.Lexer    Eval.Operator           |
  |          Parser.Expr     Eval.StringInterp       |
  |          Parser.Internal Eval.Context            |
  |          ParseError                              |
  |                             |                    |
  |                        Derivation --> Hash        |
  |                             |                    |
  |                    Store.Path  DependencyGraph    |
  |                                                 |
  +-------------------------------------------------+
                        |
               IO Boundary (thin)
  +-------------------------------------------------+
  |  Eval.IO   Store.DB   Store   Builder   Substituter|
  +-------------------------------------------------+
                        |
            C99 Data Layer (off GHC heap)
  +-------------------------------------------------+
  |  nn_symbol  nn_attrset  nn_thunk  nn_env        |
  |  nn_list    nn_ctxstr   nn_arena                |
  +-------------------------------------------------+
```

**Evaluator design:**

- **MonadEval typeclass** — The evaluator is `eval :: (MonadEval m) => Env -> Expr -> m NixValue`, polymorphic in its effect monad. `PureEval` (newtype over `Either Text`) runs all pure tests with no IO. `EvalIO` provides `readFileText`, `doesPathExist`, `listDirectory`, `getEnvVar`, `getCurrentTime`, `writeToStore`, `scopedImportFile`, `runProcess` for IO builtins.
- **Thunk-based lazy evaluation with memoization** — List elements and attribute set values are stored as unevaluated thunks. Only forced when a value is demanded. `(x: 1) (throw "boom")` returns `1` because `x` is never referenced. Thunks are 16-byte arena-allocated C cells with blackhole detection for infinite recursion. Inline scalar values (int, float, bool, null) are stored directly in the thunk payload with no Haskell heap allocation. Strings, paths, lists, and attribute sets use C-native storage (interned symbols, sorted arrays, contiguous pointer arrays).
- **C99 data layer** — All bulk evaluation data lives in C arena-allocated structures invisible to the GHC garbage collector. Interned symbols replace string comparison with integer equality. Sorted C arrays replace Haskell `Data.Map`. Arena bump allocators replace per-object malloc. The result: 68% less memory residency and 63% higher GC productivity compared to the pure Haskell baseline.
- **Knot-tying via Haskell laziness** — Recursive `let` and `rec { }` create self-referential environments. Thunks capture environments lazily so they can reference not-yet-filled slot arrays. Two-phase construction (allocate slots, create env, fill slots) ties the knot safely. Dynamic attribute keys are resolved monadically *before* knot-tying — the two-phase design (`resolveBindingKeys` then `buildResolvedBindingsMap`) cleanly separates key evaluation from value thunk construction.
- **Search path desugaring** — `<nixpkgs>` is its own AST constructor (`ESearchPath`), desugared at eval time to `builtins.findFile builtins.nixPath "nixpkgs"` — exactly how real Nix handles it. `builtins.nixPath` is populated from `NIX_PATH` and `--nix-path` flags.
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

- **31 modules** (24 Haskell + 7 C) — all implemented
- **559 tests** — hand-rolled harness, no framework dependencies
- **7 C modules** — ~2,000 lines of C99, `-Wall -Wextra -pedantic -Werror` clean
- **Zero partial functions** — total by construction, `T.uncons` over `T.head`/`T.tail`
- **Strict by default** — bang patterns on all data fields (except Thunk's Env, which is lazy for knot-tying)

---

## The Hard Problems

Building Nix on Windows means solving real platform differences:

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

### C99 Data Layer ([#4](https://github.com/Novavero-AI/nova-nix/issues/4)) — Active

Moving all evaluation data from the GHC heap to C99 arena-allocated structures. Haskell owns eval logic, C owns data layout. Target: 80%+ GC productivity, ~5 GB RSS for full nixpkgs eval.

- [x] **M1: Value tags** — Inline scalars (int/float/bool/null) + C-native strings, paths, lists, attrs in thunk payloads. 68% less memory, 63% higher GC productivity.
- [x] **M3: Context strings + blackhole** — Strings with store path context stored as C structs. Per-thunk blackhole detection for infinite recursion.
- [ ] **M5: Full Env in C** — Replace Haskell `Env` record with C `nn_env_t` struct. Arena-allocated, all fields as C data.
- [ ] **M6: Expr bytecode** — Compile 19-constructor AST to flat C bytecode array. Eliminates all pending thunk StablePtrs (~8M in nixpkgs).
- [ ] **M4: VLambda in C** — Lambda closures as C structs (trivial after M5+M6).

### Next

- [ ] **Full `import <nixpkgs> {}` performance** — nixpkgs lib layer evaluates correctly; C data layer enabling viable memory for the full 80,000+ package set
- [ ] **`nova-nix shell`** — Enter a development shell (like `nix shell`)
- [ ] **`nova-nix repl`** — Interactive evaluator

### Long-Term

- [ ] **Nix daemon protocol compatibility**
- [ ] **XZ decompression** — Enable nova-cache compression flag for real binary cache downloads
- [ ] **Store bootstrap** — Ship prebuilt bash + coreutils for Windows builds

---

## Build & Test

```bash
cabal build                              # Build library + CLI
cabal test                               # Run all 559 tests
cabal build --ghc-options="-Werror"      # Warnings as errors (CI default)
cabal haddock                            # Generate API docs
```

Requires GHC 9.8 and cabal-install 3.10+.

---

<p align="center">
  <sub>BSD-3-Clause · <a href="https://github.com/Novavero-AI">Novavero AI</a></sub>
</p>
