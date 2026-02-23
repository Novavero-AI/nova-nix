<div align="center">
<h1>nova-nix</h1>
<p><strong>Windows-Native Nix in Pure Haskell</strong></p>
<p>A from-scratch implementation of the Nix package manager — parser, lazy evaluator, content-addressed store, derivation builder, binary substituter — running natively on Windows, macOS, and Linux. No WSL. No Cygwin. No MSYS2.</p>
<p><a href="#quick-start">Quick Start</a> · <a href="#modules">Modules</a> · <a href="#architecture">Architecture</a> · <a href="#the-hard-problems">Hard Problems</a> · <a href="#roadmap">Roadmap</a> · <a href="#build--test">Build & Test</a></p>
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

- **Parser** — Hand-rolled recursive descent parser for the full Nix expression language. 13 precedence levels, all syntax forms. No Megaparsec (hnix proved that was a 10x performance bottleneck).
- **Lazy Evaluator** — Thunk-based evaluation with environment closures, knot-tying for recursive bindings via Haskell laziness. All 16 AST constructors handled: literals, strings with interpolation, attribute sets (recursive and non-recursive), let bindings, lambdas with formal parameters, if/then/else, with, assert, unary/binary operators, function application, list construction, attribute selection, and has-attribute checks.
- **17 Built-in Functions** — Type checking (`typeOf`, `isNull`, `isInt`, `isFloat`, `isBool`, `isString`, `isList`, `isAttrs`, `isFunction`), list operations (`length`, `head`, `tail`), string operations (`toString`, `stringLength`), control flow (`throw`, `abort`), and `currentSystem`.
- **Content-Addressed Store** — `/nix/store` on Unix, `C:\nix\store` on Windows, with SQLite metadata tracking (scaffold)
- **Derivation Types** — ATerm serialization, platform detection via `System.Info` (scaffold)
- **Hash Integration** — Built on [nova-cache](https://github.com/Novavero-AI/nova-cache) for SHA-256, Nix base32, NAR, narinfo, and Ed25519 signing

Every module is pure by default. IO lives at the boundaries only.

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
import Nix.Eval (eval, NixValue(..))
import Nix.Builtins (builtinEnv)

main :: IO ()
main = do
  case parseNix "<stdin>" "let x = 5; y = x * 2; in y + 1" of
    Left err -> print err
    Right expr -> case eval builtinEnv expr of
      Left err  -> putStrLn ("Error: " ++ show err)
      Right val -> print val  -- VInt 11
```

### Lazy Evaluation in Action

```haskell
-- Nix is lazy: unused bindings are never evaluated
eval builtinEnv =<< parseNix "<stdin>"
  "let unused = builtins.throw \"boom\"; x = 42; in x"
-- Right (VInt 42)  —  "boom" is never triggered

-- Recursive attribute sets with self-reference
eval builtinEnv =<< parseNix "<stdin>"
  "rec { a = 1; b = a + 1; c = b * 2; }.c"
-- Right (VInt 4)

-- Lambda closures, set patterns with defaults
eval builtinEnv =<< parseNix "<stdin>"
  "({ name, greeting ? \"Hello\" }: \"${greeting}, ${name}!\") { name = \"Nix\"; }"
-- Right (VStr "Hello, Nix!")
```

---

## Modules

### Core (Implemented)

| Module | Purpose | Status |
|--------|---------|--------|
| `Nix.Expr.Types` | Complete Nix AST — 16 expression constructors, atoms, formals, operators, string parts, source locations | Done |
| `Nix.Parser` | Hand-rolled recursive descent parser + lexer. Direct `Text` consumption, source position tracking | Done |
| `Nix.Parser.Lexer` | Tokenizer — integers, floats, strings with interpolation, paths, URIs, search paths, all operators/keywords | Done |
| `Nix.Parser.Expr` | Expression parser — 13 precedence levels, left/right/non-associative operators, application, selection | Done |
| `Nix.Parser.Internal` | Parser state and combinator internals | Done |
| `Nix.Parser.ParseError` | Structured parse errors with source positions | Done |
| `Nix.Eval` | Lazy evaluator — all 16 AST constructors, thunk forcing, env operations, builtin dispatch | Done |
| `Nix.Eval.Types` | Shared types — `NixValue` (11 constructors), `Thunk` (lazy env for knot-tying), `Env` (lexical + with-scope chain) | Done |
| `Nix.Eval.Operator` | Binary/unary operators — arithmetic with float promotion, deep structural equality, division-by-zero checks | Done |
| `Nix.Eval.StringInterp` | String interpolation — value coercion, indented string whitespace stripping | Done |
| `Nix.Builtins` | Built-in function environment — 17 builtins registered as `VBuiltin` values, dispatched in eval | Done |

### Infrastructure (Scaffold)

| Module | Purpose | Status |
|--------|---------|--------|
| `Nix.Derivation` | Derivation type, ATerm serialization, platform detection | Types done, serialization stub |
| `Nix.Hash` | Derivation hashing, store path computation | Stub |
| `Nix.Store.Path` | Store path types — `StoreDir`, `StorePath`, Windows/Unix support | Done |
| `Nix.Store.DB` | SQLite store database — path registration, reference tracking | Stub |
| `Nix.Store` | High-level store operations | Stub |
| `Nix.Builder` | Derivation builder — `CreateProcess`-based execution | Stub |
| `Nix.Substituter` | Binary cache substituter — nova-cache integration | Stub |

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
  │          ParseError                                │
  │                             │                      │
  │                        Derivation ──→ Hash         │
  │                             │                      │
  │                         Store.Path                 │
  │                                                   │
  └───────────────────────────────────────────────────┘
                        │
               IO Boundary (thin)
  ┌───────────────────────────────────────────────────┐
  │  Store.DB    Store    Builder    Substituter       │
  └───────────────────────────────────────────────────┘
```

**Evaluator design:**

- **Thunk-based lazy evaluation** — List elements and attribute set values are stored as unevaluated thunks (`Thunk Expr Env`). Only forced when a value is demanded. `(x: 1) (throw "boom")` returns `1` because `x` is never referenced.
- **Knot-tying via Haskell laziness** — Recursive `let` and `rec { }` create self-referential environments. The `Thunk` type has a lazy `Env` field so thunks can capture environments that include themselves. Haskell's own laziness resolves the recursion.
- **With-scope chain** — `Env` has lexical bindings (always win) plus a stack of with-scopes walked innermost-first. `let a = 1; in with { a = 2; }; a` correctly returns `1` because lexical scope takes priority.
- **Short-circuit operators** — `&&`, `||`, and `->` are handled directly in eval (not delegated to Operator) because they must not evaluate both operands.

**Key numbers:**

- **18 modules** — 11 implemented, 7 scaffold
- **166 tests** — hand-rolled harness, no framework dependencies
- **Zero partial functions** — total by construction
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

The biggest challenge isn't any single feature — it's **nixpkgs compatibility**. nixpkgs is 80,000+ packages defined as one massive recursive attrset. It exercises every builtin, every edge case in string context tracking, and every lazy evaluation pattern. The evaluator must handle all of this correctly and fast enough (~2-5 seconds for full nixpkgs eval).

---

## Roadmap

### Done

- [x] **Lexer** — Full Nix tokenization (integers, floats, strings with interpolation, paths, URIs, search paths, operators, keywords)
- [x] **Parser** — 13 precedence levels, all Nix syntax, structured error reporting (101 tests)
- [x] **Evaluator** — All 16 AST constructors, lazy thunks, recursive let/rec via knot-tying, with-scope chain (65 tests)
- [x] **17 builtins** — Type checks, list ops, string ops, control flow, currentSystem

### Next

- [ ] **`import`** — Load and evaluate other `.nix` files from disk
- [ ] **`derivation`** — The fundamental builtin: attrset → `.drv` build recipe
- [ ] **String contexts** — Track store path references through string operations
- [ ] **Full builtins** (~100 total) — `map`, `filter`, `foldl'`, `fetchurl`, `hashString`, `toJSON`, `replaceStrings`, etc.
- [ ] **ATerm serialization** — Read/write `.drv` files
- [ ] **Store operations** — Content-addressed store with SQLite metadata
- [ ] **Substituter** — Download pre-built binaries from binary caches
- [ ] **Builder** — Execute build recipes via `CreateProcess`
- [ ] **nixpkgs evaluation** — The ultimate test: `import <nixpkgs> {}` evaluates correctly

---

## Build & Test

```bash
cabal build                              # Build library + CLI
cabal test                               # Run all 166 tests
cabal build --ghc-options="-Werror"      # Warnings as errors (CI default)
cabal haddock                            # Generate docs
```

Requires GHC 9.6 and cabal-install 3.10+.

---

<p align="center">
  <sub>MIT License · <a href="https://github.com/Novavero-AI">Novavero AI</a></sub>
</p>
