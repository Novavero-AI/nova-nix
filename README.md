<div align="center">
<h1>nova-nix</h1>
<p><strong>A Windows-native Nix, from scratch.</strong></p>
<p>Parser, lazy evaluator, content-addressed store, derivation builder, and binary-cache substituter — in Haskell and C99. Runs natively on Windows, macOS, and Linux. No WSL, no Cygwin.</p>

[![CI](https://github.com/Novavero-AI/nova-nix/actions/workflows/ci.yml/badge.svg)](https://github.com/Novavero-AI/nova-nix/actions/workflows/ci.yml)
[![Hackage](https://img.shields.io/hackage/v/nova-nix.svg)](https://hackage.haskell.org/package/nova-nix)
![GHC](https://img.shields.io/badge/GHC-9.8-purple)
![License](https://img.shields.io/badge/license-BSD--3--Clause-blue)

</div>

---

## Status

nova-nix builds natively on Windows. The toolchain is itself a store path (fetched, hash-verified, and unpacked through derivations), and the builder runs it directly:

```console
$ nova-nix build pkgs\windows\hello.nix
  [build]  mingw-w64-x86_64-gcc-16.1.0-5-any.pkg.tar.zst
  ...      (17 fixed-output fetches, one per MSYS2 package)
  [build]  mingw-w64-seed
  [build]  hello
C:\nix\store\4lv3zydln7y7wg0znf56dfmzf5s59h9g-hello

$ C:\nix\store\4lv3zydln7y7wg0znf56dfmzf5s59h9g-hello\hello.exe
Hello from the first native Windows Nix build.
```

The seed (`pkgs/windows/seed.nix`) is stage 0 of a native Windows stdenv: the sha256-pinned runtime closure of MinGW-w64 GCC, merged into one toolchain root by in-process fetch and unpack builtins.

It also evaluates real nixpkgs. `import <nixpkgs> {}` resolves the top-level package set (~23,000 attributes) to weak head normal form, and a package evaluates through the stdenv bootstrap down to a derivation:

```console
$ NIX_PATH=nixpkgs=/path/to/nixpkgs \
    nova-nix eval --expr '(import <nixpkgs> { system = "x86_64-linux"; }).hello.drvPath'
"/nix/store/gciipqhqkdlqqn803zd4a389v86ran45-hello-2.12.1.drv"
```

That `drvPath`, and the 253-derivation closure behind it, byte-matches upstream `nix-instantiate` — verified in CI on the same nixpkgs tree. It targets the Nix 2.24 language.

## Quickstart

```bash
git clone https://github.com/Novavero-AI/nova-nix.git
cd nova-nix
cabal build
```

```console
$ nova-nix eval --expr '1 + 2'
3

$ nova-nix eval --expr 'builtins.map (x: x * x) [ 1 2 3 4 5 ]'
[ 1 4 9 16 25 ]

$ nova-nix eval FILE.nix                       # evaluate a file
$ nova-nix build FILE.nix                       # build a derivation
$ nova-nix --nix-path nixpkgs=/path eval FILE.nix
```

## How it works

Six layers — Haskell for logic, C99 for data:

1. **Parser** (`Nix.Parser`, `Nix.Expr`) — hand-rolled recursive descent; the full Nix grammar to a 19-constructor AST.
2. **Evaluator** (`Nix.Eval`) — the AST compiles to a flat 24-opcode bytecode that a lazy, thunk-memoizing evaluator runs. Recursive `let`/`rec` are knot-tied; reference cycles are caught by blackhole detection. The evaluator is polymorphic over its effect via `MonadEval`: `PureEval` for tests, `EvalIO` for the real thing.
3. **Data layer** (`cbits/nn_*.c`) — nine arena-allocated C99 modules (interned symbols, sorted attrsets, thunks, environments, lists, context strings, bytecode, lambdas) hold evaluation data off the GHC heap. Haskell calls C to create and query it; C never calls back.
4. **Store** (`Nix.Store`) — content-addressed `/nix/store` (`C:\nix\store` on Windows) with SQLite metadata and reference scanning.
5. **Builder** (`Nix.Builder`) — dependency graph, topological sort, and binary-cache substitution before local builds.
6. **Substituter** (`Nix.Substituter`) — the HTTP binary-cache protocol: narinfo parsing, Ed25519 verification, NAR download and unpack. Built on [nova-cache](https://github.com/Novavero-AI/nova-cache).

Two decisions shape the rest. **Haskell owns evaluation, C owns data layout** — bulk eval state lives off the GHC heap, so large evaluations don't thrash the collector. And **`derivation` is a lazy wrapper over the eager `derivationStrict` primop** (as in Nix's `corepkgs/derivation.nix`), so referencing a package never forces its build closure.

## Windows-native

| Unix assumption | How nova-nix handles it |
|---|---|
| `/nix/store` | `C:\nix\store` — every path is parameterized, never hardcoded |
| `fork`/`exec` | `CreateProcess` via `System.Process` |
| Symlinks | Developer-mode symlinks, with junction / copy fallback |
| 260-char path limit | `\\?\` extended-length prefixes |
| A system `bash` | the builder ships `bash.exe` in the store (MSYS2) |

## Roadmap

**Done** — parser, lazy bytecode evaluator, the Nix `builtins` set, the C99 data layer, content-addressed store, derivation builder, binary-cache substituter, `import <nixpkgs> {}` evaluation, derivation-hash parity with upstream Nix (`hello`'s 253-derivation closure byte-matches `nix-instantiate`), and native Windows builds from a store-pinned MinGW-w64 toolchain (stage 0).

**Next**

- Windows stdenv stage 1 — a setup script and compiler wrappers over the seed; rebuild coreutils and bash from source.
- Parity across more of nixpkgs — extend the byte-match check beyond `hello`'s closure.
- Substituter — XZ decompression.

## Library usage

```haskell
import Nix.Parser (parseNix)
import Nix.Eval (PureEval (..), eval)
import Nix.Builtins (builtinEnv)

main :: IO ()
main = case parseNix "<expr>" "let x = 5; in x * 2 + 1" of
  Left err   -> print err
  Right expr -> print (runPureEval (eval (builtinEnv 0 []) expr))  -- Right (VInt 11)
```

The evaluator is polymorphic over `MonadEval`: `PureEval` for deterministic tests, `EvalIO` for filesystem access.

## Build & test

```bash
cabal build
cabal test
```

Requires GHC 9.8+ and cabal-install 3.10+.

---

<p align="center"><sub>BSD-3-Clause · <a href="https://github.com/Novavero-AI">Novavero AI</a></sub></p>
