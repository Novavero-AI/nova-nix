# nova-nix

[![CI](https://github.com/Novavero-AI/nova-nix/actions/workflows/ci.yml/badge.svg)](https://github.com/Novavero-AI/nova-nix/actions/workflows/ci.yml)
[![Hackage](https://img.shields.io/hackage/v/nova-nix.svg)](https://hackage.haskell.org/package/nova-nix)
[![License](https://img.shields.io/badge/license-Apache--2.0-blue)](LICENSE)

nova-nix exists to make [Nix](https://nixos.org) work natively on Windows:
the same language, the same store paths and the same binary caches as
upstream Nix, without WSL or an existing Nix installation.

It is an implementation of Nix in Haskell with a C99 data layer, and it has
its own parser, evaluator, store, builder and binary-cache substituter. It
also builds and runs on macOS and Linux.

nova-nix is experimental. Read [Limitations](#limitations) before relying on
it.

## Status

- **Evaluation matches upstream Nix on the cases CI checks.** On a pinned
  nixpkgs 24.11 revision, `hello.drvPath` and a dependent derivation evaluate
  to the same store paths as Nix 2.24.9, and 23 smaller evaluation cases
  produce the same output. CI runs this comparison on every change. A matching
  `drvPath` means the whole build-time closure behind it matches too. How much
  of the rest of nixpkgs evaluates has not been measured yet ([#29]).
- **Windows builds run natively.** CI builds GNU Hello on a Windows runner
  through a stage-1 stdenv. Its toolchain is 17 MinGW-w64 packages and 22
  MSYS2 packages, each fetched as a fixed-output derivation pinned by SHA-256
  and unpacked into the store. sed, zlib and a small library-linking example
  also have recipes, but CI does not build them. Replacing these binary seeds
  with a toolchain built from source is tracked in [#26].
- **Binary caches work in both directions.** `nova-nix push` uploads a
  closure, and `build --substituter` downloads signed NARs instead of
  building. CI checks this two ways. On Linux, two fixtures are pushed to a
  local nova-cache server and substituted into a second empty store with
  signature verification on: a small tree whose two copies must serialize
  to the same NAR, and a 320 MiB tree that must substitute under a 128 MiB
  heap cap. On every push to main, the Windows job publishes the Hello
  closure it just built and ran to the public cache at
  [cache.novavero.ai](https://cache.novavero.ai), and a second Windows job
  builds the same closure into an empty store with that cache as its only
  substituter, fails if any path is built locally, and runs the result.

### Limitations

- No flakes, no `nix-shell` or `nix develop`, and no daemon or multi-user mode.
- Garbage collection has no runtime roots: `nova-nix store gc` keeps what
  `build --out-link` registered and what the operator roots under the store's
  `.nova-nix/gcroots`, not what a running program has open. `keep-derivations`
  and `keep-outputs` are not implemented, so an unrooted `.drv` is collected.
  `build` creates no result link unless `--out-link` is given, where
  `nix-build` defaults to `./result`.
- On Windows a build's process tree runs in a job object, so stopping a build
  stops everything it started. There is no filesystem or network isolation
  yet ([#25]).
- nova-nix itself needs neither WSL nor Cygwin, but the current Windows stdenv
  runs its build scripts with MSYS2's bash.
- Store paths that contain symlinks need Windows Developer Mode or an elevated
  shell. nova-nix creates native symlinks and fails rather than falling back
  to a copy.

## Install

Each [release](https://github.com/Novavero-AI/nova-nix/releases/latest) has an
archive per platform:

| Platform | Archive |
| --- | --- |
| Linux x86_64 | [`nova-nix-linux-x64.tar.gz`](https://github.com/Novavero-AI/nova-nix/releases/latest/download/nova-nix-linux-x64.tar.gz) |
| macOS arm64 | [`nova-nix-macos-arm64.tar.gz`](https://github.com/Novavero-AI/nova-nix/releases/latest/download/nova-nix-macos-arm64.tar.gz) |
| Windows x86_64 | [`nova-nix-windows-x64.zip`](https://github.com/Novavero-AI/nova-nix/releases/latest/download/nova-nix-windows-x64.zip) |

Each archive unpacks to a single directory holding `bin/`, `share/nova-nix/`
and `pkgs/`. `share/nova-nix/` provides the `<nix/*>` search path, so the
binary works wherever the directory is unpacked, and `pkgs/` holds the package
recipes. Check the download against the `SHA256SUMS` file attached to the same
release.

In a GitHub Actions workflow:

```yaml
- uses: Novavero-AI/install-nova-nix@v1
- run: nova-nix eval --expr '1 + 2'
```

## Usage

```console
$ nova-nix eval --expr '1 + 2'
3
$ nova-nix eval --strict --expr 'builtins.map (x: x * x) [ 1 2 3 4 5 ]'
[ 1 4 9 16 25 ]
```

```console
$ nova-nix eval FILE.nix                          # evaluate a file
$ nova-nix build FILE.nix -A ATTR                 # build an attribute of it
$ nova-nix build FILE.nix --substituter URL --trusted-key KEY  # try a cache first
$ nova-nix build FILE.nix --out-link result       # link the result and root it
$ nova-nix store gc                               # delete everything unrooted
$ nova-nix push --cache URL --key-file KEY --all  # upload every path except derivations
$ nova-nix --help
```

Evaluating a package from nixpkgs, on the 24.11 revision CI pins:

```console
$ NIX_PATH=nixpkgs=/path/to/nixpkgs nova-nix eval --expr \
    '(import <nixpkgs> { system = "x86_64-linux"; config = {}; overlays = []; }).hello.drvPath'
"/nix/store/gciipqhqkdlqqn803zd4a389v86ran45-hello-2.12.1.drv"
```

Building GNU Hello on Windows, from the unpacked release directory:

```console
> bin\nova-nix build pkgs\windows\hello.nix
...
C:\nix\store\<hash>-hello
> C:\nix\store\<hash>-hello\bin\hello.exe
Hello, world!
```

The first build fetches 40 pinned archives (the 39 toolchain packages and the
Hello source), then builds the MSYS2 seed, the MinGW-w64 seed and Hello. The
[public cache](https://cache.novavero.ai) holds this closure as CI last built
it from main, so a build that names the cache downloads what it holds and
builds only what it lacks:

```console
> bin\nova-nix build pkgs\windows\hello.nix --substituter https://cache.novavero.ai --trusted-key cache.novavero.ai-1:9gQ7tLWMM+2tdC9H5sKMJltDIPfD7X2GWlZe8Aa8hHQ=
```

## How it works

- **Parser** (`Nix.Parser`): a hand-written recursive-descent parser for the
  Nix 2.24 language.
- **Evaluator** (`Nix.Eval`): the AST compiles to a small bytecode that a lazy
  evaluator runs, memoizing thunks and detecting infinite recursion. The
  evaluator is generic over `MonadEval`, with a pure instance for tests and an
  IO instance for real evaluation. `derivation` is a lazy wrapper over the
  strict `derivationStrict` primop, as in upstream's
  `src/libexpr/primops/derivation.nix`, so referring to a package does not
  force its build closure.
- **Data layer** (`cbits/`): attribute sets, lists, thunks, environments,
  symbols, context strings, lambdas and bytecode are C99 structures outside
  the GHC heap, freed together when an evaluation session ends. Haskell calls
  into C, and C never calls back.
- **Store** (`Nix.Store`): a content-addressed store at `/nix/store`, or
  `C:\nix\store` on Windows, with SQLite metadata, reference scanning and
  per-path locks that follow upstream's protocol.
- **Builder** (`Nix.Builder`): orders derivations by their dependencies, tries
  substitution first, and runs each builder in a scrubbed environment.
- **Substituter** (`Nix.Substituter`): the HTTP binary-cache protocol, with
  Ed25519 signature checks and xz, zstd and bzip2 decompression, built on
  [nova-cache](https://github.com/Novavero-AI/nova-cache).

On Windows, derivations keep the canonical `/nix/store` spelling that hashes
are computed over, and it is mapped to the real store directory only when a
builder is started. NTFS has no executable bit, so nova-nix keeps it in an
alternate data stream. A store path therefore serializes to the same NAR, with
the same hash, as it does on other platforms.

## Library

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Control.Exception (bracket_)
import Nix.Builtins (builtinEnv)
import Nix.Eval (eval, runPureEval)
import Nix.Eval.Arena (arenaDestroy, arenaInit)
import Nix.Parser (parseNix)

main :: IO ()
main = bracket_ arenaInit arenaDestroy $
  -- The first argument is what relative path literals resolve against.
  case parseNix "/tmp" "<expr>" "let x = 5; in x * 2 + 1" of
    Left err -> print err
    Right expr -> print (runPureEval (eval (builtinEnv 0 []) expr))
```

This prints `Right (VInt 11)`. Evaluation needs the C data layer, so it must
run between `arenaInit` and `arenaDestroy`.

## Building from source

Tested with GHC 9.14.1. CI uses the latest cabal-install release. You only
need this to work on nova-nix itself, not to use a release.

```bash
git clone https://github.com/Novavero-AI/nova-nix.git
cd nova-nix
cabal update
cabal build
cabal test
```

## Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md). Planned work is on the
[Nova roadmap](https://github.com/orgs/Novavero-AI/projects/1) project. Please
report security issues privately through
[GitHub's vulnerability reporting](https://github.com/Novavero-AI/nova-nix/security/advisories/new)
rather than in a public issue.

## License

Apache-2.0. See [LICENSE](LICENSE) and [NOTICE](NOTICE).

[#25]: https://github.com/Novavero-AI/nova-nix/issues/25
[#26]: https://github.com/Novavero-AI/nova-nix/issues/26
[#29]: https://github.com/Novavero-AI/nova-nix/issues/29
