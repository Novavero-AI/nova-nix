# nova-nix

[![CI](https://github.com/Novavero-AI/nova-nix/actions/workflows/ci.yml/badge.svg)](https://github.com/Novavero-AI/nova-nix/actions/workflows/ci.yml)
[![Hackage](https://img.shields.io/hackage/v/nova-nix.svg)](https://hackage.haskell.org/package/nova-nix)
[![License](https://img.shields.io/badge/license-Apache--2.0-blue)](https://github.com/Novavero-AI/nova-nix/blob/main/LICENSE)

nova-nix is an implementation of [Nix](https://nixos.org) that runs natively on
Windows, without WSL or an existing Nix installation: the same language, the
same `/nix/store` paths as upstream Nix on Linux and macOS, and the same binary
caches. It is written in Haskell with a C99 data layer, has its own parser,
evaluator, store, builder and binary-cache substituter, and also runs on macOS
and Linux.

nova-nix is experimental. Read [Limitations](#limitations) before relying on
it.

## Status

- **Evaluation matches upstream Nix on the cases CI checks.** On every change,
  CI evaluates the nixpkgs derivations in [`drvpath-cases.txt`] and the
  expressions in [`eval-cases.sh`] with nova-nix and with upstream Nix, on each
  pairing of Nix and nixpkgs that [`parity.yml`] pins, and requires the same
  results. A matching `drvPath` means the whole build-time closure behind it
  matches too. It also probes `derivation` against upstream's over a corpus of
  calls, apart from the differences [`derivation-divergences.txt`] lists. How
  much of the rest of nixpkgs evaluates is not measured.
- **Windows builds run natively.** CI builds GNU Hello on Windows through a
  stage-1 stdenv whose toolchain is prebuilt MinGW-w64 and MSYS2 packages,
  each pinned by SHA-256 ([`pkgs/windows/bootstrap`]). A toolchain built from
  source is planned to replace them, from the 1,623-byte seed of
  [stage0-pe32] to a GCC that builds the package set. CI builds its start
  from that seed on x64 and ARM64 Windows: stage0-pe32's chain,
  [a Windows port] of GNU Mes, TinyCC as a native Windows compiler, and GNU
  make, built by that TinyCC.
- **Binary caches work in both directions.** `nova-nix push` uploads a closure
  to a [nova-cache] server, which signs it, and a build given a substituter
  downloads signed NARs from an HTTP binary cache instead of building. CI
  substitutes from a local nova-cache server on Linux. On every push to main
  it publishes the Windows Hello closure to the public cache at
  [cache.novavero.ai](https://cache.novavero.ai), then builds that closure into
  an empty store with the cache as its only substituter and fails if any path
  is built locally.

### Limitations

- No flakes, no `nix-shell` or `nix develop`, and no daemon or multi-user mode.
- Evaluation is slower than upstream's, most of all on large evaluations such
  as a NixOS system. The Parity job logs both evaluators' times for each
  derivation case.
- Garbage collection has no runtime roots: `nova-nix store gc` keeps what
  `build --out-link` registered and what the operator roots under the store's
  `.nova-nix/gcroots`, not what a running program has open. `keep-derivations`
  and `keep-outputs` are not implemented, so a rooted output does not keep the
  `.drv` that built it. `build` creates no result link unless `--out-link` is
  given, where `nix-build` defaults to `./result`.
- `--restrict-eval` and `--pure-eval`, and `restrict-eval`, `pure-eval` and
  `allowed-uris` in nix.conf, gate reads, the environment and fetches as
  upstream's modes do, with three differences: the closure of a search path
  root that is a store path is not allowed along with it,
  `builtins.findFile` treats a path the policy refuses as absent, and under
  `--pure-eval` `builtins.nixPath` lists the bundled `<nix/*>` directory,
  where upstream's is empty.
- `builtins.fetchurl` runs `curl`, `builtins.fetchTarball` runs `sh`, `curl` and
  `tar`, and `builtins.fetchGit` runs `git`, each from `PATH`.
- A store path's hash covers the spelling of the store directory. On Windows
  nova-nix hashes over `/nix/store`, as Linux and macOS do. Upstream's local
  store on Windows hashes over its native directory, `%ProgramData%\nix\store`
  by default since Nix 2.35, so the same derivation has a different store path
  in each.
- On Windows a build's process tree runs in a job object, so stopping a build
  stops everything it started. No platform isolates a build's filesystem or
  network; a filesystem sandbox for Windows builds is planned.
- nova-nix itself needs neither WSL nor Cygwin, but the Windows stdenv runs its
  build scripts with MSYS2's bash.
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

Each archive unpacks to a single directory holding `bin/`, `share/nova-nix/`,
`pkgs/` and the license files. `share/nova-nix/` provides the `<nix/*>` search
path, so the binary works wherever the directory is unpacked, and `pkgs/` holds
the package recipes. Check the download against the `SHA256SUMS` file attached
to the same release, with `sha256sum --ignore-missing -c SHA256SUMS`.

In a GitHub Actions workflow:

```yaml
- uses: Novavero-AI/install-nova-nix@v1
- run: nova-nix eval --expr '1 + 2'
```

## Usage

```console
$ nova-nix eval --expr '1 + 2'
3
$ nova-nix build FILE.nix -A ATTR --out-link result
$ nova-nix build FILE.nix --substituter URL --trusted-key KEY --fallback
$ nova-nix store gc
```

`nova-nix --help` lists every command and flag.

Building GNU Hello on Windows, from the unpacked release directory, with the
[public cache](https://cache.novavero.ai) as its substituter:

```console
> bin\nova-nix build pkgs\windows\hello.nix --substituter https://cache.novavero.ai --trusted-key cache.novavero.ai-1:9gQ7tLWMM+2tdC9H5sKMJltDIPfD7X2GWlZe8Aa8hHQ=
...
C:\nix\store\<hash>-hello
> C:\nix\store\<hash>-hello\bin\hello.exe
Hello, world!
```

The cache holds this closure as CI last built it from main, so the build
downloads what the cache holds and builds only what it lacks. Without a
substituter it fetches the pinned toolchain packages and builds the toolchain
before Hello.

## On Windows

The store is `C:\nix\store` by default. Derivations keep the `/nix/store`
spelling that hashes are computed over, and it is mapped to the real directory
only when a builder starts. A file's executable bit is kept in an NTFS
alternate data stream. A store path therefore serializes to the same NAR, with
the same hash, as it does on other platforms.

A path with a root but no drive, such as `/src/default.nix`, names that path on
the store's drive, whichever drive is current: `C:` for the default store, or
the drive of the directory `--store` names.

## Library

nova-nix is also a Haskell library, documented on
[Hackage](https://hackage.haskell.org/package/nova-nix). `Nix.Parser` parses,
`Nix.Eval` evaluates, and `Nix.Store`, `Nix.Builder`, `Nix.Realise` and
`Nix.Substituter` keep the store, build and substitute. Evaluation needs the C
data layer, so it runs between `arenaInit` and `arenaDestroy`, as
`Nix.Eval.Arena` describes.

## Building from source

You need this only to work on nova-nix itself, not to use a release. It needs
GHC and cabal-install; `tested-with` in `nova-nix.cabal` names the GHC version
CI uses.

```bash
git clone https://github.com/Novavero-AI/nova-nix.git
cd nova-nix
cabal update
cabal build
cabal test
```

## Contributing

See [CONTRIBUTING.md]. Planned work is on the
[Nova roadmap](https://github.com/orgs/Novavero-AI/projects/1) project. Please
report security issues privately through
[GitHub's vulnerability reporting](https://github.com/Novavero-AI/nova-nix/security/advisories/new)
rather than in a public issue.

## License

Apache-2.0. See [LICENSE] and [NOTICE].

[`drvpath-cases.txt`]: https://github.com/Novavero-AI/nova-nix/blob/main/.github/parity/drvpath-cases.txt
[`eval-cases.sh`]: https://github.com/Novavero-AI/nova-nix/blob/main/.github/parity/eval-cases.sh
[`parity.yml`]: https://github.com/Novavero-AI/nova-nix/blob/main/.github/workflows/parity.yml
[`derivation-divergences.txt`]: https://github.com/Novavero-AI/nova-nix/blob/main/test/data/derivation-divergences.txt
[`pkgs/windows/bootstrap`]: https://github.com/Novavero-AI/nova-nix/tree/main/pkgs/windows/bootstrap
[stage0-pe32]: https://github.com/Novavero-AI/stage0-pe32
[a Windows port]: https://github.com/Novavero-AI/mes
[nova-cache]: https://github.com/Novavero-AI/nova-cache
[CONTRIBUTING.md]: https://github.com/Novavero-AI/nova-nix/blob/main/CONTRIBUTING.md
[LICENSE]: https://github.com/Novavero-AI/nova-nix/blob/main/LICENSE
[NOTICE]: https://github.com/Novavero-AI/nova-nix/blob/main/NOTICE
