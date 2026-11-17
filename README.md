# nova-nix

[![CI](https://github.com/Novavero-AI/nova-nix/actions/workflows/ci.yml/badge.svg)](https://github.com/Novavero-AI/nova-nix/actions/workflows/ci.yml)
[![Hackage](https://img.shields.io/hackage/v/nova-nix.svg)](https://hackage.haskell.org/package/nova-nix)
[![License](https://img.shields.io/badge/license-Apache--2.0-blue)](https://github.com/Novavero-AI/nova-nix/blob/main/LICENSE)

nova-nix is an implementation of [Nix](https://nixos.org) for Windows, macOS
and Linux. It evaluates Nix expressions, builds derivations into a Nix store
and downloads from binary caches. Its commands are its own: `nova-nix eval`
and `nova-nix build` take the place of `nix-instantiate --eval` and
`nix-build`.

- **Upstream's derivations.** On each nixpkgs tree [the scoreboard][scoreboard]
  walks, every package `nix-env -qa` lists for x86_64-linux that upstream Nix
  evaluates gets the same drvPath from nova-nix. Its store paths are
  upstream's, so on Linux and macOS it downloads what cache.nixos.org has
  built, checked against the cache's signature.
- **Native on Windows.** It runs with no WSL or existing Nix installation, and
  [`pkgs/windows`] builds a toolchain from source: from [stage0-pe32]'s seed,
  through GNU Mes and TinyCC, to GCC with C++, on x64 Windows and, under its
  x86 emulation, on ARM64 Windows.
- **Rebuild checks.** `build --check` builds a derivation again and compares
  the result with the store's, as `nix-build --check` does.
- **Closure reports.** `trust` lists the fixed-output derivations a closure
  bottoms out in, and which of them run as builders or hold prebuilt code.
- **Cache uploads.** `push` uploads store paths and their closures to a
  [nova-cache] server.

nova-nix is experimental; [Status](#status) says what it does not do.

## Install

Each [release](https://github.com/Novavero-AI/nova-nix/releases/latest) has an
archive per platform:

| Platform | Archive |
| --- | --- |
| Linux x86_64 | [`nova-nix-linux-x64.tar.gz`](https://github.com/Novavero-AI/nova-nix/releases/latest/download/nova-nix-linux-x64.tar.gz) |
| macOS arm64 | [`nova-nix-macos-arm64.tar.gz`](https://github.com/Novavero-AI/nova-nix/releases/latest/download/nova-nix-macos-arm64.tar.gz) |
| Windows x86_64 | [`nova-nix-windows-x64.zip`](https://github.com/Novavero-AI/nova-nix/releases/latest/download/nova-nix-windows-x64.zip) |

An archive unpacks to one directory named after it, holding `bin/`,
`share/nova-nix/`, `pkgs/`, this README and the license files. The executable
finds the Nix expressions it bundles in `share/nova-nix/` from its own
location, so keep the directory together and put its `bin/` on `PATH`. `pkgs/`
holds the Windows package recipes. `SHA256SUMS`, attached to the same release,
lists each archive's checksum.

### macOS and Linux

The macOS executable needs macOS 26 or newer on Apple silicon. The Linux
executable needs glibc 2.38 or newer, and the GMP and zlib libraries
(`libgmp.so.10` and `libz.so.1`).

```sh
curl -fsSLO https://github.com/Novavero-AI/nova-nix/releases/latest/download/nova-nix-macos-arm64.tar.gz
curl -fsSLO https://github.com/Novavero-AI/nova-nix/releases/latest/download/SHA256SUMS
shasum -a 256 --ignore-missing -c SHA256SUMS
tar -xzf nova-nix-macos-arm64.tar.gz -C "$HOME"
export PATH="$HOME/nova-nix-macos-arm64/bin:$PATH"
```

On Linux, the archive is `nova-nix-linux-x64.tar.gz`, its `bin/` is
`$HOME/nova-nix-linux-x64/bin`, and `sha256sum` checks it in place of
`shasum -a 256`. Add the `export` line to your shell's startup file
(`~/.zshrc` or `~/.bashrc`) so that new shells find `nova-nix`.

```console
$ nova-nix eval --expr '1 + 2'
3
```

### Windows

In PowerShell:

```powershell
curl.exe -fsSLO https://github.com/Novavero-AI/nova-nix/releases/latest/download/nova-nix-windows-x64.zip
curl.exe -fsSLO https://github.com/Novavero-AI/nova-nix/releases/latest/download/SHA256SUMS
(Get-FileHash nova-nix-windows-x64.zip).Hash
Select-String nova-nix-windows-x64.zip SHA256SUMS
Expand-Archive nova-nix-windows-x64.zip -DestinationPath "$env:LOCALAPPDATA\Programs" -Force
$env:Path += ";$env:LOCALAPPDATA\Programs\nova-nix-windows-x64\bin"
```

The two hashes printed should match, apart from case. The last line puts
`nova-nix` on `Path` in this shell; for new shells, add
`%LOCALAPPDATA%\Programs\nova-nix-windows-x64\bin` to your user `Path` in
Settings (search for "environment variables"). On Windows 11 on Arm, the x64
archive runs under emulation.

```console
> nova-nix eval --expr '1 + 2'
3
```

### In GitHub Actions

```yaml
- uses: Novavero-AI/install-nova-nix@v1
- run: nova-nix eval --expr '1 + 2'
```

The [action](https://github.com/Novavero-AI/install-nova-nix) installs the
latest release, or the one its `version` input names, on x64 Linux, arm64
macOS and x64 Windows runners, and puts `nova-nix` on `PATH`. A Linux or macOS
runner has to meet the requirements above, as `ubuntu-latest` and
`macos-latest` do.

## Set up

### The store

nova-nix keeps what it builds and downloads in a store directory, with its own
database inside it under `.nova-nix/`. Every command takes `--store DIR` to use
another directory; no setting or environment variable chooses it.

Give nova-nix a store of its own. It does not read upstream Nix's database or
roots, so in a store upstream Nix also manages, `nova-nix store gc` would
delete upstream's paths, and a build would replace the upstream paths it builds
or downloads. `build`, `store gc` and `store delete` refuse such a store.
`eval` refuses it once the evaluation writes to the store, as
`builtins.toFile`, `builtins.path` and the fetchers do, and `trust` once it
fetches a fixed-output input. Where upstream Nix is installed, give nova-nix
another directory with `--store`. A build in a store other than `/nix/store`
or `C:\nix\store` writes that store's directory into its output, so the output
differs from upstream's at the same store path.

Each `nova-nix` process opens the store itself, with no daemon, so whoever
builds needs write access to the store directory.

**Linux.** The store is `/nix/store`, which nova-nix creates when `/nix` is
yours:

```sh
sudo mkdir -p /nix
sudo chown "$USER" /nix
```

**macOS.** The store is `/nix/store` too. The root volume is read-only, so
`/nix` exists only if `/etc/synthetic.conf` creates it, which
[upstream Nix's installer](https://nix.dev/manual/nix/latest/installation/installing-binary.html#macos-installation)
sets up and nova-nix does not. Without it, a command that writes to the store,
such as a build or an evaluation that computes a nixpkgs package's store path,
fails:

```text
error: /nix: createDirectory: permission denied (Read-only file system)
```

Give those commands a store in your home directory, `--store ~/nix/store`, as
the examples below do.

**Windows.** The store is `C:\nix\store`, which nova-nix creates, and it
inherits the drive root's permissions, so every signed-in user can modify it.
Turn on Developer Mode (Settings, For developers): nova-nix makes native
symlinks, inside store paths and for `--out-link`, and Windows allows them only
in Developer Mode or an elevated shell. Without either, building or downloading
a path that holds a symlink fails.

Expressions see store paths as `/nix/store/...` on Windows too, so a
derivation has the same store path whichever platform evaluates it; a build
prints the path under `C:\nix\store`. A path with a root but no drive, such as
`/src/default.nix`, names that path on the store's drive: `C:` for the default
store, or the drive of the directory `--store` names.

### Configuration

nova-nix reads `nix.conf` from a system file, a user file and the
`NIX_CONFIG` environment variable, as upstream Nix does. On macOS and Linux the
files are `/etc/nix/nix.conf` and `~/.config/nix/nix.conf`; on Windows they
are `%ProgramData%\nix\nix.conf` and `%APPDATA%\nix\nix.conf`. `nova-nix --help`
lists the order they apply in, the settings it reads and the variables that
move these files.

nova-nix has no default binary cache (upstream Nix defaults to
cache.nixos.org), so unless `substituters` or `--substituter` names one, every
derivation is built locally.

To use cache.nixos.org on macOS or Linux (this replaces an existing user
file):

```sh
mkdir -p ~/.config/nix
printf '%s\n' 'substituters = https://cache.nixos.org' 'trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=' > ~/.config/nix/nix.conf
```

On Windows, for [cache.novavero.ai](https://cache.novavero.ai), which holds the
Windows packages CI builds (this also replaces an existing file):

```powershell
New-Item -ItemType Directory -Force "$env:APPDATA\nix" | Out-Null
Set-Content "$env:APPDATA\nix\nix.conf" 'substituters = https://cache.novavero.ai', 'trusted-public-keys = cache.novavero.ai-1:9gQ7tLWMM+2tdC9H5sKMJltDIPfD7X2GWlZe8Aa8hHQ='
```

Write the file as UTF-8 without a byte-order mark, as `Set-Content` does for
these lines. After a byte-order mark nova-nix ignores the first setting, or
refuses the file when its first line is a comment. Windows PowerShell 5.1's
`>` writes UTF-16, which nova-nix refuses as a syntax error.

`NIX_CONFIG` holds the same lines and applies over the files, here for one
shell:

```sh
export NIX_CONFIG='substituters = https://cache.nixos.org
trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY='
```

`--substituter URL` and `--trusted-key KEY` add a cache and a key for one
command, on top of what is configured. The examples below pass them, so they
work with or without a `nix.conf`.

## First steps

### Evaluate an expression

```console
$ nova-nix eval --expr '{ a = 1 + 1; }'
{ a = <CODE>; }
$ nova-nix eval --strict --expr '{ a = 1 + 1; }'
{ a = 2; }
```

`eval` prints a value as `nix-instantiate --eval` does. A part of it that
evaluation never needed prints as `<CODE>`, and `--strict` evaluates the whole
value first. A file evaluates the same way:

```console
$ cat greet.nix
let name = "world"; in "Hello, ${name}!"
$ nova-nix eval greet.nix
"Hello, world!"
```

### Build a derivation on macOS or Linux

```console
$ nova-nix build --store ~/nix/store -o result --expr 'derivation { name = "greeting"; system = builtins.currentSystem; builder = "/bin/sh"; args = [ "-c" "echo hello > $out" ]; }'
  [build]  greeting
/Users/me/nix/store/gs1rwhzwrg6p0vf3pk8lxv35d7z2dfiz-greeting
$ cat result
hello
```

`build` prints the output's path on stdout and its progress on stderr.
`-o result` links the output from `./result` and makes it a root that
`nova-nix store gc` keeps; without it nothing is linked. The collector does not
keep the `.drv` behind a rooted output, or a path a running program has open.
On Linux with `/nix` set up, leave out `--store`.

### Build GNU Hello on Windows

From the directory the release unpacked to, with cache.novavero.ai as the
substituter:

```powershell
Set-Location "$env:LOCALAPPDATA\Programs\nova-nix-windows-x64"
```

```console
> nova-nix build pkgs\windows\hello.nix -o result --substituter https://cache.novavero.ai --trusted-key cache.novavero.ai-1:9gQ7tLWMM+2tdC9H5sKMJltDIPfD7X2GWlZe8Aa8hHQ=
  [subst]  hello
C:\nix\store\78n2mrxhg3xgjganf7nfkaznh2j03z68-hello
> .\result\bin\hello.exe
Hello, world!
```

The build downloads what the cache holds and builds only what it lacks. The
first time, that includes the MinGW-w64 toolchain Hello references, about
127 MB to download and 555 MB in the store, which prints no line of its own.
Without a substituter it downloads the prebuilt MinGW-w64 and MSYS2 packages
Hello's stdenv is made of, and builds Hello with them.

Build scripts run in MSYS2's bash from those packages, so nothing else needs
installing. `pkgs\windows\default.nix` is the whole Windows package set, and
`-A` selects one of its packages.

### Get a package from cache.nixos.org on macOS or Linux

Unpack a nixpkgs revision in the working directory, then import and call
it in an expression:

```sh
curl -fsSL https://github.com/NixOS/nixpkgs/archive/6f06d204d8f2d8761d9da78778745974a3216381.tar.gz | tar -xz
mv nixpkgs-6f06d204d8f2d8761d9da78778745974a3216381 nixpkgs
```

```console
$ nova-nix eval --expr '(import ./nixpkgs { }).hello.version'
"2.12.3"
$ nova-nix build --store ~/nix/store -o result --expr '(import ./nixpkgs { }).hello' --substituter https://cache.nixos.org --trusted-key cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=
  [subst]  hello-2.12.3
/Users/me/nix/store/0blwcqcg0swwz9x748rhyzbfyaybmql8-hello-2.12.3
```

A program from cache.nixos.org loads its libraries from `/nix/store`, so it
runs only from a store at `/nix/store`. From `~/nix/store`, macOS stops it with
`Library not loaded: /nix/store/...`. On Linux with `/nix` set up:

```console
$ nova-nix build -o result --expr '(import ./nixpkgs { }).hello' --substituter https://cache.nixos.org --trusted-key cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=
  [subst]  hello-2.12.3
/nix/store/fkv51s1m2gsy6pz71nnyqyprbvfg7sd7-hello-2.12.3
$ ./result/bin/hello
Hello, world!
```

## Status

Not implemented yet, and on the [roadmap]: flakes, `nix-shell` and
`nix develop`, profiles (`nix-env`, `nix profile`; run a program from its store
path or an `--out-link` instead), and a daemon for a multi-user store. Builds
are not sandboxed on any platform: a builder can read and write outside its
build directory and reach the network.

`eval` and `build` take a file or `--expr`, not a directory or a search-path
argument such as `<nixpkgs>` (an expression can use one). They take no `--arg`
or `--argstr` and do not call a function the file evaluates to, and `build`
builds one derivation, where `nix-build` builds each one in a list or set.
`builtins.fetchurl` runs `curl`, `builtins.fetchTarball` runs `sh`, `curl` and
`tar`, and `builtins.fetchGit` runs `git`, each from `PATH`.

## Where to read more

- `nova-nix --help`, also [on the web](https://novavero.ai/docs/nova-nix/cli):
  every command, flag and `nix.conf` setting.
- [Hackage](https://hackage.haskell.org/package/nova-nix): nova-nix as a
  Haskell library.
- [CHANGELOG.md]: what each release changed, and where nova-nix's behavior
  differs from upstream's.
- [`drvpath-cases.txt`], [`eval-cases.sh`] and [`parity.yml`]: what CI checks
  against upstream Nix, and on which pairings of Nix and nixpkgs.
- [The scoreboard's runs][scoreboard]: how nova-nix's evaluation of nixpkgs
  compares with upstream Nix's, in each run's summary.
- [`pkgs/windows`]: the Windows package set, and the toolchain built from
  [stage0-pe32]'s seed.

## Building from source

You need this only to work on nova-nix itself, not to use a release. It needs
GHC and cabal-install, and on Linux zlib's development files (`zlib1g-dev` on
Debian and Ubuntu); `tested-with` in `nova-nix.cabal` names the GHC version CI
uses.

```bash
git clone https://github.com/Novavero-AI/nova-nix.git
cd nova-nix
cabal update
cabal build
cabal test
```

## Contributing

See [CONTRIBUTING.md]. Planned work is on the [Nova roadmap][roadmap]
project. Please report security issues privately through
[GitHub's vulnerability reporting](https://github.com/Novavero-AI/nova-nix/security/advisories/new)
rather than in a public issue.

## License

Apache-2.0. See [LICENSE] and [NOTICE].

[`drvpath-cases.txt`]: https://github.com/Novavero-AI/nova-nix/blob/main/.github/parity/drvpath-cases.txt
[`eval-cases.sh`]: https://github.com/Novavero-AI/nova-nix/blob/main/.github/parity/eval-cases.sh
[`parity.yml`]: https://github.com/Novavero-AI/nova-nix/blob/main/.github/workflows/parity.yml
[scoreboard]: https://github.com/Novavero-AI/nova-nix/actions/workflows/scoreboard.yml
[`pkgs/windows`]: https://github.com/Novavero-AI/nova-nix/tree/main/pkgs/windows
[stage0-pe32]: https://github.com/Novavero-AI/stage0-pe32
[nova-cache]: https://github.com/Novavero-AI/nova-cache
[CHANGELOG.md]: https://github.com/Novavero-AI/nova-nix/blob/main/CHANGELOG.md
[CONTRIBUTING.md]: https://github.com/Novavero-AI/nova-nix/blob/main/CONTRIBUTING.md
[LICENSE]: https://github.com/Novavero-AI/nova-nix/blob/main/LICENSE
[NOTICE]: https://github.com/Novavero-AI/nova-nix/blob/main/NOTICE
[roadmap]: https://github.com/orgs/Novavero-AI/projects/1
