# Changelog

## Unreleased

- **Substitution accepts bzip2, so historical cache paths and narinfos with no Compression field at all can be fetched.** cache.nixos.org narinfos written before the xz switch declare `Compression: bzip2`, and upstream C++ Nix reads an absent or empty Compression field as bzip2, so those paths could only ever fail here: the register named the codec in its rejection but no decoder stood behind the name. Both decompression paths - streaming and the strict oracle - now dispatch it through nova-cache 0.11's bzip2 sublibrary, bounded by the declared NarSize exactly as xz and zstd are, with stream errors retried and output past the bound refused as the served object misdeclaring; there is no decoder-memory knob to set, because bzip2 carries no attacker-chosen dictionary size and its decoder state is a small constant of the format. The empty spelling now decodes rather than rejects, completing the upstream parity the register was written for. The bound rises to nova-cache >= 0.11 && < 0.12 with the bzip2 sublibrary added to the library's set, and the pinned index-state advances past the release. 0.11 also tightens the zstd codec, which the suite now pins: a truncated frame and trailing bytes after the last frame refuse with a stream error instead of yielding a short output, so truncation is caught at the codec rather than left to the NAR grammar above it. (#115)
- **Cancellation is never spent as retry budget nor as cache fallthrough.** The retry classifier already re-threw asynchronous exceptions, but the per-cache catch-all one frame up converted every exception into a substitution error, so a Ctrl-C or timeout arriving mid-download was swallowed, the cache scan continued, and the build fell through to building locally instead of aborting. The sync/async split now lives in one shared helper used by the retry classifier, the per-cache attempt, and the builder's top-level catch-all: only synchronous exceptions convert into recoverable failures, and an interrupt propagates out of the whole operation.
- **Per-store-path locks close the substitution race.** The streaming substituter deleted the destination before the first byte downloaded, with no lock and no validity re-check anywhere, so two processes sharing a store could interleave delete, materialize, and register into a valid database row pointing at deleted or torn bytes. Substitution now takes an exclusive lock on upstream's "\<store-path\>.lock" file - a raw write-access descriptor locked through filelock (flock on POSIX, LockFileEx on Windows; CC0-licensed, so nothing encumbers the Apache-2.0 distribution), because base's Handle-based locks cannot express this lock - before touching the destination, re-checks validity under the lock and adopts another process's finished path instead of deleting and redoing it (waiting on a busy lock is announced, as upstream does), and holds the lock across download, materialization, the on-disk recheck, and the caller's registration transaction, releasing it on every exit path. The protocol is upstream C++ Nix's pathlocks mechanism (src/libstore/pathlocks.cc and the substitution path in local-store.cc) exactly. One deliberate divergence, documented in Nix.Store.Lock: lock files persist instead of being marked and deleted, which removes upstream's deleted-lock-file hazard outright and needs no separate Windows path.

- **store delete joins the per-path lock protocol, and lock files are not delete targets.** The delete command ran its unregister-and-remove sequence with no lock, so a delete racing another process's substitution of the same path could remove the freshly materialized tree between the substituter's on-disk recheck and its registration commit - the exact valid-row-pointing-at-deleted-bytes race the locks close, reopened through the other door. Deletion now holds the target's path lock across the whole sequence, blocking (with the announced wait) while a substituter holds it, as upstream's deletePath does. A target naming a lock file is refused with the reason: lock files are never deleted here by design, and removing one would reopen the deleted-lock-file hazard that design removes. Names like flake.lock are legal store-path names, so the refusal consults the registration rows: only an unregistered file whose suffix-stripped name is a well-formed store basename reads as a lock file, and a registered object of the same shape deletes normally.

- **zstd, both directions: substitution accepts it, push can produce it.** The modern caches (Cachix, attic, FlakeHub) serve NARs zstd-compressed; both decompression paths - streaming and the strict oracle - now speak it through nova-cache 0.10's zstandard sublibrary, bounded by the declared NarSize like xz (the strict decompressor's resolved function moves into IO: the zstd binding's decoder is IO-native, and the shape follows the codecs). Push gains --compression none|zstd, a property of the destination cache as upstream models it - none stays the default so existing cache content stays uniform - with the compressed object named by its own file hash and the narinfo's file fields describing it. The bound rises to nova-cache 0.10 with the zstandard sublibrary and the pinned index-state advances past the release. (#111)
- **Substitution failures carry a retry class, and every failure path cleans up.** Exceptions thrown mid-stream - a dropped connection, a full disk - previously escaped both the retry budget and the partial-tree cleanup, so the single most common transient failure never retried and could orphan an unverified tree at the store path. Every attempt now converts synchronous exceptions into the pipeline's failure channel (asynchronous cancellation re-throws untouched) and removes the tree on every exit. Failures classify explicitly, matching upstream's transfer layer: transport errors, torn transfers, and truncation retry with backoff; a completed transfer that verifies wrong, a body past its ceiling, or a 4xx fails at once, since retrying a deterministic failure only delays the local-build fallback.
- **The NAR download cap derives from the signed NarSize.** The compressed-body cap was keyed to the narinfo's FileSize, which the Ed25519 fingerprint does not cover, so a rewritten FileSize on a validly-signed narinfo could buy an unbounded download. The cap is now a ceiling computed from the signed NarSize plus a small framing-overhead margin, and the unsigned FileSize may only lower it, never raise it. Caches that omit FileSize (on-the-fly compressors) gain the same margin, so incompressible NARs whose compressed body slightly exceeds NarSize substitute where they previously aborted.
- **An empty Compression field means bzip2, never identity.** Upstream decodes an absent or empty Compression as bzip2, the field's historical default, and nova-cache's parser matches that coercion; treating the empty string as identity diverged from it. The compression vocabulary now lives in one register (Nix.Compression) parsed once at the narinfo boundary, so the streaming and strict dispatch cannot drift.
- **Substitution streams: download, decompress, hash, parse, and unpack in one bounded pass.** The substituter realized every NAR in memory before touching disk - an RSS spike the size of the path being fetched, documented in place since the first cut. The pipeline now drives nova-cache's streaming pieces end to end: the HTTP body feeds the bounded xz source, the incremental hash, the chunk-fed NAR parser, and a new streaming store sink that materializes events as they arrive, so memory is bounded by decoder buffers and the parser's structural-string cap, never by archive or file size. Disk writes therefore begin before the hash can be known - upstream's ordering exactly - and any failure or mismatch removes the tree it wrote; the on-disk recheck streams too. One divergence, documented at the sink: sibling names colliding on a folding filesystem always take upstream's case-hack renaming, never the NTFS true-name path, which can only be enabled on an empty directory the stream cannot pre-scan. (#107)
- **Substitution from cache.nixos.org works: xz NARs decompress.** The default cache serves every NAR xz-compressed, and decompressorFor rejected the value up front, so the shipped configuration could never substitute a single path. nova-cache 0.9's bounded decoder is wired in via its public nova-cache:xz sublibrary - an ordinary solver-visible dependency, no flags on either side; the bound rises to >= 0.9 && < 0.10 and the pinned index-state advances past the release - with output capped at the narinfo's signed NarSize and decoder memory at nova-cache's default, so a hostile stream expands to nothing the narinfo did not promise. Unsupported compression values still reject before any download is paid for. Verified against production data: recorded cache.nixos.org narinfos parse, validate, and signature-verify offline in the suite, and a real path substitutes end to end from the live cache. (#27)
- **nova-cache 0.8.0.0.** The bound rises to >= 0.8 && < 0.9 and the pinned index-state advances past the release. NAR parsing now rejects the full Windows reserved-device set at the parse boundary - COM0/LPT0, space-padded device stems ("NUL .txt"), and the superscript-digit forms - the same names materialization already refused, so the refusal moves earlier and the two guards agree. 0.8 also brings the streaming NAR interface and the NarSize-bounded xz decoder that foreign-cache substitution builds on next.
- **builtin:unpack resolves srcs through the store dir.** The srcs env value carries eval's canonical /nix/store spelling; unpack opened it verbatim, and on Windows a bare /-rooted path resolves against the current drive - the build worked only when the process happened to run from C:. Found by the e2e job's first flights (#100) on GitHub's D:-workspace runners. Each srcs entry now parses as a store path and renders through the configured store dir (the identity on Unix), a non-store-path entry fails loudly, and the e2e job no longer pins its working directory. (#101)
- **Toolchain: GHC 9.14.1 (the first GHC LTS release)** - CI, the release pipeline, and the README badge move from GHC 9.8.4 to 9.14.1. GHC 9.14 opens GHC's new LTS scheme (a minimum of two years of bugfix releases), and under that scheme every earlier series stops receiving fixes, so no version in between had a future. Dependency bounds already admitted the newer compiler and the full set resolves on it. The project targets the LTS alone: base >= 4.22, and foldl' comes from the Prelude, so its eleven redundant Data.List imports are gone. The deprecated pattern namespace specifier stays for now - hlint cannot yet parse the data replacement GHC suggests - with its deprecation warning muted in the cabal file until hlint learns the new spelling.
- **Removed: `pkgs/windows/bootstrap/hello.nix`** - the hand-written proof that the stage-0 MinGW-w64 seed could build something, superseded once `by-name/he/hello` built the same GNU hello through the real stdenv. `pkgs/windows/hello.nix` now points `nova-nix build` at that package instead.
- **The package set is a fixpoint** - `pkgs/windows/lib.nix` adds `callPackageWith` and `packagesFromDirectoryRecursive`, modelled on nixpkgs' `lib.customisation` and `lib.filesystem`. A package now declares its dependencies as formal parameters (`{ stdenv, zlib }: ...`) and callPackage supplies them from the set, instead of each recipe reaching across the tree with a relative import. The bootstrap chain gets a scope layered over the top level, so a link can name the link below it (`hex0` takes `hex0-bootstrap`) without those names reaching the top.
- **`pkgs/windows` follows the nixpkgs layout** - packages moved to `by-name/<shard>/<pname>/package.nix` (shard = the pname's first two characters), the stdenv machinery to `stdenv/`, and the sha256-pinned MSYS2 seeds plus the raw-derivation bootstrap proof to `bootstrap/`. `pkgs/windows/default.nix` discovers `by-name` with `builtins.readDir`, so adding a package is adding a directory. No recipe changed beyond its import paths.

## 0.6.0.0 - 2026-06-12

### Milestone: A Stage-1 stdenv That Builds Real Software

nova-nix now builds real third-party software from source on Windows through a small stage-1 stdenv. A package is `{ name; src; buildInputs; }`, and a `setup.sh` genericBuild drives unpack, configure, build, install, and a fixup that makes outputs standalone - proven on GNU hello, GNU sed, and zlib plus a program that links it.

- **New: a store-pinned MSYS2 userland seed and `mkDerivation`** - the seed bash runs `setup.sh`, which unpacks the source, runs `./configure && make && make install`, and builds through the mingw toolchain. GNU hello and GNU sed build from source with a two-line derivation.
- **New: `buildInputs`** - a package depends on another package. Each input's `include/` goes on `CPPFLAGS`, `lib/` on `LDFLAGS`, and `bin/` on `PATH`, and its derivation builds first. Proven by libgreet (a library built from source) and greeter (a program that links it).
- **New: a `cc-wrapper`** - a `gcc`/`cc` shim installed on the build `PATH` ahead of the real toolchain. It strips ambient header/library search paths from the environment (hermeticity), defaults to `-std=gnu17` so classic gnulib-bearing sources compile under gcc 15+'s C23 default, and adds `-Wl,--no-insert-timestamp` for reproducible PE headers. A package can override the standard with its own `-std`.
- **New: a fixup phase** - Windows has no RPATH, so a distributable binary must carry its non-system DLLs beside it. fixup reads each output binary's PE import table and, for every imported DLL outside the guaranteed Windows ABI, finds it among the declared inputs and bundles it, recursing to a fixpoint. An undeclared non-system DLL fails the build rather than shipping silently broken.
- **New: build-phase overrides** - optional `dontConfigure` / `buildPhase` / `installPhase` attrs let a package build differently from the autotools default (e.g. via a hand-written makefile) while the common case stays a two-line derivation.
- **Real third-party library: zlib 1.3.2** - its `./configure` emits Unix `.so` naming under MSYS2, so zlib builds via its `win32/Makefile.gcc` for a proper `zlib1.dll` and `libz.dll.a` import lib. A demo links zlib (statically against `libz.a`, or dynamically against `zlib1.dll` with the DLL bundled) and round-trips data.
- **Relicensed from BSD-3-Clause to Apache-2.0.** Apache adds an explicit patent grant and trademark terms, and a `NOTICE` file now carries the copyright (Novavero AI Inc.). Earlier releases on Hackage remain under BSD-3-Clause.
- **Substituter: retry transient NAR download failures** before falling back to a local build, matching Nix's `download-attempts` (5 attempts, linear backoff), so a dropped connection or a stale CDN negative no longer forces a rebuild.
- **`builtin:fetchurl` sends a User-Agent**, so hosts that reject empty-UA requests (some GNU mirrors) serve the fetch.
- **Docs:** 100% Haddock coverage across all modules.

## 0.5.0.0 - 2026-06-11

### Milestone: Push and Pull Against a Binary Cache

A native Windows build can now publish its outputs to a binary cache and substitute them back on another machine. The toolchain seed and the `hello` build push to `cache.novavero.ai` with `nova-nix push`, and a fresh store realizes them by signed download instead of rebuilding.

- **New: `nova-nix push`** - uploads store paths and their full reference closure to a binary cache. Each path is NAR-serialized (via `nova-cache`), its NAR uploaded before its narinfo so a client never sees metadata pointing at absent data, and the server signs each narinfo on receipt. Already-cached paths are skipped via one `GET /narinfo-hashes` diff, the recorded NAR hash is cross-checked against the store database before upload, and a signed round-trip is verified at the end. Published narinfos use the canonical `/nix/store` path form. Usage: `nova-nix push --cache URL --key-file KEY (--all | <paths>)`.
- **New: `build --substituter URL --trusted-key K`** - try a binary cache before building. The trusted key (`name:base64`) is required alongside the substituter, since substituting without signature verification would be unsafe; the build falls back to local compilation for any path the cache lacks.
- **New: `--store DIR`** - run a command against an alternate store directory instead of the platform default, so a build or push can target a scratch store.
- **Reproducible `hello`** - `hello.nix` links with `-Wl,--no-insert-timestamp`, and the builder pins `SOURCE_DATE_EPOCH` (1980-01-01) for every build, so determinism-aware tools write fixed timestamps. Two cold bootstraps produce a byte-identical `hello.exe`.
- **Dependencies modernized** - builds against `nova-cache >= 0.4.2` (Ed25519 signing, `normalizeKeyText`), `crypton >= 1.1` with `ram` replacing the deprecated `memory`, and current upper bounds for `containers`, `http-client-tls`, and `time`. No source change beyond the dependency swap; derivation-hash parity is unaffected (CI-verified).

## 0.4.0.0 - 2026-06-10

### Milestone: The First Native Windows Build

`nova-nix build pkgs/windows/hello.nix` compiles and runs a C program through a Nix derivation, natively on Windows: evaluation records the derivation closure, the builder realizes 17 fixed-output fetches and a toolchain unpack in dependency order, and a compiler that is itself a store path produces `C:\nix\store\...-hello\hello.exe`. The first package built natively on Windows by a Nix implementation.

- **New: `builtin:unpack`** - an in-process archive extractor, the counterpart to `builtin:fetchurl`: the unpacker is the thing being bootstrapped, so it cannot be a store tool, and ambient `tar` is unpinned. Extracts `.tar.zst`/`.tar` archives listed in `srcs` into the single `out` output, in order. MSYS2 packages share the `mingw64/` prefix, so a package set merges into one working toolchain root. Symlink and hardlink entries are materialized as copies (store outputs must not require Developer Mode), pacman per-package metadata is skipped, and cross-archive file collisions and path traversal fail the build loudly. MSYS2's zstd frames carry no pledged content size, so decompression is streaming. New dependencies: `tar >= 0.7.1`, `zstd`.
- **New: `pkgs/windows/seed.nix`** - stage 0 of the native Windows stdenv: the 17-package runtime closure of `mingw-w64-x86_64-gcc`, sha256-pinned against `repo.msys2.org`, fetched and merged into a single toolchain root. `pkgs/windows/hello.nix` is the proof build; its recipe also documents the Windows DLL rule (a subprocess resolves DLLs via `PATH`, not RPATH, so dependencies' `bin` dirs go on the build `PATH`).
- **Fix: the build driver materializes eval-coerced source paths** - `src = ./file` produced only the source store path *text* during evaluation (eval performs no store writes - the parity runner's store is not writable), so input validation failed on the missing path. The driver now copies each coerced source into the store, marks it read-only, and registers it with its real NAR hash before building.
- **`data/nix/fetchurl.nix` documentation rewritten as original prose** - the functional content is fixed byte-for-byte by derivation-hash parity and is unchanged (CI-verified).

## 0.3.0.0 - 2026-06-06

### Milestone: Derivation-Hash Parity with Upstream Nix

`(import <nixpkgs> {}).hello.drvPath`, and all 253 derivations in its closure, now hash byte-for-byte identically to `nix-instantiate`, verified in CI on the same nixpkgs tree. A new `eval --aterm` mode dumps a derivation's ATerm for diffing against `nix derivation show`.

- **Fix: indented-string indentation is stripped per literal string-part, before interpolation** - the common indentation was removed from the fully-interpolated string, so a multi-line interpolated value (e.g. `${commonPreHook}` in the stdenv `preHook`) could lower the computed minimum indent to zero and leave the literal lines indented. Indentation is now computed from the literal parts only - interpolated values are opaque content - matching C++ Nix.
- **Fix: path literals interpolated into strings are copied to the store** - `"${./foo}"` left the raw source path in the result; it now copies the file to the store and yields its store path, with the path added to the string context so it lands in the derivation's `inputSrcs`. `builtins.toString` still does not copy, per Nix semantics.
- **Fix: `builtins.placeholder`** - now returns `/` followed by the full SHA-256 of `nix-output:<name>` in Nix base-32, matching Nix's `hashPlaceholder`. It previously truncated the hash and wrapped it as a `/nix/store` path, so any derivation using `${placeholder "out"}` (e.g. perl's `configureFlags`) diverged.
- **Fix: a derivation's default output is its first declared output** - a bare reference to a multi-output derivation now resolves to `outputs[0]` (both name and path), not a hardcoded `out`. This affected packages whose first output is not `out`, such as `xz` (first output `bin`).
- **Fix: `builtins.attrNames` is sorted lexicographically** - names were returned in interned-symbol order rather than sorted by key, and inconsistently with `builtins.attrValues`. Surfaced in `fetchurl`'s `impureEnvVars` (the generated `NIX_MIRRORS_*` list).

## 0.2.0.0 - 2026-06-05

### Milestone: `import <nixpkgs> {}` Evaluates

- **`import <nixpkgs> {}` now evaluates to WHNF** - the top-level package set resolves and enumerates ~23,000 attributes, and a package evaluates through the full stdenv bootstrap down to a derivation: `(import <nixpkgs> {}).hello.drvPath` yields a `/nix/store/...-hello-2.12.1.drv` path. (Derivation-hash parity with upstream Nix and on-Windows building are the next fronts - this milestone is evaluation, not yet building.)
- **Fix: `inherit <name>;` de Bruijn level in positional let/rec blocks** - `inherit x;` (without `from`) in a positional `let`/`rec` block desugars to `x = x;` with the right-hand side resolved against the *outer* scope, so it refers to the enclosing `x` rather than the binding being defined. But the desugared thunk is evaluated at runtime in the *inner* (let/rec) env, which adds one parent-chain level the outer resolution did not count - so every resolved level was short by one. This produced `nn_env_lookup_resolved: idx out of bounds` whenever an outer *lexical* variable was inherited inside a positional block - first triggered by perl's `inherit version;` (argument-set slot 7) nested in a 4-binding `let`. Fixed by shifting the desugared `inherit` RHS up one de Bruijn level in `Nix.Expr.Resolve`. (`inherit (from) ...` was already correct: its RHS resolves against the inner scope.)
- **Fix: Hackage build - ship C headers in the sdist** - the `cbits/*.h` headers are `#include`d by the C sources (via `include-dirs: cbits`) but were never listed in the cabal file, so `cabal sdist` omitted them and the Hackage build failed with `nn_*.h: No such file or directory`. Added `extra-source-files: cbits/*.h`. A regression introduced in 0.1.9.0 (the first release to ship the C data layer); CI never caught it because CI builds the working tree, not the sdist. Verified by building from a freshly-extracted sdist tarball in isolation.

## 0.1.9.0 - 2026-06-05

### nixpkgs Evaluation: Lazy Derivations

- **Fix: `import <nixpkgs> {}` infinite recursion - lazy `derivation` over `derivationStrict`** - `builtins.derivation` was eager: forcing a derivation to WHNF forced its entire env/input closure. This turned nixpkgs' lazy `perl` and `libxcrypt` dependency cycle into a blackhole - `perl`'s `assert (libxcrypt != null)` forced `libxcrypt`'s build, which forced `perl` mid-construction. Restored Nix's two-tier model: the eager `builtins.derivationStrict` primop (identical content-hashing, renamed from the old `builtins.derivation` body) plus a lazy `derivation` wrapper over it (mirroring `corepkgs/derivation.nix`). Forcing a derivation to WHNF now yields its attribute spine without computing `drvPath`/`outPath` or forcing its inputs, so referencing a package no longer builds its closure. No C changes; `drvPath`/`outPath` hashes unchanged. The stdenv bootstrap now progresses through derivation construction instead of looping (next blocker: a variable-slot bug while evaluating perl's `@`-pattern).

### Technical Audit + C Data Layer Polish

- **Fix: Nix integer division semantics** - `builtins.div` and `builtins.mod` now use floored division (`div`/`mod`) instead of truncated (`quot`/`rem`). `-7 / 3` correctly returns `-3` (was `-2`). Affects `builtinDiv`, `builtinMod`, and `evalDiv`.
- **Fix: `inherit (from)` in positional let/rec blocks** - The resolution pass treated `inherit (expr) x y;` as positional, but the bytecode evaluator did not handle `BcInheritFrom` in `allBcPositional`, `bcBindingSlotCount`, `buildBcSlotThunks`, or `buildBcAttrMapFromSlots`. This caused env slot count mismatches and `idx out of bounds` assertions on any nixpkgs expression using `inherit (from)` in a positional let block.
- **C memory safety audit** - `nn_bytecode.c`: `ensure_op_space`/`ensure_data_space` now return error codes instead of failing silently; overflow guard on capacity doubling. `nn_lambda.c`: `NN_ASSERT` bounds checks on entry accessors prevent null dereference when `formal_count == 0`. `nn_attrset.c`: documented partial-realloc-failure behavior.
- **Dead code removal** - Removed unused `cattrsetIntersect` Haskell wrapper, `nn_attrset_intersect`, and `nn_attrset_values_ptr` C functions.
- **New: `nn_assert.h`** - Debug-mode bounds checking macro. Compiles to nothing under `NDEBUG`.
- **Performance** - Stress test: 6.25 MB max residency, 56.3% GC productivity (down from 69.7 MB / 1.6% pre-C-data-layer).

## 0.1.8.0 - 2026-03-08

### Builder Correctness, Windows Store Paths, Hackage Fix

- **Fix #1: builder no longer pre-creates output paths** - `buildDerivationInner` no longer calls `createDirectoryIfMissing` for `$out`, `$dev`, etc. before running the builder. The builder is now responsible for creating its own outputs, matching real Nix semantics. Builds fail with `"builder succeeded but outputs missing"` if the builder exits successfully but expected outputs don't exist.
- **File outputs supported** - `$out` can now be a regular file, not just a directory. `registerSingleOutput`, `addToStore`, `scanReferences`, and `pathExists` all use `doesPathExist` instead of `doesDirectoryExist`. `moveOutput` (renamed from `moveDirectory`) handles both files and directories in cross-device fallback. `collectRegularFiles` accepts file paths directly for reference scanning.
- **Fix #2: Windows store paths use native separators** - CLI now uses `platformStoreDir` (`C:\nix\store` on Windows, `/nix/store` on Unix) for filesystem operations and display. Evaluator internals keep canonical `/nix/store` for ATerm hash compatibility.
- **Fix: `crypton < 1.1` in .cabal file** - Previous `crypton` pin was only in `cabal.project` (which Hackage ignores). Moved to `.cabal` so the Hackage solver respects it. `crypton >= 1.1` switched from `memory` to `ram` for `ByteArrayAccess`, breaking `http-client-tls`.

## 0.1.7.1 - 2026-03-07

### Hackage Build Fix

- **Fix: `nova-cache < 0.3.1` bound** - Hackage ignores `cabal.project` constraints, so the solver picked `nova-cache-0.3.1.0` (which uses `ram`) alongside `http-client-tls` (which uses `memory`), causing `ByteArrayAccess` instance mismatches. Narrowed bound to `< 0.3.1` to force `nova-cache-0.3.0.0` (which uses `memory`) until `http-client-tls` migrates to `ram`.

## 0.1.7.0 - 2026-03-07

### Build Fix: drvPath Resolution + cmd.exe Quoting

- **Fix: `nova-nix build` drvPath resolution** - `builtinDerivation` now writes `drvPath` and output paths (`out`, `dev`, etc.) into `drvEnv` on the `Derivation` struct. Previously these were only present in the eval result attr set, so `buildAndRegister` could not find the `.drv` store path for dependency resolution (error: "no drvPath available for dependency resolution").
- **Fix: `extractDerivation` returns `StorePath`** - The CLI `build` command now extracts the `drvPath` `StorePath` directly from the eval result alongside the `Derivation`, passing it explicitly to `buildWithDeps`. Eliminates the fragile `Map.lookup "drvPath" drvEnv` fallback.
- **Fix: cmd.exe builder quoting on Windows** - New `mkBuilderProcess` detects when the builder is `cmd.exe` with `/c` and uses `Proc.shell` instead of `Proc.proc`. GHC's `proc` wraps each argument in double-quotes for `CommandLineToArgvW`, but cmd.exe doesn't use that convention - `args = [ "/c" "echo Hello" ]` would fail because cmd.exe tried to find an executable literally named `"echo Hello"`.
- **Fix: UTF-16 auto-detection** - New `readFileAutoEncoding` detects UTF-16 LE/BE and UTF-8 BOM at the byte level before decoding. PowerShell's `>` operator writes UTF-16 LE by default - a Windows-first Nix implementation must handle this at the input boundary. Wired into all 5 file-read sites (Parser, Eval/IO, Main).
- **nova-cache `>= 0.3.0`** - Bumped lower bound to track nova-cache 0.3.x series.
- **`crypton < 1.1` pin** - `http-client-tls` still uses `memory`'s `ByteArrayAccess`; `crypton >= 1.1` switched to `ram`, causing instance mismatches. Pinned in `cabal.project` until `http-client-tls` migrates.

## 0.1.6.0 - 2026-02-26

### De Bruijn-Style Positional Env + SmallArray Slots

- **De Bruijn-style variable resolution** - New `Nix.Expr.Resolve` pass replaces `EVar` with `EResolvedVar level index` for lambda-bound variables. Called at parse time; all subsequent evaluation uses positional indices instead of name-based `Map` lookups.
- **Array-based Env** - `envSlots :: SmallArray Thunk` replaces `envBindings :: Map Text Thunk`. Lambda formals get O(1) positional indexing via `indexSmallArray` instead of O(log n) `Map.lookup`. Let/rec bindings and builtins remain in name-based `envLazyScope` (already zero Map.Bin overhead).
- **Scope chain** - `Env` uses parent pointer chain instead of `Map.union`. Variable lookup walks the chain: positional slots, then `envLazyScope`, then parent, then with-scopes. Avoids O(n) `Map.union` when extending large envs.
- **Inherit desugaring** - `inherit x;` is desugared to `x = x;` in the resolution pass so inherited lambda formals resolve to `EResolvedVar` (lambda slots have no names at runtime).
- **Heap savings** - Eliminates 29.9M Map.Bin nodes (1.37 GB) from lambda formals. Replaced by SmallArray (one heap object per env, O(1) index) + scope chain parent pointers.
- `primitive` dependency added for `Data.Primitive.SmallArray`

## 0.1.5.0 - 2026-02-26

### New Builtins, Coercion Fixes, CI Cleanup

- **`builtins.warn`** - prints warning to stderr, returns second arg
- **`builtins.path`** - copies file/dir to store with `SCPlain` context
- **`builtins.seq` fix** - now forces first arg to WHNF (was silent no-op)
- **`builtins.trace`/`traceVerbose` fix** - print to stderr via `MonadEval.traceMessage`
- **`coerceToString` fix** - handle `VAttrs` with `__toString` and `outPath`, enabling `"${pkgs.hello}/bin/hello"` interpolation (was type error on all attrsets)
- **`fetchTarball` fix** - downloads and extracts via curl|tar pipeline (was returning raw .tar.gz)
- **`MonadEval`** - added `traceMessage`, `copyPathToStore` methods
- CI: switched to `haskell-actions/run-ormolu@v16` (matching all Novavero repos)

## 0.1.4.0 - 2026-02-26

### Memory Optimization + Lazy Non-Rec Attrsets

- **Lazy non-rec attrsets** - `evalNonRecAttrs` now uses `LazyAttrs` to defer thunk+IORef allocation until first access. For `all-packages.nix` (~15k entries per stdenv stage), only accessed packages allocate thunks.
- **Per-binding Env in `LazyBinding`** - Each `LazyExpr`/`LazyInherit`/`LazyInheritFrom` carries its own `Env` instead of sharing one per-attrset. Fixes a bug where `//` merges of `LazyAttrs` from different scopes lost variables - broke `lib.extends` overlay pattern used by `makeExtensibleWithCustomName`.
- **Batched formal set matching** - `matchFormalSet` creates ONE `Env` with all formals instead of N singleton `Env`s. Uses knot-tying so default expressions can reference other formals, including forward references (`{ a ? b, b ? 1 }: a` evaluates to `1`).
- **Env scope chain** - `Env` now uses a parent pointer chain instead of `Map.union`. Variable lookup walks the chain: local bindings, then parent, then with-scopes. Avoids O(n) `Map.union` when extending large envs (e.g. 30k-entry nixpkgs rec set). Peak Map.Bin heap: 956MB down to ~40MB.
- **ThunkCell release** - On force, `Pending expr env` is overwritten to `Computed val`, dropping all references to `Expr` and `Env` (matching C++ Nix in-place mutation). Previously retained dead closures indefinitely.
- **Lazy `//` operator** - `mergeAttrSets` preserves `LazyAttrs` when possible, merging binding recipe maps instead of materializing all thunks.
- **Lazy with-scopes** - `pushWithScope` accepts `AttrSet` directly so `LazyAttrs` with-scopes stay lazy. `lookupWithScopes` uses `attrSetLookup` to materialize only the accessed key.
- **Key-driven `intersectAttrs`** - Only touches keys present in both sets instead of materializing entire attrsets.
- **New builtins** - `min`, `max`, `mod`, base64 `encode`/`decode` (via nova-cache 0.2.4)
- `nova-cache >= 0.2.4` dependency bump
- 511 tests, -Werror clean, ormolu 0.7.7.0 clean, hlint clean

## 0.1.3.0 - 2026-02-25

### nixpkgs Compatibility: Module System, Callable Sets, Parser Fixes

- **`__functor` support** - Attribute sets with `__functor` are now callable, matching real Nix. `(set.__functor set) arg` dispatch enables nixpkgs patterns like `lib.makeOverridable` and `lib.setFunctionArgs`.
- **`builtins.setFunctionArgs`** - Wraps a function in a callable attrset with `__functor` and `__functionArgs` metadata. Used by `lib.mirrorFunctionArgs`, `lib.makeOverridable`, and the entire nixpkgs override system.
- **Null dynamic keys** - `{ ${null} = val; }` now skips the binding instead of erroring (matching real Nix). Used extensively by the nixpkgs module system for conditional attributes like `${if cond then "key" else null}`.
- **Indented string interpolation fix** - `''${expr}''` now parses correctly. The lexer had an overzealous guard (`isIndStringEscape`) that blocked `''` from opening an indented string when followed by `$`. This broke `lib.generators` and many nixpkgs files.
- **`splitVersion` fix** - Dots are separators, not components. `splitVersion "1.2.3"` now returns `["1" "2" "3"]` (was `["1" "." "2" "." "3"]`). Fixes `lib.versions.majorMinor` and all version comparison logic.
- **`builtins.functionArgs` on callable sets** - Now inspects `__functionArgs` metadata on attrsets produced by `setFunctionArgs`, enabling `lib.functionArgs` to work on overridable functions.
- **CLI eval sub-parser** - `--strict` and `--nix-path` flags now work after `eval` command (e.g. `nova-nix eval --strict --expr '...'`). Previously only worked before the command.
- **Bundled `<nix/fetchurl.nix>`** - Ships as a Cabal data-file, added to search paths automatically. nixpkgs stdenv bootstrap imports this file; no system Nix install needed.
- **nixpkgs module system working** - `lib.evalModules`, `lib.mkOption`, `lib.mkIf`, `lib.types.*` all evaluate correctly.
- **nixpkgs functional patterns working** - `lib.makeOverridable`, `lib.makeExtensible`, `lib.fix`, `lib.generators.toKeyValue` all correct.
- **`lib.systems.elaborate` instant** - Was taking minutes before lazy builtins; now returns immediately for any system string.
- 101 builtins (up from 91), 511 tests

## 0.1.2.0 - 2026-02-24

### Lazy Builtins + Correctness Fixes

- **Lazy `map`** - returns deferred thunks instead of eagerly forcing all elements. `map f [80000 items]` is now O(1) until elements are demanded.
- **Lazy `genList`** - each element is a deferred `f(i)` thunk, forced only on demand
- **Lazy `mapAttrs`** - each attr value is a deferred `f key val` thunk. Critical for nixpkgs which `mapAttrs` over the entire package set.
- **Semi-lazy `concatMap`** - forces applications to discover list structure for concatenation, but element thunks within sub-lists stay lazy
- **`@`-pattern scoping fix** - `{ system ? args.system or ..., ... }@args:` now correctly binds the `@`-name before evaluating defaults, matching real Nix. Previously, default expressions couldn't reference the `@`-binding.
- **Synthetic thunk memo cell fix** - `mkSyntheticThunk` uses env bindings (not expr) for IORef uniqueness, preventing GHC's full-laziness transform from sharing one memo cell across all deferred thunks. Without this, `map f [a b c]` would return `[f(a) f(a) f(a)]`.
- `deferApply` helper: builds thunks wrapping `EApp (EResolvedVar 0 0) (EResolvedVar 0 1)` in a self-contained env with positional slots, reusing existing eval + memoization machinery
- 511 tests (3 new: map laziness, genList laziness, mapAttrs laziness)

## 0.1.1.0 - 2026-02-24

- **Thunk memoization** - Per-thunk `IORef` memo cells (matching real Nix in-place mutation). `forceThunk` is a `MonadEval` method: IO evaluators memoize per-allocation, pure evaluators re-evaluate. GC reclaims dead thunks naturally - no unbounded global cache. `unsafePerformIO` CAF float-out prevented via `NOINLINE` + `seq` pattern.
- **8.4x builtin dispatch speedup** - Case dispatch replacing polymorphic `Map` reconstruction on every call. Direct pattern match on builtin name for zero-allocation dispatch in the hot path.
- **Regex builtins** - `builtins.match` and `builtins.split` via `regex-tdfa` (pure Haskell POSIX ERE, cross-platform)
- `ESearchPath !Text` AST constructor - `<nixpkgs>` is now its own node, desugared at eval time to `builtins.findFile builtins.nixPath "nixpkgs"` (matching real Nix semantics)
- NIX_PATH parsing: `parseNixPath` converts colon-separated `name=path` entries into `{ prefix, path }` attrset thunks
- `builtinEnv` now accepts search paths: `builtinEnv :: Integer -> [Thunk] -> Env`
- `EvalState.esSearchPaths` populated from `NIX_PATH` environment variable at startup
- Directory imports: `import ./dir` automatically resolves to `./dir/default.nix`
- Dynamic attribute keys: `{ ${expr} = val; }` fully supported in all contexts (non-rec, rec, let, select, hasAttr)
- Two-phase binding resolution for knot-tying: `resolveBindingKeys` (monadic, evaluates dynamic keys) then `buildResolvedBindingsMap` (pure, enables recursive self-reference)
- Monadic `resolveKey` replaces pure `resolveStaticKey` - handles both `StaticKey` and `DynamicKey` uniformly
- CLI `--nix-path NAME=PATH` flag (repeatable, merged with NIX_PATH)
- CLI `--expr EXPR` for inline expression evaluation
- CLI deep-force and pretty-printing for eval output
- `README.md` added to `extra-doc-files` in cabal (shows on Hackage)
- Parser fix: `TokInterpOpen` in expression context expects `TokRBrace` (not `TokInterpClose`) for closing brace
- 91 builtins (up from 88)
- 508 tests (14 new: parseNixPath, search path parsing/eval, dynamic keys, directory import, populated search path resolution)

## 0.1.0.0 - 2026-02-24

### Cross-Platform Fixes

- Cross-platform ATerm serialization: `storePathToText` always uses `/` for store paths regardless of OS, `parseStorePath` accepts both `/` and `\`
- Builder inherits system environment, overlays build env via `Map.union` - fixes silent process failures on Windows (missing `SYSTEMROOT`)
- Builder PATH derived from builder location (`buildPath`) - includes builder dir and MSYS2 sibling `usr/bin` for coreutils discovery
- `findTestShell` prefers known Git for Windows bash over WSL launcher (`System32\bash.exe`)
- Parser strips UTF-8 BOM - Windows editors (Notepad, PowerShell) commonly add byte order marks
- Demo `test.nix` included: derivations, lambdas, builtins.map, arithmetic - runs on all platforms
- 16 builtins exposed at top level without `builtins.` prefix (`toString`, `map`, `throw`, `import`, `derivation`, `abort`, `baseNameOf`, `dirOf`, `isNull`, `removeAttrs`, `placeholder`, `scopedImport`, `fetchTarball`, `fetchGit`, `fetchurl`, `toFile`) - matches real Nix language spec, required for nixpkgs compatibility

### String Contexts, Dependency Resolution, Binary Substituter

- String context tracking on all `VStr` values: `SCPlain`, `SCDrvOutput`, `SCAllOutputs`
- Context propagation through interpolation, string concatenation, `replaceStrings`, `substring`, `concatStringsSep`, and all string builtins
- `Nix.Eval.Context` module: pure helpers for context construction, queries, and extraction
- `derivation` builtin now collects string contexts into `drvInputDrvs` and `drvInputSrcs`
- New builtins: `hasContext`, `getContext`, `appendContext`
- `Nix.DependencyGraph`: BFS graph construction with `Data.Sequence` (O(V+E)), topological sort via Kahn's algorithm, cycle detection
- `Nix.Substituter`: full HTTP binary cache protocol - narinfo fetch/parse, Ed25519 signature verification, NAR download/decompress/unpack, store DB registration, priority-ordered multi-cache
- `Nix.Builder.buildWithDeps`: recursive dependency resolution - topo sort, cache check, binary substitution, local build fallback
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
- MonadEval typeclass - evaluator is polymorphic in its effect monad (PureEval for tests, EvalIO for real evaluation)
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

- Total functions only - no `read`, `head`, `tail`, `!!`, `fromJust`, or `Map.!`
- Argument injection prevention in fetch builtins (`--` separator before user URLs)
- Path traversal validation in writeToStore (rejects `/`, `..`, null bytes)
- Content-hashed temp directories for fetchGit (no predictable paths)
- Store paths set read-only after registration (immutability enforcement)

### Architecture

- Store paths parameterized via `StoreDir` - no hardcoded `/nix/store/` strings
- Cross-device safe directory moves (rename with copy+remove fallback)
- Hash utilities deduplicated into Nix.Hash (single source of truth)
- currentSystem is a constant (not a function), matching real Nix semantics
- Platform-aware environment setup (HOME vs USERPROFILE, Unix vs Windows PATH)
