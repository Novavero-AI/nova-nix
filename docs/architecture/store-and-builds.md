# The store and builds

The store is a directory of immutable paths and a database of which of
them are valid. A build realises a derivation into it: it downloads each
output a binary cache holds, and builds the rest.

## Layout

A store is one directory: `/nix/store` on Linux and macOS, `C:\nix\store`
on Windows, or the directory `--store` names. Each path in it is
`<hash>-<name>`, a file, a directory or a symlink that does not change once
it is registered. The store keeps its own state inside the directory:

```text
<store>/
  <hash>-<name>              a store path
  <hash>-<name>.lock         its lock, once a build or download has taken it
  .nova-nix/
    db.sqlite                the database
    gc.lock                  the store-wide lock
    gcroots/                 the garbage collector's roots
      auto/                  a record per out-link
    builds/
      <hash>                 a build's directory
      <hash>.chroot          a sandboxed build's root
```

[`openStore`][handle] opens a store under one canonical spelling of its
directory, and each database row names its path under that spelling. A
store is therefore used at the directory it was made in: opened anywhere
else it holds no valid path, and the collector refuses to run over rows
that lie outside the directory it opened ([`src/Nix/Store/GC.hs`]).

A store whose directory upstream Nix's database also manages is refused
by each operation that could replace or delete a path in it
([`src/Nix/Store/Upstream.hs`]): nova-nix has no rows for upstream's paths
and would take them for debris.

## The database

[`src/Nix/Store/DB.hs`] keeps three tables, named and shaped after
upstream's: `ValidPaths`, a row per registered path with its NAR hash and
size, its deriver, its registration time, whether this store built it,
its signatures and the content address its name was made from; `Refs`, a
row per reference from one valid path to another; and
`DerivationOutputs`, the outputs of each valid derivation. A path is valid
when it has a row. The database runs in WAL mode with foreign keys on, and a connection
waits up to an hour for another's lock, as upstream's does. A read-only
evaluation opens it read-only and writes no row.

A tree is complete on disk, with canonical metadata, before its row is
written, and a delete removes the row before the tree. A tree with no row
is debris the collector removes; a row with no tree would be believed.

Canonical metadata is upstream's ([`src/Nix/Store/Metadata.hs`]): on
POSIX each file and directory is mode `0444`, or `0555` when its owner may
execute it, and every node's modification time is one second past the
epoch. On Windows each file and directory gets the read-only attribute
and the same time.

## Store paths

A store path's hash is fixed before anything is built, by what the path
will hold or how it will be made ([`src/Nix/Hash.hs`], upstream's
`makeStorePath`):

```text
sha256("<type>:sha256:<hex of digest>:/nix/store:<name>")
```

compressed to 20 bytes and written in Nix's base-32 as the 32 characters
before the name. The store directory in that string is `/nix/store`
whatever directory the store is in and on every platform, so a derivation
has the same paths everywhere and cache.nixos.org's paths are nova-nix's.
Expressions see `/nix/store/...` too (`builtins.storeDir` is `/nix/store`).
The store's real directory is spelled only where a path meets the
machine: a file opened, a builder spawned, a database row, a path
printed.

The type says what the digest is of:

- `text`, followed by each reference, for a `.drv` file and a
  `builtins.toFile` result: the digest is of the contents.
- `source`, followed by each reference, for a tree added by its NAR's
  SHA-256, as a copied source is.
- `output:<name>` for a derivation's output: the digest is of the
  derivation, as below.

A fixed-output derivation's output is a `source` path of its hash when
the hash is a recursive SHA-256, and otherwise the `output:out` path of
the digest of `fixed:out:<method><algorithm>:<hex>:`. An input-addressed
derivation's outputs use upstream's `hashDerivationModulo`: the
derivation's rendering with every output's path and hash blanked and each
input derivation replaced by that input's own hash
([`inputAddressed`][derivation]). Its `.drv` file is the `text` path of its
rendering, referring to its input sources and input derivations.

## From derivation to build

Evaluation renders each `.drv` and records its bytes. Before it builds,
`build` writes the whole recorded closure to the store and registers it
([`writeDrvClosure`][store]), so what is built is what the paths were
computed from. The build reads the graph back from the `.drv` files in the
store ([`src/Nix/DependencyGraph.hs`]), refuses a cycle, and
[`src/Nix/Realise.hs`] decides what to do with each derivation, in the
order upstream's goals take them:

- A derivation's outputs are tried before its inputs, so a package a
  cache holds never pulls in what it was built from.
- A substituted path's references, as the cache that serves it declares
  them, arrive first, so the store holds the closure of every path at
  every step.
- What no cache holds is built, its inputs first. A cache that lacks a
  path is told apart from one that fails, and a failure stops the build
  unless `fallback` is on. A derivation whose `allowSubstitutes` is false
  is built even when a cache holds it, unless `always-allow-substitutes`
  is on.

Every effect is a field of a record ([`RealiseEffects`][realise]), so the
decisions run in tests over a fake cache and store, and
[`src/Nix/Builder.hs`] supplies the real ones. Under `--check`, the
requested derivation is built again beside its valid outputs and compared
with them, and the rebuild registers nothing.

## Substitution and trust

The configured caches are asked in turn for each path
([`src/Nix/Substituter.hs`]):

1. `<hash>.narinfo` is fetched and its fields checked. It is accepted when
   any trusted public key verifies one of its signatures; as in upstream,
   a key is not bound to a cache. The references it declares are realised
   first.
2. The path's lock is taken ([`src/Nix/Store/Lock.hs`]) and its validity
   checked again: a path another process finished meanwhile is adopted.
3. The NAR the narinfo names is downloaded from the same cache and, in
   one bounded streaming pass, decompressed (xz, zstd, bzip2 or none),
   hashed and unpacked into the path. The NAR's hash and size must match
   the narinfo's.
4. Its metadata is made canonical, the tree on disk is serialised and
   checked again, and the path is registered with the narinfo's
   references, its lock held until the row is committed.

The signature vouches for the narinfo; the NAR hash binds the bytes to it.
Each cache is asked about each path once a run. The NAR format, narinfos
and signatures come from [nova-cache], the library nova-nix shares with
its cache server.

`nova-nix trust` answers a different question: what a closure relies on.
It lists every fixed-output derivation in the closure, which of them run
as builders, and which hold prebuilt code
([`src/Nix/Trust.hs`]).

## Building a derivation

[`buildOutputs`][builder] builds one derivation whose inputs are valid:

1. It takes the lock of every output, in one fixed order so that two
   processes cannot deadlock, and checks again whether every output is
   valid, adopting them if another process built them meanwhile.
2. It checks the inputs and takes their closure: what an output may refer
   to, besides its sibling outputs.
3. It makes the build directory, `.nova-nix/builds/<hash>` after the first
   output's hash, with a call that fails if anything already has the name,
   open to the user nova-nix runs as alone ([`src/Nix/Builder/BuildDir.hs`]).
   What an earlier build that was killed left at the name is removed
   first, or moved aside where it cannot be removed.
4. It runs the builder, which writes each output at its own store path, in
   its root when sandboxed. An output that is already valid is written to
   a scratch path instead, and discarded.
5. It scans each output for the hash of every path in the input closure
   and of every sibling output, orders the outputs so that each follows
   those it refers to (a cycle fails the build), moves each into the
   store if it was written elsewhere, makes its metadata canonical,
   checks a fixed output against its declared hash, and registers all of
   them in one transaction.

A failure or an interrupt before registration removes what the builder
wrote, and the build directory is removed however the build ends.

`builtin:fetchurl`, `builtin:unpack` and `builtin:buildenv` run in
nova-nix's own process; running the first two in the sandbox, as upstream
runs its builtins, is [#682]. Any other builder is a program, spawned with:

- the derivation's environment, its store paths rewritten to the store's
  real directory where that is not `/nix/store`, and its output
  placeholders replaced by the output paths;
- `NIX_BUILD_TOP`, `TMPDIR`, `TEMPDIR`, `TMP` and `TEMP` naming the build
  directory, `HOME` (on Windows `USERPROFILE`) naming it too outside the
  sandbox, `NIX_STORE`, `NIX_BUILD_CORES` from `cores`,
  `SOURCE_DATE_EPOCH`, and `PATH`, which on Linux and macOS is upstream's
  `/path-not-set` and on Windows is [its own](windows.md#how-a-build-runs),
  each unless the derivation sets it;
- nothing from nova-nix's own environment, but the variables a
  fixed-output derivation's `impureEnvVars` names, and on Windows the few
  the system needs ([Windows](windows.md#how-a-build-runs)).

The builder's standard output and standard error are one pipe, copied to
nova-nix's standard error a line at a time ([`src/Nix/Builder/Log.hs`]). A
derivation for a system this machine cannot run is refused unless
`--exec-wrapper` names a launcher for it; an `i686` derivation runs
directly on `x86_64` Linux and Windows. A build in a store away from its
platform's directory writes that directory into its output, so its bytes
depend on where the store is. Upstream builds in such a store only inside
its Linux sandbox, which shows the store at its logical directory, and
refuses elsewhere; refusing such a build is [#432].

## The sandbox

`sandbox` is on by default on Linux and off elsewhere, as in upstream.

On Linux a sandboxed build runs in new user, mount, PID, IPC and UTS
namespaces, and in a network namespace holding only a loopback interface
unless its derivation is fixed-output. Its root holds the closure of its
inputs, the paths `sandbox-paths` binds and its build directory at
`sandbox-build-dir`, and the builder writes its outputs at their store
paths inside it. It runs as uid 1000 and gid 100 in its user namespace,
which map to the invoking user or to a build user, with `HOME` at
`/homeless-shelter`, which does not exist.

On macOS a sandboxed build runs under a profile of macOS's sandbox. It
sees its build directory and its outputs where they are on disk, with
`HOME` at `/homeless-shelter`. It may read and run its inputs and the
paths `sandbox-paths` names, write only its outputs and its build
directory, and reach the network only when its derivation is
fixed-output. There `sandbox-paths` defaults to upstream's system
frameworks, libraries and shells. The builder runs as the invoking user,
or as a build user, so the profile grants nothing that would reach that
user's own files, sockets, terminals or shared memory, and when the build
ends the helper stops and kills every process of the builder's user that
carries the build's mark, a name only that build's profile allows,
including one that left the builder's process group, and reports the
build as it ended only once a listing of that user's processes finds
none of them left. The builder writes its output to a pipe the helper
copies into the build log. A build run as the invoking user can still
read the command lines of the user's other processes, and any build can
create System V IPC objects that outlive it, which upstream removes for a
build user.

The work is split three ways:

- [`src/Nix/Builder/Sandbox.hs`] decides, in pure functions, whether a
  build is sandboxed and what its plan is, and on macOS renders its
  profile.
- [`src/Nix/Builder/Sandbox/Spawn.hs`] probes whether the system can make
  the sandbox, makes the Linux root's skeleton beside the build
  directory, looks at the paths a macOS profile names, and runs the
  helper.
- The helper, `nova-nix-sandbox` ([`sandbox/`]), is a C program started
  with an empty environment. It reads the plan on its standard input and
  executes the builder: on Linux it first makes the namespaces, the
  mounts and the root, and on macOS it applies the profile. That work
  falls just before the builder's exec, where a child of GHC's threaded
  runtime may make only async-signal-safe calls and the process library
  offers no hook. A step that fails is reported as one record on the
  helper's standard output. Its standard input stays open after the plan
  as a lifeline: when nova-nix closes it, the builder is killed.

nova-nix finds the helper beside its executable, or in Cabal's install
directory. The Linux release archive also carries a shell,
`libexec/nova-nix/sh`, which `sandbox-paths` binds as `/bin/sh` by
default. A Linux system that cannot make the namespaces builds
unsandboxed after a warning, or fails the build with `sandbox-fallback`
off; on macOS a sandbox that cannot be made fails the build, as
upstream's does. A sandboxed build as root with `build-users-group`
empty is refused, whatever `sandbox-fallback` says.

## Build users

nova-nix running as root on Linux or macOS builds as the members of
`build-users-group`, `nixbld` by default, as upstream does
([`src/Nix/Builder/BuildUser.hs`]); anyone else builds as themselves.

- **The pool.** Each build takes the first member whose lock is free in
  `/nix/var/nix/userpool` (or `$NIX_STATE_DIR/userpool`), the pool
  upstream's daemon locks too, and waits while every member is taken. A
  group that is missing or empty, a member that names no user, and a
  member that is the user nova-nix runs as fail the build in upstream's
  words.
- **The build.** Every process of the member is killed before the build
  and once its builder ends. The store directory is root's and the
  group's with mode 1775, the directory the builder works in and the
  files nova-nix writes there are the member's, and the directory holding
  builds must not be below one every user may write. In the Linux sandbox
  the namespace's uid 1000 and gid 100 map to the member and the group;
  on macOS and outside the sandbox the builder takes the member's uid,
  the group and, on Linux, the member's supplementary groups, set between
  the fork and the exec.
- **The outputs.** An output whose root is not the member's, or that
  holds a node another user owns, fails the build. A passing output is
  made root's and read-only.

Differences from upstream's Linux sandbox are open issues, among them its
seccomp filter ([#678]) and the personality it sets for the derivation's
system ([#712]). Windows builds run in a job object and nothing more
([Windows](windows.md#how-a-build-runs)).

## Garbage collection and roots

A root keeps a store path and everything it refers to. Roots live in
`.nova-nix/gcroots/` ([`src/Nix/Store/GC.hs`]):

- **Out-links.** `build` links its output from `./result`, or the path
  `--out-link` gives, and a profile links each of its generations the same
  way. The bytes of the link's absolute path are recorded in a file in
  `gcroots/auto/` named by their SHA-256. A record whose link is gone is
  removed by the next walk, and one whose link no longer points into the
  store roots nothing.
- **Permanent roots** are the operator's: anywhere else under `gcroots/`,
  a symlink into the store roots the path it points into, and a regular
  file named `<hash>-<name>` roots that path. Nothing in nova-nix makes
  one.
- **Upstream's roots.** For a store at `/nix/store`, or `<root>/nix/store`,
  the roots under upstream's `gcroots/` and `profiles/` in
  `/nix/var/nix` (or `<root>/nix/var/nix`) count too, read the same way:
  upstream's clients keep their roots and profiles there.

The collector walks the references in the database from the roots. It
removes the rows of every other valid path in one transaction, then every
entry of the store directory that is neither live nor `.nova-nix`: dead
paths, trees left with no row, and dead paths' lock files. `store delete`
checks the paths it is given against the same walk and refuses a live
one.

Every open store handle holds `gc.lock` shared ([`src/Nix/Store/Handle.hs`]),
and the collector takes it exclusively, so a collection runs only while
no other process has the store open, and a process that opens the store
during one waits for it. A path's own lock covers it while it is being
produced, from before anything is deleted or written until its row is
committed; the store-wide lock covers the rest, such as the time between
an input's registration and the build that uses it.

Upstream's runtime roots, which keep what running programs have open, are
[#341]. Its `keep-derivations`, which keeps a live output's `.drv`, is
[#635], and until then a `.drv` stays only while something refers to it.
Nothing collects on its own ([#476]).

## Profiles, develop and the daemon

Profiles and `develop` are built on the store and builder above. The
daemon is to serve them to users who cannot write the store, and some of
its pieces are in the library.

- **Profiles.** `profile add`, `profile remove` and `profile list` keep a
  profile of generations as upstream's `nix profile` does
  ([`src/Nix/Profile/Generation.hs`], [`src/Nix/Profile/Manifest.hs`]). A
  generation is no derivation. The active packages' files are linked into
  one tree by priority ([`src/Nix/Profile/Tree.hs`]), a `manifest.json` is
  written beside them, and the whole is added as a content-addressed path
  named `profile` whose references are the packages' paths. Its link,
  `PROFILE-N-link` beside the profile, is made and rooted as `build`'s
  out-link is, and the profile is a link to its current generation. A
  change makes the new generation's path, then takes the profile's lock,
  and starts again if the profile's link moved meanwhile.
  `builtin:buildenv` ([`src/Nix/Builder/Buildenv.hs`]) builds the same
  tree as the output of the derivation `nix-env` evaluates; nova-nix has
  no `nix-env` command.
  Where a user's profiles live is in the [command-line reference][cli].
- **`develop` and `print-dev-env`**, on Linux and macOS. Each builds a
  copy of the target's derivation ([`src/Nix/Develop/Capture.hs`]) whose
  builder runs upstream's `get-env.sh` ([`data/develop/`](../../data/develop)),
  which sources the stdenv's setup and writes bash's variables and
  functions as JSON. [`src/Nix/Develop/Environment.hs`] reads that
  document and renders the script that sets them again; `develop` starts
  `bash` with it, and `print-dev-env` prints it. Windows refuses both, and
  `nix-shell` ([#690]) has no command.
- **The daemon** ([#512], and on Windows [#633]). Each `nova-nix` process
  opens the store itself, so whoever builds needs write access to it. The
  daemon is to own the store and build for clients in upstream's worker
  protocol, whose framing and values [`src/Nix/Daemon/`] reads and
  writes; nothing serves it yet. `eval`, `build` and the profile commands
  run over [`StoreOps`][ops], a record of what they ask of a store, so a
  store reached through a daemon could take the local one's place there.
  `push`, `store gc`, `store delete` and `trust`'s fetches open the store
  directly.

[cli]: https://novavero.ai/docs/nova-nix/cli
[nova-cache]: https://github.com/Novavero-AI/nova-cache
[#341]: https://github.com/Novavero-AI/nova-nix/issues/341
[#432]: https://github.com/Novavero-AI/nova-nix/issues/432
[#476]: https://github.com/Novavero-AI/nova-nix/issues/476
[#512]: https://github.com/Novavero-AI/nova-nix/issues/512
[#633]: https://github.com/Novavero-AI/nova-nix/issues/633
[#635]: https://github.com/Novavero-AI/nova-nix/issues/635
[#678]: https://github.com/Novavero-AI/nova-nix/issues/678
[#682]: https://github.com/Novavero-AI/nova-nix/issues/682
[#690]: https://github.com/Novavero-AI/nova-nix/issues/690
[#712]: https://github.com/Novavero-AI/nova-nix/issues/712
[builder]: ../../src/Nix/Builder.hs
[derivation]: ../../src/Nix/Derivation.hs
[handle]: ../../src/Nix/Store/Handle.hs
[ops]: ../../src/Nix/Store/Ops.hs
[realise]: ../../src/Nix/Realise.hs
[store]: ../../src/Nix/Store.hs
[`sandbox/`]: ../../sandbox
[`src/Nix/Builder.hs`]: ../../src/Nix/Builder.hs
[`src/Nix/Builder/BuildDir.hs`]: ../../src/Nix/Builder/BuildDir.hs
[`src/Nix/Builder/Buildenv.hs`]: ../../src/Nix/Builder/Buildenv.hs
[`src/Nix/Builder/BuildUser.hs`]: ../../src/Nix/Builder/BuildUser.hs
[`src/Nix/Builder/Log.hs`]: ../../src/Nix/Builder/Log.hs
[`src/Nix/Builder/Sandbox.hs`]: ../../src/Nix/Builder/Sandbox.hs
[`src/Nix/Builder/Sandbox/Spawn.hs`]: ../../src/Nix/Builder/Sandbox/Spawn.hs
[`src/Nix/Daemon/`]: ../../src/Nix/Daemon
[`src/Nix/DependencyGraph.hs`]: ../../src/Nix/DependencyGraph.hs
[`src/Nix/Develop/Capture.hs`]: ../../src/Nix/Develop/Capture.hs
[`src/Nix/Develop/Environment.hs`]: ../../src/Nix/Develop/Environment.hs
[`src/Nix/Hash.hs`]: ../../src/Nix/Hash.hs
[`src/Nix/Profile/Generation.hs`]: ../../src/Nix/Profile/Generation.hs
[`src/Nix/Profile/Manifest.hs`]: ../../src/Nix/Profile/Manifest.hs
[`src/Nix/Profile/Tree.hs`]: ../../src/Nix/Profile/Tree.hs
[`src/Nix/Realise.hs`]: ../../src/Nix/Realise.hs
[`src/Nix/Store/DB.hs`]: ../../src/Nix/Store/DB.hs
[`src/Nix/Store/GC.hs`]: ../../src/Nix/Store/GC.hs
[`src/Nix/Store/Handle.hs`]: ../../src/Nix/Store/Handle.hs
[`src/Nix/Store/Lock.hs`]: ../../src/Nix/Store/Lock.hs
[`src/Nix/Store/Metadata.hs`]: ../../src/Nix/Store/Metadata.hs
[`src/Nix/Store/Upstream.hs`]: ../../src/Nix/Store/Upstream.hs
[`src/Nix/Substituter.hs`]: ../../src/Nix/Substituter.hs
[`src/Nix/Trust.hs`]: ../../src/Nix/Trust.hs
