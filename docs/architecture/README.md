# Architecture

These pages say how nova-nix works: what its parts are, how a command
passes through them, and which modules to open for each. What each
command and flag does is in the [command-line reference][cli], the
library's API is on [Hackage][hackage], and [the README](../../README.md)
says how to install and use nova-nix.

## The parts

nova-nix is a Haskell library with a C data layer under its evaluator,
the `nova-nix` executable over that library, and on Linux and macOS a
small C program, `nova-nix-sandbox`, that sandboxed builds run through.

- **The command line.** [`app/Main.hs`] reads the arguments
  ([`cli/Nix/Cli/Args.hs`]), the `nix.conf` files
  ([`src/Nix/Config.hs`]) and the store directory, and runs one command.
  Every command and flag is described once, in
  [`cli/Nix/Cli/Reference.hs`], which `--help` prints.
- **The parser.** [`src/Nix/Parser.hs`] and [`src/Nix/Parser/`] turn a
  file's bytes into an expression tree, and [`src/Nix/Expr/`] binds its
  variables and makes its relative paths absolute. Parsing is pure.
- **The compiler and the evaluator.** [`src/Nix/Eval/Compile.hs`]
  flattens the tree into bytecode, and [`src/Nix/Eval.hs`] runs the
  bytecode lazily, over thunks. The evaluator reaches files, processes
  and the store only through the `MonadEval` class, whose real instance
  is [`src/Nix/Eval/IO.hs`].
- **The C data layer.** [`cbits/`] holds every value an evaluation makes:
  thunks, environments, attribute sets, lists, lambdas, strings, interned
  names and the bytecode. Its region collector frees memory as evaluation
  goes, at the ends of thunks' evaluations, marking only what those
  evaluations allocated rather than the whole graph.
- **Derivations.** `builtins.derivationStrict`, in `src/Nix/Eval.hs`,
  turns a derivation's attributes into a `.drv` file
  ([`src/Nix/Derivation.hs`]) and its outputs' paths, hashed as upstream
  Nix hashes them ([`src/Nix/Hash.hs`]).
- **The store.** A directory of immutable paths and, inside it, a SQLite
  database of the valid ones and their references, their locks, the
  garbage collector's roots and the build directories
  ([`src/Nix/Store.hs`], [`src/Nix/Store/`]).
- **Realisation.** [`src/Nix/Realise.hs`] decides, for each derivation a
  build needs, whether to download its outputs or build it.
  [`src/Nix/Substituter.hs`] downloads from binary caches, and
  [`src/Nix/Builder.hs`] runs builders, in a sandbox on Linux, and on
  macOS when `sandbox` is on ([`src/Nix/Builder/Sandbox.hs`],
  [`sandbox/`]).
- **Profiles.** [`src/Nix/Profile/`] keeps a user's profiles:
  generations of packages linked into one tree, each a garbage-collector
  root.
- **Pushing and reports.** [`src/Nix/Push.hs`] uploads store paths to a
  nova-cache server or writes a binary cache to a directory, and
  [`src/Nix/Trust.hs`] reports what a closure relies on.

Two projects of their own stand beside it. [nova-cache] is the Haskell
library nova-nix reads and writes NARs, narinfos and their Ed25519
signatures with, and the cache server `push` uploads to.
[install-nova-nix] downloads a release, puts it on `PATH`, and sets up
`/nix`, or `C:\nix` on Windows, in which nova-nix creates the store the
first time it opens it.

## How a build passes through them

`nova-nix build ./default.nix -A hello` uses every part:

1. `main` reads the arguments and the configuration and settles the
   store directory: `/nix/store`, `C:\nix\store` on Windows, or the one
   `--store` names. A directory upstream Nix's database also manages is
   refused before anything is evaluated.
2. The store's operations are made available as one record,
   [`StoreOps`][ops], over a store that opens the first time an operation
   needs it. One open handle serves the evaluation and the build.
3. An evaluation session starts ([`withEvalSession`][evalio]): the C data
   layer's arena is set up, the region collector starts, and the base
   environment of builtins and the search path is built.
4. The target is read, parsed, compiled to bytecode and evaluated.
   `-A hello` selects the attribute, and its `drvPath` is forced.
5. Forcing `drvPath` runs `derivationStrict`, which renders the `.drv`
   file, computes its path and its outputs' paths, and records the file's
   bytes in the session. Each derivation it depends on that this
   evaluation computes was recorded the same way first, since its paths
   were needed; one an earlier run evaluated, named only through a
   string's context, is read from the store instead. Importing a path
   that a derivation builds (import from derivation) builds it then,
   through the same store operations.
6. The session ends and the arena is torn down, freeing every value.
   What the build needs was read out first: the `.drv` path, the output's
   name, and what evaluation made for the store (the sources it copied,
   the files it wrote and the `.drv` files it recorded).
7. The store adds those, references first, and registers them.
8. The build reads the `.drv` graph back from the store and realises it.
   A derivation's outputs are downloaded from a configured binary cache
   when one holds them, before anything they were built from is
   considered, and what no cache holds is built, its inputs first.
9. Each built output is scanned for references, made read-only and
   registered. The `./result` link is made and registered as a garbage
   collector root, and the output's path is printed.

`nova-nix eval` settles the store directory, starts a session, evaluates
its target and prints the value. Its session is read-only by default, as
`nix-instantiate --eval` is: it computes the paths of what evaluation
makes without writing them, refuses every substitution, and writes only
what a fetcher downloads and what an import from derivation builds, which
is an output that may not be substituted.

`trust` evaluates read-only too, then fetches the closure's fixed-output
inputs that a builtin fetches, downloads the others from the configured
caches, builds none of them, and scans what the store holds to report on
them. `profile add` builds what it adds as `build` does, or takes a path
already in the store, and it and `profile remove` make the profile's next
generation. `develop` builds a copy of its target's derivation that
records the build environment, then starts `bash` in that environment,
and `print-dev-env` prints it. `push` reads a closure from the store's
database and uploads
it. `store gc` and `store delete` run the garbage collector
([`src/Nix/Store/GC.hs`]).

## Not built yet

Each `nova-nix` process opens the store itself. There is no daemon yet
([#512], and on Windows [#633]), so whoever builds needs write access to
the store. `nix-shell` ([#690]) and flakes ([#514]) have no command, and
`develop` none on Windows. Some of the pieces the daemon rests on are in
the library, as
[The store and builds](store-and-builds.md#profiles-develop-and-the-daemon)
describes.

## The pages

- [The evaluator](evaluator.md): parsing, compilation to bytecode,
  evaluation over thunks, the C data layer and its region collector,
  string context, derivations, `import` and the search path, pure and
  restricted evaluation, and how builtins reach the store.
- [The store and builds](store-and-builds.md): the store's layout and
  database, store paths and their hashes, realisation, substitution, how
  a derivation is built, the sandbox on each OS, garbage collection,
  profiles, and where `develop` and the daemon fit.
- [Windows](windows.md): the store's location and access, links and
  Developer Mode, path spellings, case sensitivity, how a build runs, the
  bootstrap from the seed, and the planned service and sandbox.

[cli]: https://novavero.ai/docs/nova-nix/cli
[hackage]: https://hackage.haskell.org/package/nova-nix
[nova-cache]: https://github.com/Novavero-AI/nova-cache
[install-nova-nix]: https://github.com/Novavero-AI/install-nova-nix
[#512]: https://github.com/Novavero-AI/nova-nix/issues/512
[#514]: https://github.com/Novavero-AI/nova-nix/issues/514
[#633]: https://github.com/Novavero-AI/nova-nix/issues/633
[#690]: https://github.com/Novavero-AI/nova-nix/issues/690
[evalio]: ../../src/Nix/Eval/IO.hs
[ops]: ../../src/Nix/Store/Ops.hs
[`app/Main.hs`]: ../../app/Main.hs
[`cbits/`]: ../../cbits
[`cli/Nix/Cli/Args.hs`]: ../../cli/Nix/Cli/Args.hs
[`cli/Nix/Cli/Reference.hs`]: ../../cli/Nix/Cli/Reference.hs
[`sandbox/`]: ../../sandbox
[`src/Nix/Builder.hs`]: ../../src/Nix/Builder.hs
[`src/Nix/Builder/Sandbox.hs`]: ../../src/Nix/Builder/Sandbox.hs
[`src/Nix/Config.hs`]: ../../src/Nix/Config.hs
[`src/Nix/Derivation.hs`]: ../../src/Nix/Derivation.hs
[`src/Nix/Eval.hs`]: ../../src/Nix/Eval.hs
[`src/Nix/Eval/Compile.hs`]: ../../src/Nix/Eval/Compile.hs
[`src/Nix/Eval/IO.hs`]: ../../src/Nix/Eval/IO.hs
[`src/Nix/Expr/`]: ../../src/Nix/Expr
[`src/Nix/Hash.hs`]: ../../src/Nix/Hash.hs
[`src/Nix/Parser.hs`]: ../../src/Nix/Parser.hs
[`src/Nix/Parser/`]: ../../src/Nix/Parser
[`src/Nix/Profile/`]: ../../src/Nix/Profile
[`src/Nix/Push.hs`]: ../../src/Nix/Push.hs
[`src/Nix/Realise.hs`]: ../../src/Nix/Realise.hs
[`src/Nix/Store.hs`]: ../../src/Nix/Store.hs
[`src/Nix/Store/`]: ../../src/Nix/Store
[`src/Nix/Store/GC.hs`]: ../../src/Nix/Store/GC.hs
[`src/Nix/Substituter.hs`]: ../../src/Nix/Substituter.hs
[`src/Nix/Trust.hs`]: ../../src/Nix/Trust.hs
