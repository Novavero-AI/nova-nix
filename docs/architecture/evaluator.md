# The evaluator

An expression becomes a value in three steps. The parser reads source
into a tree, the compiler flattens the tree into bytecode, and the
evaluator runs the bytecode lazily, forcing thunks as their values are
needed. Every value an evaluation makes lives in a C data layer, which a
region collector reclaims as evaluation goes.

## Parsing

[`src/Nix/Parser/Lexer.hs`] reads a source's bytes into tokens. Every
token class is ASCII, as upstream's lexer's are, and a string literal
holds whatever bytes it holds, so a Nix string is a byte string from the
first step. A mode stack follows string interpolation, and a column counts
bytes. A source that begins with a UTF-16 or UTF-8 byte-order mark is
read in that encoding first ([`src/Nix/Parser.hs`]).

[`src/Nix/Parser/Expr.hs`] is a recursive-descent parser over the tokens
that produces an `Expr` ([`src/Nix/Expr/Types.hs`]). It also records each
name where upstream's parser interns one, in the same order
(`ParsedSource`). A name's place in the symbol table is the order it was
interned in, and an attribute set keeps its attributes in symbol order,
so interning a program's names in upstream's order is what gives its sets
upstream's order.

[`parseNixWithScope`][parser] then runs three pure passes over the tree:

- **Binding variables** ([`src/Nix/Expr/Resolve.hs`], upstream's
  `bindVars`). A variable bound by a lambda, or by a `let` or recursive
  set whose every binding is a single static name or an `inherit`,
  becomes a pair of indices: how many scopes up, and which slot. A name
  bound by any other `let` or recursive set, or by the root environment,
  is looked up by name when it runs, and a name only a `with` could
  supply is looked up in the `with` scopes. A variable nothing binds fails
  the whole expression here, before anything is evaluated.
- **Trimming closures** ([`src/Nix/Expr/ClosureTrim.hs`]). A lambda, `let`
  or recursive set whose references to outer scopes are all resolved
  records the slots it uses, so the evaluator gives it a flat environment
  of those alone instead of one that keeps its whole parent chain alive.
- **Resolving paths.** A relative path literal becomes absolute against
  the directory of the file it is written in, so what it names does not
  depend on which file is being evaluated when it is forced.

A `<name>` path parses as the call `__findFile __nixPath "name"`, which the
search path answers ([below](#import-and-the-search-path)).

## Compilation

[`src/Nix/Eval/Compile.hs`] compiles the tree into bytecode held in the C
data layer ([`cbits/nn_bytecode.h`]). Each expression node is one 16-byte
instruction, and children are compiled before their parent, so a child's
index is always below its parent's. Operands of variable length (a
string's parts, a set's bindings, a lambda's formals, a closure's captured
slots, an attribute path) go in a separate buffer of 32-bit words, and the
bytes of literals in a string table the code owns. A literal's value
points at those bytes rather than copying them, since the code outlives
every value made from it.

Compiling interns names, so it runs in an evaluation session.
[`evalParsed`][eval] interns the parsed names first, then compiles, then
builds the environment the code runs in, and [`evalBytecode`][eval] runs
it: it reads an instruction's opcode from C and dispatches on it.

## Evaluation

Evaluation is lazy. A `let` binding, an attribute, a list element and a
function argument become thunks, and a thunk is forced, evaluated, only
when its value is needed.

A thunk is a 16-byte cell in the C data layer ([`cbits/nn_thunk.h`]) in
one of three states:

- **Pending.** It holds the index of the instruction to run and the
  environment to run it in, or a deferred application of a function to an
  argument, which a builtin such as `map` makes for each element.
- **Blackhole.** It is being forced. Forcing it again before that ends is
  infinite recursion, which aborts evaluation, as upstream's does.
- **Computed.** It holds its value: a scalar inline, or a pointer to a C
  object.

The IO evaluator's [`forceThunk`][evalio] marks the cell, runs what it
holds and stores the value. A force that fails puts the cell back to
pending, so a later force after `builtins.tryEval` caught the failure
evaluates it again instead of reporting recursion.

An environment ([`cbits/nn_env.h`]) is a word followed by its slots, one
thunk each. In an environment of one to seven slots with no scopes (a
name table or `with` scopes) the word holds the parent's address; in any
other it is a header saying which fields follow the slots. A resolved
variable is one lookup by scope depth and slot. A `with` scope is a thunk
forced only when a lookup reaches it.

The values are C objects too:

- an attribute set is arrays of symbols, positions and thunks sorted by
  symbol and searched by bisection, and a set that `//` makes can be a
  layer of the right operand's entries over the left set
  ([`cbits/nn_attrset.h`]);
- a list is an array of thunks ([`cbits/nn_list.h`]);
- a lambda is its environment, its body's instruction and its formals
  ([`cbits/nn_lambda.h`]);
- a builtin applied to fewer arguments than it takes is the builtin and
  the arguments it has ([`cbits/nn_primop.h`]);
- a string or a path is its bytes ([`cbits/nn_str.h`]), never interned, and
  a string with context is one object pointing at its text and at each
  context element's store path and output name, each of them such a
  string ([`cbits/nn_ctxstr.h`]).

Names (attribute names, formals, variables) are interned in a symbol
table ([`cbits/nn_symbol.h`]) and compared as integers. The Haskell
[`NixValue`][types] is a view of a value read out of a cell, and storing
one writes it back.

## Effects

The evaluator is written against [`MonadEval`][types], a class of the
effects evaluation needs: raising and catching evaluation errors, reading
files and directories, importing, running processes, reading and writing
the store, and memoizing a forced thunk. [`EvalIO`][evalio] is the
instance the commands run. `PureEval`, in the same module as the class,
runs the same evaluator with no IO for tests: it re-evaluates a thunk each
time it is forced and has no store.

Every read of, write to and allocation in the C data layer is an action
in the evaluator's monad ([`src/Nix/Eval/CData.hs`]), run where the
evaluation reaches it and in order with the actions around it, so a
collector can free memory at points the evaluation knows. Two functions
read the layer outside the monad by design, each with the invariant that
makes it sound: `readImmutable`, for an interned name's bytes, and
`runCDataPure`, which runs a whole pure evaluation as a value.

The builtins are one type, [`Builtin`][builtin], with a constructor for
each primop. `src/Nix/Eval.hs` defines each, and [`src/Nix/Builtins.hs`]
builds the base environment from them, the constants and `derivation`,
which is Nix source over the `derivationStrict` primop, as upstream's is.

## The C data layer

Every value lives in C ([`cbits/`]) and Haskell holds only pointers into
it, so the value graph takes nothing on GHC's heap and GHC's collector
never walks it. The layer is strict C99, held to warnings as errors and
run under AddressSanitizer and UndefinedBehaviorSanitizer in CI
([CONTRIBUTING.md]).

### Sessions

An evaluation session ([`withEvalSession`][evalio]) owns the layer's
arena. The outermost session sets up the symbol table, the string arena,
the thunk arena, the environment pages and the bytecode store as it
starts ([`src/Nix/Eval/Arena.hs`]), and tears them down as it ends,
freeing every value at once. A session started inside another shares its
arena. Outside a session the allocators refuse, and the Haskell side
raises `ArenaNotInitialized` instead of touching memory that does not
exist. A few cells are static and outlive every session: `null`, `true`,
`false`, the empty list and each builtin's own cell.

Objects are allocated in two ways. Thunks, environments (and the arrays
of a list's items, a `with`'s scopes and a deferred application's cells)
and string bytes are bumped from blocks. Attribute sets, list headers,
lambdas, partial applications and strings with context are allocated one
at a time and recorded in a tracker for their kind
([`cbits/nn_tracker.h`]), which frees them all at teardown.

A pointer Haskell keeps past one evaluation step is a root, registered as
it is made ([`cbits/nn_roots.h`]): the session's base environment, each
imported file's value in the import cache, and the cells `--arg` makes.

### Regions

The region collector ([`cbits/nn_region.h`], bound in
[`src/Nix/Eval/CRegion.hs`]) reclaims memory during evaluation without
walking the whole graph.

A region is the extent of one force of a thunk: everything the data
layer allocates from the force's start to the store of its value, inner
forces included. `forceThunk` opens one as each force starts and closes it
once the value is stored or the force throws. When a force has returned,
an object it allocated can be reached from outside it in only two ways:
through a pointer the force wrote into an older object (the forced cell,
a cell an inner force memoized, a `with` scope resolved to its set), or
through a root registered during it. Regions record every such write. At
a close, a mark that starts from the records of older objects and the
roots registered since the region opened, and stops at anything older
than the region, finds what the region allocated that is still reached.
The rest is garbage.

Age is allocation order. A block is handed out in spans, each numbered as
it starts, and a tracked object's age is its place in its tracker, so
whether an object is younger than a region is a comparison of numbers.

The collector tenures, with a budget of 1 MiB. It marks only at the close
of a region that allocated at least 16 KiB; a smaller region's
allocations pass to the region around it unexamined, and an outermost
region under 16 KiB is left to the teardown. A close marks only what no
close inside its region has already tenured, then tenures the region. The
outermost close, and a close whose region holds at least the budget in
tenured bytes that no mark of the whole region has reached since, marks
its region whole. At each close that marks, the collector frees the
region's tracked objects the mark left unreached, keeping each tracker in
age order, and each 128-byte line of a block once every unit in it is
free. An allocator whose block is full bumps next from a run of freed
lines, in a span of its own, so what it puts there is younger than every
open region. Garbage can outlive the force that made it: what a close
tenured is examined again only by a mark of a whole region around it.

The collector checks its premise as it runs: a mark that reaches
something already freed aborts the process. Under AddressSanitizer a
freed line is poisoned until it is handed out again. When one of the
regions' tables cannot grow, the collector stops freeing and leaves the
rest to the teardown.

Every command that evaluates runs the collector. `eval` without `--aterm`
runs regions as a census instead when `NOVA_NIX_REGION_STATS` names a
file, which frees nothing and reports what collectors freeing at each
close would hold, and runs none when `NOVA_NIX_C_STATS` names one, to
report everything the evaluation allocated ([command-line
reference][cli]). Bringing evaluation's peak memory down to upstream's is
[#466].

## String context

A string carries a context: the store paths its bytes name, so that a
derivation built from it depends on them. Each element of a context is
one of three kinds ([`StringContextElement`][types]):

- a plain store path, such as a source copied to the store or a file
  `builtins.toFile` wrote;
- one output of a derivation, which an output path such as
  `"${pkgs.hello}"` carries;
- every output of a derivation, which a `drvPath` carries.

Concatenation and interpolation merge contexts
([`src/Nix/Eval/Context.hs`], [`src/Nix/Eval/StringInterp.hs`]).

A builtin that reads a path (`import`, `builtins.readFile` and the like)
first realises the path's context ([`src/Nix/Eval/RealiseContext.hs`]):
every path it names must be valid, and every output it names is realised
then, unless `allow-import-from-derivation` is off. That is import from
derivation, and it runs through the same store operations as a build
([The store and builds](store-and-builds.md)), within what the session's
store allows ([below](#how-builtins-reach-the-store)).

## Derivations

`derivation` returns a set whose `drvPath` and `outPath` are lazy, so
forcing a derivation to see its attributes builds nothing and computes no
path. Forcing either runs `derivationStrict`
([`builtinDerivationStrict`][eval]), which:

1. forces every attribute into the environment the builder gets, or
   under `__structuredAttrs` into one JSON document;
2. takes the inputs from the contexts of what it forced: plain paths
   become input sources, outputs become input derivations with the
   outputs named, and an every-outputs element brings in the named
   derivation's whole closure, every `.drv` with all its outputs;
3. computes the output paths and the `.drv` file's path
   ([Store paths](store-and-builds.md#store-paths)), using each input
   derivation's hash, cached in the session as each one is computed, or
   computed from the input's `.drv` in the store when the session did not
   evaluate it;
4. records the rendered `.drv` bytes ([`src/Nix/Derivation.hs`]) in the
   session, for a build to write to the store, and returns `drvPath` and
   each output's path, with their contexts.

Inputs are computed before their dependents, so by the time a derivation
is recorded, every derivation it depends on that the session evaluated
has been; one it names only through a string's context, evaluated by an
earlier run, is read from the store ([`derivationAt`][eval]).

## Import and the search path

`import` ([`importFile`][evalio]) resolves its argument, checks it against
the evaluation policy, and looks it up in the session's import cache,
keyed by the resolved file. A file not imported yet is read, parsed
against the base environment's names, and evaluated in the base
environment with the file's directory as the base for its relative
paths. Its value is cached in a cell registered as a root, so importing
the same file again gives the same value. `scopedImport` parses against
its scope's names too, and is not cached.

The search path is `builtins.nixPath`, a list of `{ prefix, path }` sets
made as the session starts ([`src/Nix/Builtins.hs`]). It holds the
entries `--nix-path` gives, then `NIX_PATH`'s, then the data directory
nova-nix ships ([`data/`](../../data)), whose `nix/fetchurl.nix` answers
`<nix/fetchurl.nix>`. That directory is `NIX_DATA_DIR` when it is set,
otherwise `share/nova-nix` beside the executable's `bin/`, as a release
archive lays it out, otherwise the one Cabal installed
([`resolveDataDir`][main]).

## Pure and restricted evaluation

[`EvalPolicy`][policy] holds upstream's `restrict-eval`, `pure-eval`,
`allowed-uris`, `trace-verbose` and import-from-derivation settings.
Under either `restrict-eval` or `pure-eval`, every file the evaluation
reads passes an allow list: a path is readable when it lies under an
allowed prefix or is an ancestor of one, and the list starts as the
search path's roots and grows by every store object the evaluation
copies, fetches or writes. `builtins.getEnv` answers empty.

- Under `restrict-eval`, a fetch must also match `allowed-uris`.
- Under `pure-eval`, the search path holds only the bundled expressions,
  `builtins.currentTime` and `builtins.currentSystem` do not exist,
  `builtins.storePath` is refused, and the fetchers demand a hash or a
  revision.

The settings come from `nix.conf`, and a command-line flag only moves one
toward confinement, never away from it: `--restrict-eval` and
`--pure-eval` turn theirs on, and `--no-allow-import-from-derivation`
turns its off ([`evalPolicyFor`][main]). The rules are pure functions,
the walk of a path through its symlinks included, which the IO evaluator
applies. This policy is unrelated to `PureEval`, the evaluator instance
with no IO.

## How builtins reach the store

The evaluator reaches the store only through `MonadEval`'s store
methods, which `EvalIO` carries out against the session's store
([`SessionStore`][evalio]):

- Coercing a path to a string (`"${./src}"`, or a path among a
  derivation's attributes) hashes the tree and names its store path
  without copying it. The copy is recorded, and a build restores it from
  the tree that was hashed.
- `builtins.toFile` names a text path from its contents and references.
- `builtins.path` and `builtins.filterSource` add a tree by its NAR hash,
  and `builtins.path` with `recursive = false` adds a single file by the
  hash of its bytes.
- The fetchers download into the store, at the path their hash fixes,
  through the programs the README's [Status](../../README.md#status)
  section names.
- `derivationStrict` reads a `.drv` it did not evaluate from the store.

A session's store is one of two kinds. `eval` runs a read-only one by
default, as `nix-instantiate --eval` does: it asks the store's database
what is valid without writing to it, computes the paths of what
evaluation makes without writing them, and writes and registers at once
only what a fetcher fetched. It refuses every substitution, so an import
from derivation there builds an output that may not be substituted and
fails on any other that is not valid yet, as upstream's read-only
evaluation does. `build`, `eval` with `--read-write-mode` or `--aterm`,
and `profile add` run a read-write one: what evaluation makes is written
or recorded, and the store adds all of it, references first, before any
realisation and once evaluation ends ([`soAddEvalObjects`][ops]).

[cli]: https://novavero.ai/docs/nova-nix/cli
[#466]: https://github.com/Novavero-AI/nova-nix/issues/466
[CONTRIBUTING.md]: ../../CONTRIBUTING.md
[parser]: ../../src/Nix/Parser.hs
[eval]: ../../src/Nix/Eval.hs
[evalio]: ../../src/Nix/Eval/IO.hs
[types]: ../../src/Nix/Eval/Types.hs
[builtin]: ../../src/Nix/Eval/Builtin.hs
[policy]: ../../src/Nix/Eval/Policy.hs
[main]: ../../app/Main.hs
[ops]: ../../src/Nix/Store/Ops.hs
[`cbits/`]: ../../cbits
[`cbits/nn_attrset.h`]: ../../cbits/nn_attrset.h
[`cbits/nn_bytecode.h`]: ../../cbits/nn_bytecode.h
[`cbits/nn_ctxstr.h`]: ../../cbits/nn_ctxstr.h
[`cbits/nn_env.h`]: ../../cbits/nn_env.h
[`cbits/nn_lambda.h`]: ../../cbits/nn_lambda.h
[`cbits/nn_list.h`]: ../../cbits/nn_list.h
[`cbits/nn_primop.h`]: ../../cbits/nn_primop.h
[`cbits/nn_region.h`]: ../../cbits/nn_region.h
[`cbits/nn_roots.h`]: ../../cbits/nn_roots.h
[`cbits/nn_str.h`]: ../../cbits/nn_str.h
[`cbits/nn_symbol.h`]: ../../cbits/nn_symbol.h
[`cbits/nn_thunk.h`]: ../../cbits/nn_thunk.h
[`cbits/nn_tracker.h`]: ../../cbits/nn_tracker.h
[`src/Nix/Builtins.hs`]: ../../src/Nix/Builtins.hs
[`src/Nix/Derivation.hs`]: ../../src/Nix/Derivation.hs
[`src/Nix/Eval/Arena.hs`]: ../../src/Nix/Eval/Arena.hs
[`src/Nix/Eval/CData.hs`]: ../../src/Nix/Eval/CData.hs
[`src/Nix/Eval/Compile.hs`]: ../../src/Nix/Eval/Compile.hs
[`src/Nix/Eval/Context.hs`]: ../../src/Nix/Eval/Context.hs
[`src/Nix/Eval/CRegion.hs`]: ../../src/Nix/Eval/CRegion.hs
[`src/Nix/Eval/RealiseContext.hs`]: ../../src/Nix/Eval/RealiseContext.hs
[`src/Nix/Eval/StringInterp.hs`]: ../../src/Nix/Eval/StringInterp.hs
[`src/Nix/Expr/ClosureTrim.hs`]: ../../src/Nix/Expr/ClosureTrim.hs
[`src/Nix/Expr/Resolve.hs`]: ../../src/Nix/Expr/Resolve.hs
[`src/Nix/Expr/Types.hs`]: ../../src/Nix/Expr/Types.hs
[`src/Nix/Parser.hs`]: ../../src/Nix/Parser.hs
[`src/Nix/Parser/Expr.hs`]: ../../src/Nix/Parser/Expr.hs
[`src/Nix/Parser/Lexer.hs`]: ../../src/Nix/Parser/Lexer.hs
