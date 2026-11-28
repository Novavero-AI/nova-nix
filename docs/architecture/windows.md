# Windows

On Windows nova-nix is a native x64 program with no POSIX layer under it,
and on Windows on Arm the same executable runs under x64 emulation. The
evaluator, the store and the builder are the code they are on Linux and
macOS. This page says where Windows changes how they work, and how
[`pkgs/windows`](../../pkgs/windows) builds a toolchain on Windows itself
from a seed. The README's [Set up](../../README.md#set-up) section says
what a user needs to do.

## The store's location and access

The store is `C:\nix\store` ([`platformStoreDir`][path]), with its state in
`C:\nix\store\.nova-nix` as on every platform
([The store and builds](store-and-builds.md#layout)). Store paths are
hashed with `/nix/store` and expressions see `/nix/store/...`, so a
derivation evaluated on Windows has the paths it has elsewhere; the
`C:\nix\store` spelling appears only where a path meets the machine: a
file opened, a builder spawned, a database row, a path printed.

A directory created with no security descriptor takes the access list its
parent passes down, and a drive root passes down modify for every
signed-in user. So nova-nix creates each directory of a store with a
protected access list, which inherits nothing, in the same
`CreateDirectoryW` call that makes it ([`src/Nix/Store/Access.hs`],
[`cbits/nn_winfs.c`]):

| Directory | Full control | Read and execute |
| --- | --- | --- |
| The store and `.nova-nix` | the user, SYSTEM, Administrators | Users |
| A parent made on the way, such as `C:\nix` | the user, SYSTEM, Administrators | nobody else |
| A build directory | the user | nobody else |

The user is the one nova-nix runs as, and every grant is inherited by
what is made below. [install-nova-nix] gives `C:\nix` the same list as
the store's, and accepts a `C:\nix` nova-nix made. A directory that
already exists keeps the list it has; refusing an existing `C:\nix` that
another user could control is [#660].

When a path is added, each file and directory in it gets the read-only
attribute and one modification time ([`src/Nix/Store/Metadata.hs`]). On a
directory the attribute does not stop entries being added or removed;
only an access list could, which the planned sandbox's store layout gives
([below](#planned-the-service-and-the-sandbox)).

## Links and Developer Mode

nova-nix makes native symbolic links: the symlinks a NAR holds, when it
is unpacked into a store path, the out-link `build` makes, and a
profile's links. Windows lets a process make one only in Developer Mode
or elevated. Without either, a path that holds a symlink fails to build
or download, with an error naming the link, rather than being stored in
a form whose NAR hash would differ ([`src/Nix/Store/Symlink.hs`]).

A Windows link is a file link or a directory link. A NAR's link is made
as what its target is, so a NAR's links are made after the rest of its
tree, each once its target exists. A profile's links all lead to
directories, and a rename cannot replace a directory link, so switching a
profile removes its link and makes it again
([`src/Nix/Profile/Generation.hs`]).

Where nova-nix needs no link it makes none. A record under
`gcroots/auto` is a regular file holding the out-link's path, a permanent
root can be a regular file named for its path, and `builtin:unpack` copies
an archive's symlink and hard-link entries ([`src/Nix/Builder/Unpack.hs`]).

## Path spellings

A path value is `/`-separated text, led by a drive such as `C:` where it
names one. A path rooted with no drive, such as
`/src/default.nix`, names that path on the store's drive, `C:` for the
default store, so it reads the same file from any current drive
([`qualifyPathText`][path]), and text under `/nix/store` resolves into the
store directory. The evaluation policy checks the same spelling the read
opens.

A builder gets its derivation's strings with `/nix/store/` rewritten to
the store directory followed by `/` ([`storeRewrite`][builder]), so it
sees `C:\nix\store/<hash>-<name>`. The seed stdenv's `setup.sh` respells
the paths it hands to configure and make with forward slashes,
`C:/nix/store/...`, since a backslash starts an escape in a sed script, a
C string and a word `sh` reads twice, and busybox and every Windows API
take `/` as well. For the same reason the bootstrap recipes hand store
paths to Mes and TinyCC in environment variables instead of compiling
them into strings. An output's references are found by the 32-character
hash alone ([`scanReferences`][store]), whichever spelling the build
wrote.

The store's own Win32 calls use the `\\?\` form, so a path longer than
`MAX_PATH` opens there as it does through GHC's I/O
([`src/Nix/Store/Win32Path.hs`]). An environment variable or argument,
UTF-16 on Windows, is read as the UTF-8 bytes a Nix string holds, and one
with an unpaired surrogate, which has no UTF-8 form, is refused
([`src/Nix/Environment.hs`]). A `.nix` file that begins with a UTF-16 or
UTF-8 byte-order mark, as Windows PowerShell's `>` writes one, is read in
that encoding.

## Case sensitivity

Windows compares names without regard to case, and a NAR can hold two
siblings whose names differ only by case. When one does, the directory
being unpacked is given NTFS's per-directory case-sensitivity flag while
it is still empty, the mechanism WSL uses, and both names are kept as
spelled ([`src/Nix/Store/CaseSensitive.hs`], [`src/Nix/Store.hs`]). Where
the flag is refused (a volume that is not NTFS, or a policy), the later
sibling takes upstream's case-hack suffix, `~nix~case~hack~<N>`, which
serialising strips again. Either way the path serialises back to the NAR
it was unpacked from. A source outside the store is read with the
case-hack on, as on macOS.

## The executable bit

A NAR records whether each file is executable, and Windows has no such
bit: what `System.Directory` reports comes from the file's extension.
nova-nix keeps the bit as an NTFS alternate data stream, `nova.exec`,
whose presence is the bit, set before a path is made read-only and
carried across copies ([`src/Nix/Store/ExecBit.hs`]). A file `tar.exe`
unpacks or `git` checks out has no stream, so an executable among them
hashes as a plain file when evaluation copies it to the store ([#430]).

## How a build runs

A build takes the same steps as anywhere
([Building a derivation](store-and-builds.md#building-a-derivation)), in
`C:\nix\store\.nova-nix\builds\<hash>`, which only the user can open. The
builder is spawned through the process library, which calls
`CreateProcess`:

- The builder's whole process tree runs in a Win32 job object that kills
  it when closed, so an interrupt ends the builder's descendants too, and
  the build waits until every process in the tree has exited, not only
  the builder.
- Its environment holds, besides the build's variables, `SystemRoot`,
  `SystemDrive` and `windir` from nova-nix's own environment, `COMSPEC`
  made from `SystemRoot`, and `PATHEXT` fixed at `.COM;.EXE;.BAT;.CMD`
  ([`scrubAmbient`][builder]). Names compare without regard to case, so
  two spellings of one name never both reach the builder.
- Unless the derivation sets them, `PATH` is the builder's directory, its
  sibling `usr\bin`, `C:\Windows\System32` and `C:\Windows`, the first
  two left out for a builder named relative to the working directory, and
  `USERPROFILE` is the build directory.
- A `cmd.exe /c` builder gets its command as one string, unquoted, since
  `cmd.exe` does not split arguments as other programs do.
- An `i686-windows` derivation runs directly on x64 Windows, through
  WoW64.

Nothing else contains a build: a builder can read and write whatever the
user can and reach the network. Which builds may reach the network is
[#634], and the sandbox is planned below.

## The Windows package set

[`pkgs/windows/default.nix`](../../pkgs/windows/default.nix) is a package
set laid out as nixpkgs' is: a `by-name/<shard>/<name>/package.nix` per
package, each called with its dependencies from the set itself. It has
two stdenvs with one `mkDerivation` interface, both made only of tools
built from the seed:

- `stdenv` ([`pkgs/windows/stdenv`]): the seed stdenv's shell and tools
  with the ladder's native x86_64 GCC and binutils behind compiler and
  linker wrappers, building for `x86_64-windows`. Its fixup phase copies
  each DLL an output's programs and libraries import, and Windows does
  not provide, beside them. [`hello.nix`](../../pkgs/windows/hello.nix)
  and the set's other packages build with it.
- `seedStdenv`, the seed stdenv ([`pkgs/windows/seed-stdenv`]):
  busybox-w32's `sh` and applets, GNU make and TinyCC. The ladder's rungs
  from binutils on build with it, as `i686-windows` derivations.

Sources are fetched by `builtin:fetchurl`, which runs inside nova-nix.
The archives stage0-pe32's chain reads are unpacked by `builtin:unpack`,
inside nova-nix too: nothing in the store can unpack an archive before
the ladder has built a tool that can, and the system's `tar.exe` is not
pinned ([`src/Nix/Builder/Unpack.hs`]). From Mes on, each rung unpacks
its source with tools the rungs below it built.

## The bootstrap from the seed

The seed ladder builds a toolchain on Windows itself, each rung by the
rungs below it, from [stage0-pe32]'s 1,623-byte seed, `hex0-seed.exe`, and
follows nixpkgs' minimal bootstrap rung for rung where it has one. Its
stages, grouped here from the ten lists
[`pkgs/windows/rungs.nix`](../../pkgs/windows/rungs.nix) gives CI:

1. **stage0** ([`stage0-pe32`][stage0]). The seed assembles `hex0`, which is
   fixed-output to the seed's own hash, so it must reproduce the seed
   byte for byte. Then come `hex1`, `hex2`, `catm`, `M0`, `cc_x86`,
   M2-Planet, `M1`, `hex2-new` and `kaem`, each step one command of
   stage0-pe32's `mescc-tools-mini.cmd`, and mescc-tools-extra. These are
   32-bit programs that reach Windows through ntdll. Nothing else runs:
   no `cmd.exe`, and no shell but `kaem`.
2. **Mes** ([`mes`][mes]). GNU Mes 0.28, from a fork that carries a Windows
   port: `mes-m2` built by the stage0 tools, the Mes C library compiled by
   MesCC on it, and `mes.exe`.
3. **TinyCC** ([`tinycc`][tinycc]). janneke's bootstrappable TinyCC
   compiled by MesCC, then by each round's `tcc` into the next, writing PE
   from the first round. Then upstream TinyCC's source, and then
   `tinycc-native`, a native Windows `tcc` that links against
   `msvcrt.dll` and `kernel32.dll`, compiled twice so that the second
   build is the first compiling itself; CI checks the two are the same
   bytes. GNU make 4.4.1 and busybox-w32, built by it, complete the seed
   stdenv.
4. **GCC 4.6.4.** binutils 2.41 and GCC 4.6.4's C compiler, built by
   TinyCC, with MinGW-w64 8.0.3's headers; MinGW-w64's runtime and libgcc,
   built by that GCC; then GCC 4.6.4 with C++, built by its C compiler.
5. **GCC 10.4.0** with C and C++, built by GCC 4.6.4, with MinGW-w64
   14.0.0's headers and the runtime it compiles; then **GCC 16.2.0**,
   built by GCC 10.
6. **x86_64.** Everything so far runs as i686 and writes i686. binutils
   2.46, GCC 16.2.0 and the MinGW-w64 runtime for `x86_64-w64-mingw32`
   follow as a cross toolchain running on i686, and then, built by it,
   binutils and GCC 16.2.0 that run on x86_64 and write x86_64.
7. **Stdenv.** The compiler and linker wrappers of the set's `stdenv`
   over that native toolchain.

Every rung is an ordinary derivation.

CI builds the ladder as one job per stage, each starting from the paths
the stage before built
([`seed-ladder.yml`](../../.github/workflows/seed-ladder.yml),
[`ladder.yml`](../../.github/workflows/ladder.yml)). A pull request that
changes more than documentation, or a push to main, builds it on x64
Windows, taking from the public cache each rung the cache holds, and a
push to main publishes what it holds. The
daily run, a release and a dispatched run build every rung from the seed
on x64 Windows and on ARM64 Windows, where the i686 programs run under
Windows' x86 emulation. An intermittent failure of the GCC 4.6.4 C++ rung
on ARM64 is [#698].

Among what remains toward [#453], a Windows stdenv bootstrapped from
source that builds unmodified nixpkgs:

- a bash built from source, since nixpkgs' `setup.sh` needs bash and
  busybox-w32's `sh` is not one ([#697]);
- packages from an unpatched nixpkgs on that stdenv ([#454]);
- every rung rebuilding to the same bytes, across rebuilds and across x64
  and ARM64 hosts;
- showing, with `nova-nix trust`, that the seed is the only prebuilt input
  in the stdenv's closure, and that no rung runs a program from outside
  the store;
- an outsider reproducing it from written instructions.

## Planned: the service and the sandbox

Neither is built. Each is designed in its issue, and this is the design.

- **The service** ([#633]). `nova-nix daemon` is to run as a Windows
  service under the virtual account `NT SERVICE\nova-nix`, which the
  store's access list would grant modify in place of the installing user.
  Clients would connect over the named pipe `\\.\pipe\nova-nix-daemon`,
  and the daemon would name a client by impersonating it and match its
  user and group SIDs against `trusted-users` and `allowed-users`. A user
  who cannot write the store would then build into it through the
  service.
- **The sandbox** ([#194]). Builds are to run as local build accounts, one
  per build slot, which the service starts through a Windows backend of
  `nova-nix-sandbox`, the helper Linux builds already run through. Each
  build would run on the window station of its slot's logon, with a
  pseudoconsole and a job with a per-process memory cap that no process
  can leave, and every process of its slot would be ended before and
  after it. The store directory would let build accounts create entries
  and nothing else, and registering an output would make the service its
  owner and the output read and execute for builds. The issue records
  why restricted, write-restricted and AppContainer tokens are not used.

[install-nova-nix]: https://github.com/Novavero-AI/install-nova-nix
[stage0-pe32]: https://github.com/Novavero-AI/stage0-pe32
[#194]: https://github.com/Novavero-AI/nova-nix/issues/194
[#430]: https://github.com/Novavero-AI/nova-nix/issues/430
[#453]: https://github.com/Novavero-AI/nova-nix/issues/453
[#454]: https://github.com/Novavero-AI/nova-nix/issues/454
[#633]: https://github.com/Novavero-AI/nova-nix/issues/633
[#634]: https://github.com/Novavero-AI/nova-nix/issues/634
[#660]: https://github.com/Novavero-AI/nova-nix/issues/660
[#697]: https://github.com/Novavero-AI/nova-nix/issues/697
[#698]: https://github.com/Novavero-AI/nova-nix/issues/698
[builder]: ../../src/Nix/Builder.hs
[path]: ../../src/Nix/Store/Path.hs
[store]: ../../src/Nix/Store.hs
[stage0]: ../../pkgs/windows/by-name/st/stage0-pe32/package.nix
[mes]: ../../pkgs/windows/by-name/me/mes/package.nix
[tinycc]: ../../pkgs/windows/by-name/ti/tinycc/package.nix
[`cbits/nn_winfs.c`]: ../../cbits/nn_winfs.c
[`pkgs/windows/seed-stdenv`]: ../../pkgs/windows/seed-stdenv
[`pkgs/windows/stdenv`]: ../../pkgs/windows/stdenv
[`src/Nix/Builder/Unpack.hs`]: ../../src/Nix/Builder/Unpack.hs
[`src/Nix/Environment.hs`]: ../../src/Nix/Environment.hs
[`src/Nix/Profile/Generation.hs`]: ../../src/Nix/Profile/Generation.hs
[`src/Nix/Store.hs`]: ../../src/Nix/Store.hs
[`src/Nix/Store/Access.hs`]: ../../src/Nix/Store/Access.hs
[`src/Nix/Store/CaseSensitive.hs`]: ../../src/Nix/Store/CaseSensitive.hs
[`src/Nix/Store/ExecBit.hs`]: ../../src/Nix/Store/ExecBit.hs
[`src/Nix/Store/Metadata.hs`]: ../../src/Nix/Store/Metadata.hs
[`src/Nix/Store/Symlink.hs`]: ../../src/Nix/Store/Symlink.hs
[`src/Nix/Store/Win32Path.hs`]: ../../src/Nix/Store/Win32Path.hs
