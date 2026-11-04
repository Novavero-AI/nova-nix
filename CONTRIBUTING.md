# Contributing to nova-nix

nova-nix is a project of Novavero AI Inc. Thanks for your interest. Issues
and pull requests are welcome.

## Ground rules

- Keep PRs focused; one change per PR.
- Code must build warning-clean and pass the test suite. CI runs
  `cabal build --enable-tests --ghc-options="-Werror"`, then the test suite
  (`cabal test` locally).
- Match the existing style: ormolu 0.9.0.0 and hlint 3.10, the versions CI
  pins.
- Source is ASCII only: CI rejects any other byte in a tracked `.hs`, `.c`,
  `.h`, `.nix`, `.sh`, `.cabal` or `.md` file.
- C in `cbits/` compiles under `gcc -std=c99 -Wall -Wextra -pedantic
  -Werror`, and CI also runs the test suite with it built under ASan and
  UBSan.
- The README says what nova-nix is and how to use it, and links to what
  owns each detail rather than repeating it: Haddock for the library,
  `nova-nix --help` for the CLI, the parity case lists for what CI checks.
  A copy drifts from its source; a link cannot.

## Commit messages

- The subject is `Area: summary`, such as `Eval:`, `Store:`, `Parser:`,
  `Builder:`, `Pkgs:`, `CI:` or `Toolchain:`.
- The body says why, not what. What changed is in the diff; the message is for
  whoever reads `git log` later asking why a line looks the way it does.

## Changelog

User-visible changes get a bullet in a new file under `changelog.d/`, named
after the change (`changelog.d/fetchgit-pinned-rev.md`): a bold lead sentence,
then the why (the upstream behavior matched, the hazard closed, the reason the
obvious approach does not work) in full sentences.

The bold lead is what someone scanning a release reads. Everything after it is
for whoever needs the reasoning, and it is worth the space. Match the entries
already in `CHANGELOG.md`; they are the specification. A change to the
library API gets a second bullet in the same file, led by `**Library API:**`,
naming what a library user has to change.

One file per entry rather than one shared section, because every branch
appending to the same section collides there and nowhere else. Two branches
adding entries touch two different files, so they cannot conflict.

`CHANGELOG.md` stays the record of what shipped. At release,
`scripts/assemble-changelog.sh` prints the fragments in the order they were
added, they go under the new version heading, and the fragments are deleted,
so nothing is written twice.

## Licensing of contributions

nova-nix is licensed under Apache-2.0. Per Section 5 of the license, any
contribution intentionally submitted for inclusion in this project is licensed
under Apache-2.0, without additional terms or conditions. You keep the
copyright to your work. Submit only work you have the right to license.

This is the standard inbound=outbound arrangement: you contribute under the
same terms you received the project under, nothing more.

Tooling does not change this. AI-assisted work is fine here, and noting it in
a commit is optional; you are responsible for what you submit either way.
Code is judged on review, and how it was written is not evidence in either
direction.
