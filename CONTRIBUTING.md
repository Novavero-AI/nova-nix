# Contributing to nova-nix

nova-nix is a project of Novavero AI Inc. Thanks for your interest - issues
and pull requests are welcome.

## Ground rules

- Keep PRs focused; one change per PR.
- Code must build warning-clean and pass the test suite (`cabal build && cabal test`).
- Match the existing style (ormolu-formatted, hlint-clean).

## Commit messages

- Subject is `Area: summary` - `Eval:`, `Store:`, `Parser:`, `Builder:`,
  `Pkgs:`, `CI:`, `Toolchain:`, and so on.
- The body says why, not what. What changed is in the diff; the message is for
  whoever reads `git log` later asking why a line looks the way it does.

## Changelog

User-visible changes get a bullet in a new file under `changelog.d/`, named
after the change (`changelog.d/fetchgit-pinned-rev.md`): a bold lead sentence,
then the why - the upstream behavior matched, the hazard closed, the reason the
obvious approach does not work - in full sentences.

The bold lead is what someone scanning a release reads. Everything after it is
for whoever needs the reasoning, and it is worth the space. Match the entries
already in `CHANGELOG.md`; they are the specification.

One file per entry rather than one shared section, because every branch
appending to the same section collides there and nowhere else. Two branches
adding entries touch two different files, so they cannot conflict.

`CHANGELOG.md` stays the record of what shipped. At release the fragments are
assembled under the new version heading with `scripts/assemble-changelog.sh`
and removed, so nothing is written twice.

## Licensing of contributions

nova-nix is licensed under Apache-2.0. Per Section 5 of the license, any
contribution intentionally submitted for inclusion in this project is licensed
under Apache-2.0, without additional terms or conditions. You keep the
copyright to your work. Submit only work you have the right to license.

Tooling does not change this. AI-assisted work is fine here, and noting it in
a commit is optional - you are responsible for what you submit either way.
Code is judged on review, and how it was written is not evidence in either
direction.

This is the standard inbound=outbound arrangement: you contribute under the
same terms you received the project under, nothing more.
