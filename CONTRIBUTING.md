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

User-visible changes get a bullet under `## Unreleased` in `CHANGELOG.md`: a
bold lead sentence, then the why - the upstream behavior matched, the hazard
closed, the reason the obvious approach does not work - in full sentences.

The bold lead is what someone scanning a release reads. Everything after it is
for whoever needs the reasoning, and it is worth the space. Match the entries
already in the file; they are the specification.

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
