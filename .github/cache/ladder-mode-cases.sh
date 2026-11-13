#!/usr/bin/env bash
# The mode ladder-mode.sh prints for lists of changed files, one case per
# rule and per edge.  CI runs these before it lets the script decide a run's
# mode, since a wrong skip is a run that tests nothing and stays green.
#
# Portable to bash 3.2, which is what macOS ships.
#
# No set -e: a mismatch is the output, not a reason to stop before the rest
# of the cases have run.
set -uo pipefail

script=$(dirname "${BASH_SOURCE[0]}")/ladder-mode.sh
checked=0
failures=0

# report WANT GOT LABEL
report() {
  checked=$((checked + 1))
  if [ "$2" = "$1" ]; then
    printf 'ok    %-10s %s\n' "$2" "$3"
  else
    printf 'FAIL  %-10s %s (expected %s)\n' "${2:-nothing}" "$3" "$1"
    failures=$((failures + 1))
  fi
}

# expect WANT [PATH ...]: the mode for these paths, each ended by a NUL.
# The label quotes each path as the shell reads it, so a space or a newline
# inside one shows.
expect() {
  local want=$1 got label='(no changed file)'
  shift
  if [ $# -gt 0 ]; then label=$(printf '%q ' "$@"); fi
  got=$(if [ $# -gt 0 ]; then printf '%s\0' "$@"; fi | bash "$script" 2> /dev/null)
  report "$want" "$got" "${label% }"
}

# Documentation alone.
expect skip README.md
expect skip CHANGELOG.md CONTRIBUTING.md
expect skip changelog.d/seed-stdenv.md
expect skip LICENSE NOTICE
expect skip 'READ ME.md'

# Each builder path, alone.
expect seed src/Nix/Builder.hs
expect seed cbits/nn_arena.c
expect seed app/Main.hs
expect seed cli/Nix/Cli/Args.hs
expect seed data/nix/fetchurl.nix
expect seed nova-nix.cabal
expect seed cabal.project
expect seed cabal.project.local
expect seed 'src/Nix/A Module.hs'

# A workflow or one of its scripts is not the builder.
expect substitute .github/workflows/ci.yml
expect substitute .github/cache/ladder-mode.sh

# One builder file decides, whatever comes with it.
expect seed README.md src/Nix/Eval.hs
expect seed pkgs/windows/rungs.nix test/Main.hs cbits/nn_arena.h
expect seed changelog.d/seed-stdenv.md nova-nix.cabal

# The package set, the checks and the rest.
expect substitute pkgs/windows/by-name/he/hello/package.nix
expect substitute test/Main.hs
expect substitute test/data/mes.nix
expect substitute README.md pkgs/windows/rungs.nix
expect substitute docs/cli.json
expect substitute .gitattributes
expect substitute scripts/assemble-changelog.sh
expect substitute 'pkgs/windows/a file.nix'

# A name that only resembles a builder path or a document.
expect substitute test/data/run-command.nix
expect substitute srcs/notes.txt
expect substitute LICENSE.txt

# Markdown is documentation at the root alone.
expect substitute pkgs/windows/README.md
expect substitute docs/notes.md
expect seed src/README.md
expect substitute .github/pull_request_template.md

# A rename arrives as both names, and the one it leaves counts.
expect seed NOTES.md src/Notes.md
expect substitute README.md pkgs/windows/README.md
expect skip README.md OVERVIEW.md

# Nothing changed.
expect seed

# A path holding a newline is one path: read by lines, its second line
# would pass for a builder file.
expect substitute $'pkgs/windows/odd\nsrc/name.nix'

# A last path that no NUL ends is still read.
report seed "$(printf 'README.md\0src/Nix/Eval.hs' | bash "$script" 2> /dev/null)" 'a last path with no NUL'

echo "$checked cases, $failures failed"
[ "$failures" -eq 0 ]
