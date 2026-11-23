#!/usr/bin/env bash
# The mode ladder-mode.sh prints for each event and ref, and for a pull
# request's lists of changed files, one case per rule and per edge.  Ladder
# (mode) runs these before it lets the script decide a run's mode, since a
# wrong skip is a run that tests nothing and stays green.
#
# Portable to bash 3.2, which is what macOS ships.
#
# No set -e: a mismatch is the output, not a reason to stop before the rest
# of the cases have run.
set -uo pipefail

script=$(dirname "${BASH_SOURCE[0]}")/ladder-mode.sh
checked=0
failures=0

readonly MAIN=refs/heads/main
readonly PULL=refs/pull/1/merge

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

# expect WANT EVENT REF [PATH ...]: the mode for an event on a ref with these
# changed paths, each ended by a NUL.  The label quotes each path as the
# shell reads it, so a space or a newline inside one shows.
expect() {
  local want=$1 event=$2 ref=$3 got label
  shift 3
  label="$event $ref"
  if [ $# -gt 0 ]; then label="$label $(printf '%q ' "$@")"; fi
  got=$(if [ $# -gt 0 ]; then printf '%s\0' "$@"; fi | bash "$script" "$event" "$ref" 2> /dev/null)
  report "$want" "$got" "${label% }"
}

# A pull request that changes documentation alone.
expect skip pull_request "$PULL" README.md
expect skip pull_request "$PULL" CHANGELOG.md CONTRIBUTING.md
expect skip pull_request "$PULL" changelog.d/seed-stdenv.md
expect skip pull_request "$PULL" LICENSE NOTICE
expect skip pull_request "$PULL" 'READ ME.md'

# A pull request that changes anything else takes the rungs from the cache,
# the builder's sources included.
expect substitute pull_request "$PULL" src/Nix/Builder.hs
expect substitute pull_request "$PULL" cbits/nn_arena.c
expect substitute pull_request "$PULL" app/Main.hs
expect substitute pull_request "$PULL" cli/Nix/Cli/Args.hs
expect substitute pull_request "$PULL" data/nix/fetchurl.nix
expect substitute pull_request "$PULL" nova-nix.cabal
expect substitute pull_request "$PULL" cabal.project
expect substitute pull_request "$PULL" 'src/Nix/A Module.hs'
expect substitute pull_request "$PULL" pkgs/windows/by-name/he/hello/package.nix
expect substitute pull_request "$PULL" test/Main.hs
expect substitute pull_request "$PULL" test/data/mes.nix
expect substitute pull_request "$PULL" .github/workflows/ci.yml
expect substitute pull_request "$PULL" .github/workflows/ladder.yml
expect substitute pull_request "$PULL" .github/cache/ladder-mode.sh
expect substitute pull_request "$PULL" docs/cli.json
expect substitute pull_request "$PULL" .gitattributes
expect substitute pull_request "$PULL" scripts/assemble-changelog.sh
expect substitute pull_request "$PULL" 'pkgs/windows/a file.nix'

# One file that is not documentation decides, whatever comes with it.
expect substitute pull_request "$PULL" README.md src/Nix/Eval.hs
expect substitute pull_request "$PULL" README.md pkgs/windows/rungs.nix
expect substitute pull_request "$PULL" changelog.d/seed-stdenv.md nova-nix.cabal

# A name that only resembles a document.
expect substitute pull_request "$PULL" LICENSE.txt

# Markdown is documentation at the root alone.
expect substitute pull_request "$PULL" pkgs/windows/README.md
expect substitute pull_request "$PULL" docs/notes.md
expect substitute pull_request "$PULL" src/README.md
expect substitute pull_request "$PULL" .github/pull_request_template.md

# A rename arrives as both names, and the one it leaves counts.
expect substitute pull_request "$PULL" NOTES.md src/Notes.md
expect substitute pull_request "$PULL" README.md pkgs/windows/README.md
expect skip pull_request "$PULL" README.md OVERVIEW.md

# A pull request with no changed file runs the ladder.
expect substitute pull_request "$PULL"

# A path holding a newline is one path: read by lines, it would pass for two
# documents.
expect substitute pull_request "$PULL" $'README.md\nchangelog.d/name.md'

# A last path that no NUL ends is still read.
report substitute "$(printf 'README.md\0src/Nix/Eval.hs' | bash "$script" pull_request "$PULL" 2> /dev/null)" 'a last path with no NUL'

# A push to main takes the rungs from the cache, whatever it changes, and
# documentation alone does not skip it.
expect substitute push "$MAIN"
expect substitute push "$MAIN" README.md
expect substitute push "$MAIN" src/Nix/Eval.hs

# Every other run builds the ladder from the seed: a release's tag, the
# daily run, a dispatched run on any branch, and an event no rule names.
expect seed push refs/tags/v0.8.0.0
expect seed schedule "$MAIN"
expect seed workflow_dispatch "$MAIN"
expect seed workflow_dispatch refs/heads/some-branch
expect seed repository_dispatch "$MAIN"

# Without an event and a ref the script decides nothing.
report '' "$(bash "$script" 2> /dev/null < /dev/null)" 'no arguments'
report '' "$(bash "$script" pull_request 2> /dev/null < /dev/null)" 'an event without a ref'

echo "$checked cases, $failures failed"
[ "$failures" -eq 0 ]
