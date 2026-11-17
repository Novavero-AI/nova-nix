#!/usr/bin/env bash
# What assemble-changelog.sh prints for a history of fragments added,
# renamed and removed, one repository per case.  A fragment it drops is a
# change missing from the release notes, so the cases run in CI before a
# release can need them.
#
# Portable to bash 3.2, which is what macOS ships.
#
# No set -e: a mismatch is the output, not a reason to stop before the rest
# of the cases have run.
set -uo pipefail

script=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/assemble-changelog.sh
scratch=$(mktemp -d)
trap 'rm -rf "$scratch"' EXIT
checked=0
failures=0

# A commit in the case's repository, with an identity of its own so that
# no user configuration is read.
commit() {
  git -c user.name=cases -c user.email=cases@invalid -c commit.gpgsign=false \
    commit -q -m "$1"
}

# fresh NAME: a new repository at $scratch/NAME, the working directory.
fresh() {
  mkdir "$scratch/$1" && cd "$scratch/$1" && git init -q && mkdir changelog.d
}

# add FILE TEXT: commit a fragment holding TEXT.
add() {
  printf -- '- **%s**\n' "$2" >"changelog.d/$1" && git add "changelog.d/$1" && commit "add $1"
}

# report LABEL WANT: compare the script's output and status with WANT.
report() {
  checked=$((checked + 1))
  got=$(bash "$script" 2>&1; echo "exit $?")
  if [ "$got" = "$2" ]; then
    printf 'ok    %s\n' "$1"
  else
    printf 'FAIL  %s\n  expected: %s\n  got:      %s\n' "$1" "$2" "$got"
    failures=$((failures + 1))
  fi
}

lines() { printf '%s\n' "$@"; }

fresh plain
add first.md first
add second.md second
report "fragments print in the order they were added" "$(lines '- **first**' '- **second**' 'exit 0')"

fresh renamed
add early.md early
add later.md later
git mv changelog.d/early.md changelog.d/renamed.md && commit "rename early.md"
report "a renamed fragment prints, at its rename" "$(lines '- **later**' '- **early**' 'exit 0')"

fresh readded
add kept.md kept
add twice.md twice
git rm -q changelog.d/twice.md && commit "remove twice.md"
add twice.md "twice again"
report "a fragment removed and added again prints once" "$(lines '- **kept**' '- **twice again**' 'exit 0')"

fresh removed
add gone.md gone
add stays.md stays
git rm -q changelog.d/gone.md && commit "remove gone.md"
report "a removed fragment does not print" "$(lines '- **stays**' 'exit 0')"

fresh uncommitted
add committed.md committed
printf -- '- **loose**\n' >changelog.d/loose.md
report "an uncommitted fragment fails the count" "$(lines \
  'git names 1 fragment(s) but 2 exist on disk.' \
  'A shallow clone cannot see when a fragment was added: fetch full history' \
  '(git fetch --unshallow) and run this again.  An uncommitted fragment is' \
  'invisible here too.' \
  'exit 1')"

printf '%d cases, %d failed\n' "$checked" "$failures"
[ "$failures" -eq 0 ]
