#!/usr/bin/env bash
# Diff what a program can observe of `derivation` under nova-nix against
# upstream Nix, over a probe corpus (test/data/derivation-probes.txt unless
# one is named; its header describes the rows) and the rows known to differ
# (test/data/derivation-divergences.txt unless a second file is named).
#
# Every probe is evaluated for every argument, its value rendered by
# toJSON.  Both evaluators must print the same value, or both must fail.
# Where either fails with the message of a `throw` written in the row, both
# must fail with that message: which value is forced first is the
# wrapper's to decide, while nova-nix does not word every other error as
# upstream does.  An imported expression must be refused by both, naming
# the same attribute.  A row the divergence list names must differ, and
# every other row must agree.
#
# NOVA_NIX_BIN names the executable under test; nix-instantiate comes from
# PATH.  Portable to bash 3.2, which is what macOS ships: no mapfile, no
# associative arrays.
#
# No set -e: a mismatch is the output, not a reason to stop before the rest
# of the corpus has run.
set -uo pipefail

novaBin=${NOVA_NIX_BIN:?set NOVA_NIX_BIN to the nova-nix executable}
repoRoot=$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)
corpus=${1:-$repoRoot/test/data/derivation-probes.txt}
divergences=${2:-$repoRoot/test/data/derivation-divergences.txt}
for file in "$corpus" "$divergences"; do
  if [ ! -r "$file" ]; then
    echo "cannot read $file" >&2
    exit 2
  fi
done
if ! command -v nix-instantiate >/dev/null 2>&1; then
  echo "nix-instantiate is not on PATH; this script needs a real Nix to diff against" >&2
  exit 1
fi

# nova-nix gets a store of its own, as in drvpath-cases.sh; store paths keep
# the logical /nix/store prefix either way.
scratch=$(mktemp -d) || exit 1
trap 'rm -rf "$scratch"' EXIT
mkdir "$scratch/work"

# The corpus by kind, one `id source` line per row, in the order it lists
# them, and the listed rows as `issue key` lines, the key being an argument
# and a probe id, or `imported` and an id.
grep '^argument ' "$corpus" | sed 's/^argument //' >"$scratch/arguments"
grep '^probe ' "$corpus" | sed 's/^probe //' >"$scratch/probes"
grep '^imported ' "$corpus" | sed 's/^imported //' >"$scratch/imported"
grep -v -e '^#' -e '^$' "$divergences" >"$scratch/listed"
if [ ! -s "$scratch/arguments" ] || [ ! -s "$scratch/probes" ]; then
  echo "$corpus lists no argument or no probe, so nothing would be compared" >&2
  exit 2
fi
: >"$scratch/seen"

# The message of an evaluator's error: its last line that starts with
# `error: `, which is followed by a source excerpt when the error has a
# position, or its last line when none does.
errorLine() {
  message=$(printf '%s\n' "$1" | sed 's/^ *//' | grep '^error: ' | tail -n1)
  if [ -n "$message" ]; then
    printf '%s' "$message"
  else
    printf '%s\n' "$1" | tail -n1 | sed 's/^ *//'
  fi
}

# Evaluate one expression with both, from the same directory.  Sets the
# output (stdout and stderr), status and error message of each.
evaluate() {
  upstream=$(cd "$scratch/work" && nix-instantiate --eval --strict -E "$1" 2>&1)
  upstreamStatus=$?
  nova=$(cd "$scratch/work" && "$novaBin" eval --strict --store "$scratch/store" --expr "$1" 2>&1)
  novaStatus=$?
  upstreamError=$(errorLine "$upstream")
  novaError=$(errorLine "$nova")
}

# Whether an error message is the one a throw of the row raises.
raisedByThrow() {
  message=$1
  row=$2
  printf '%s\n' "$row" | grep -o 'throw "[^"]*"' | sed 's/^throw "//; s/"$//' >"$scratch/messages"
  while IFS= read -r thrown; do
    [ "$message" = "error: $thrown" ] && return 0
  done <"$scratch/messages"
  return 1
}

# Whether the two agree on the probe row just evaluated.  Two failures
# agree unless one of them is a throw's, whose message both must then give.
agrees() {
  row=$1
  if [ "$upstreamStatus" -eq 0 ] && [ "$novaStatus" -eq 0 ]; then
    [ "$upstream" = "$nova" ]
  elif [ "$upstreamStatus" -eq 0 ] || [ "$novaStatus" -eq 0 ]; then
    false
  elif raisedByThrow "$upstreamError" "$row" || raisedByThrow "$novaError" "$row"; then
    [ "$upstreamError" = "$novaError" ]
  else
    true
  fi
}

# The attribute an error names: the last quoted name in its message.
namedAttribute() {
  printf '%s\n' "$1" | sed -n "s/.*'\([^']*\)'.*/\1/p"
}

# The issue the divergence list gives a row, or nothing.
listedIssue() {
  while IFS=' ' read -r issue key; do
    if [ "$key" = "$1" ]; then
      printf '%s' "$issue"
      return
    fi
  done <"$scratch/listed"
}

# One side of a row: the value, or the message of a failure.
shown() {
  if [ "$1" -eq 0 ]; then printf '%s' "$2"; else printf '%s' "$3"; fi
}

checked=0
known=0
failures=0

# Count a row as agreeing or not, against what the list says of it, and
# print it when the two disagree with the list.  Arguments: the key, the
# label printed under a heading, and whether the row agreed (0 or 1).
judge() {
  key=$1
  label=$2
  agreed=$3
  issue=$(listedIssue "$key")
  printf '%s\n' "$key" >>"$scratch/seen"
  checked=$((checked + 1))
  if [ "$agreed" -eq 0 ] && [ -z "$issue" ]; then
    return 0
  elif [ "$agreed" -ne 0 ] && [ -n "$issue" ]; then
    known=$((known + 1))
    return 0
  fi
  failures=$((failures + 1))
  if [ -n "$issue" ]; then
    printf '  AGREES  %s, listed under %s\n' "$label" "$issue"
  else
    printf '  DIFF  %s\n' "$label"
  fi
  return 1
}

echo "upstream: $(nix-instantiate --version 2>&1 | head -n1)"
echo "nova-nix: $("$novaBin" --version 2>&1 | head -n1)"
echo "corpus:   $corpus"
echo "known:    $divergences"
echo

echo "== probes of each argument =="
while IFS=' ' read -r argumentId argument <&3; do
  before=$failures
  knownBefore=$known
  while IFS=' ' read -r probeId probe <&4; do
    evaluate "let w = derivation; args = $argument; d = w args; in builtins.toJSON ($probe)"
    agrees "$argument $probe"
    agreed=$?
    if ! judge "$argumentId $probeId" "$argumentId $probeId" "$agreed"; then
      printf '          argument  %s\n' "$argument"
      printf '          probe     %s\n' "$probe"
      printf '          upstream  (exit %s) %s\n' "$upstreamStatus" "$(shown "$upstreamStatus" "$upstream" "$upstreamError")"
      printf '          nova-nix  (exit %s) %s\n' "$novaStatus" "$(shown "$novaStatus" "$nova" "$novaError")"
    fi
  done 4<"$scratch/probes"
  if [ "$failures" -eq "$before" ]; then
    printf '  ok    %s (%d known)\n' "$argumentId" $((known - knownBefore))
  fi
done 3<"$scratch/arguments"
echo

echo "== imported expressions =="
while IFS=' ' read -r expressionId expression <&3; do
  printf '%s\n' "$expression" >"$scratch/work/imported.nix"
  evaluate 'import ./imported.nix'
  upstreamName=$(namedAttribute "$upstreamError")
  novaName=$(namedAttribute "$novaError")
  [ "$upstreamStatus" -ne 0 ] && [ "$novaStatus" -ne 0 ] && [ -n "$upstreamName" ] && [ "$upstreamName" = "$novaName" ]
  agreed=$?
  judge "imported $expressionId" "imported $expressionId" "$agreed"
  printf '  %s  %s\n' "$expressionId" "$expression"
  printf '          upstream  (exit %s) %s\n' "$upstreamStatus" "$upstreamError"
  printf '          nova-nix  (exit %s) %s\n' "$novaStatus" "$novaError"
done 3<"$scratch/imported"
echo

# A listed row the corpus does not have can never be checked.
while IFS=' ' read -r issue key <&3; do
  if ! grep -qxF "$key" "$scratch/seen"; then
    failures=$((failures + 1))
    printf '  STALE  %s, listed under %s, is not a row of the corpus\n' "$key" "$issue"
  fi
done 3<"$scratch/listed"

printf '%d checked, %d known to differ, %d unexpected\n' "$checked" "$known" "$failures"
[ "$failures" -eq 0 ]
