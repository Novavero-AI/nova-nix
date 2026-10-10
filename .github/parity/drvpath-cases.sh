#!/usr/bin/env bash
# Compare nova-nix's drvPath against upstream Nix's for every case in a case
# file (drvpath-cases.txt beside this script unless one is named).
#
# A derivation's store path hashes the store path of every input derivation,
# so one matching drvPath validates its whole build-time closure.  Each case
# line is a label and an expression for a derivation; `pkgs` is bound to the
# nixpkgs tree in NIXPKGS for x86_64-linux with no user config or overlays.
# Both evaluators run from the case file's directory, so a relative path in
# an expression names the same file for each.
#
# NOVA_NIX_BIN names the executable under test; nix-instantiate comes from
# PATH.  Portable to bash 3.2, which is what macOS ships: no mapfile, no
# associative arrays, no EPOCHREALTIME (the time keyword measures instead).
#
# No set -e: a mismatch is a row of the table, not a reason to stop before
# the rest of the set has run.
set -uo pipefail

novaBin=${NOVA_NIX_BIN:?set NOVA_NIX_BIN to the nova-nix executable}
nixpkgs=${NIXPKGS:?set NIXPKGS to the nixpkgs tree both evaluators read}
repoRoot=$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)
caseFile=${1:-$repoRoot/.github/parity/drvpath-cases.txt}
if [ ! -r "$caseFile" ]; then
  echo "cannot read the case file $caseFile" >&2
  exit 2
fi
caseDir=$(cd "$(dirname "$caseFile")" && pwd)
if ! command -v nix-instantiate >/dev/null 2>&1; then
  echo "nix-instantiate is not on PATH; this script needs a real Nix to diff against" >&2
  exit 1
fi

# nova-nix writes what evaluation creates (toFile output, a fetched tree)
# into the store it is given, and /nix/store belongs to the Nix daemon, so it
# gets a store of its own; store paths keep the logical /nix/store prefix
# either way.  Upstream writes to its own store, as any evaluation that
# instantiates does: nix-instantiate's --eval alone implies read-only mode,
# which cannot take the closure of a drvPath's deep context (primops.cc at
# 2.24.9), so the oracle would fail where a user's evaluation succeeds.
# Both get a fresh cache directory, so a fetch is a fetch and never a hit
# left behind by an earlier run.
scratch=$(mktemp -d) || exit 1
trap 'rm -rf "$scratch"' EXIT
export XDG_CACHE_HOME="$scratch/cache"

pkgsBinding='pkgs = import <nixpkgs> { system = "x86_64-linux"; config = { }; overlays = [ ]; };'

TIMEFORMAT=%R

# Run one evaluator from the case directory, its stdout and stderr to files,
# and print the wall-clock seconds it took.  Returns the evaluator's status.
timed() {
  outFile=$1
  errFile=$2
  shift 2
  { time (cd "$caseDir" && "$@" </dev/null >"$outFile" 2>"$errFile"); } 2>&1
}

# The stdout of a run, or the last lines of its stderr when it failed.
describe() {
  status=$1
  outFile=$2
  errFile=$3
  if [ "$status" -eq 0 ]; then
    cat "$outFile"
  else
    tail -n 20 "$errFile"
  fi
}

rowFormat='%-18s %9s %9s  %-6s %s\n'
checked=0
failures=0

echo "upstream: $(nix-instantiate --version 2>&1 | head -n1)"
echo "nova-nix: $("$novaBin" --version 2>&1 | head -n1)"
echo "nixpkgs:  $nixpkgs"
echo
# shellcheck disable=SC2059 # the format is the table's, defined once above
printf "$rowFormat" case upstream nova-nix result drvPath

# The || clause keeps a last line that has no newline.
while read -r label expr <&3 || [ -n "$label" ]; do
  case "$label" in
    '' | '#'*) continue ;;
  esac
  if [ -z "$expr" ]; then
    echo "$caseFile: case '$label' has no expression" >&2
    exit 2
  fi
  checked=$((checked + 1))
  wrapped="let $pkgsBinding in ($expr).drvPath"

  upSecs=$(timed "$scratch/up.out" "$scratch/up.err" \
    nix-instantiate --eval --read-write-mode -I "nixpkgs=$nixpkgs" -E "$wrapped")
  upStatus=$?
  novaSecs=$(timed "$scratch/nova.out" "$scratch/nova.err" \
    "$novaBin" eval --store "$scratch/store" \
    --nix-path "nixpkgs=$nixpkgs" --nix-path "nix=$repoRoot/data/nix" \
    --expr "$wrapped")
  novaStatus=$?

  upstream=$(describe "$upStatus" "$scratch/up.out" "$scratch/up.err")
  nova=$(describe "$novaStatus" "$scratch/nova.out" "$scratch/nova.err")
  if [ "$upStatus" -eq 0 ] && [ "$novaStatus" -eq 0 ] && [ "$upstream" = "$nova" ]; then
    # shellcheck disable=SC2059
    printf "$rowFormat" "$label" "${upSecs}s" "${novaSecs}s" ok "$(printf '%s' "$upstream" | tr -d '"')"
  else
    failures=$((failures + 1))
    # shellcheck disable=SC2059
    printf "$rowFormat" "$label" "${upSecs}s" "${novaSecs}s" DIFF "$expr"
    printf '      upstream (exit %s)\n%s\n' "$upStatus" "$(printf '%s\n' "$upstream" | sed 's/^/        /')"
    printf '      nova-nix (exit %s)\n%s\n' "$novaStatus" "$(printf '%s\n' "$nova" | sed 's/^/        /')"
    if [ "${GITHUB_ACTIONS:-}" = true ]; then
      echo "::error title=drvPath mismatch::nova-nix and upstream Nix disagree on case '$label'"
    fi
  fi
done 3<"$caseFile"

echo
printf '%d checked, %d differing\n' "$checked" "$failures"
if [ "$checked" -eq 0 ]; then
  echo "$caseFile holds no cases, so nothing was compared" >&2
  exit 2
fi
[ "$failures" -eq 0 ]
