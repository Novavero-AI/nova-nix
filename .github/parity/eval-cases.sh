#!/usr/bin/env bash
# Diff nova-nix's evaluation against upstream Nix's, one expression at a time.
#
# The drvPath oracle beside this proves the two agree on one large real
# closure.  What it cannot do is say WHERE a disagreement lives, and it only
# exercises whatever nixpkgs' hello happens to use.  This runs a small set of
# expressions through both evaluators and names the ones that differ, so a
# divergence arrives as the expression that demonstrates it.
#
# NOVA_NIX_BIN names the executable under test; nix-instantiate comes from
# PATH.  Portable to bash 3.2, which is what macOS ships: no mapfile, no
# associative arrays.
#
# No set -e: a mismatch is the output, not a reason to stop before the rest
# of the set has run.
set -uo pipefail

novaBin=${NOVA_NIX_BIN:?set NOVA_NIX_BIN to the nova-nix executable}
if ! command -v nix-instantiate >/dev/null 2>&1; then
  echo "nix-instantiate is not on PATH; this script needs a real Nix to diff against" >&2
  exit 1
fi

# A tree, because path literals are only interesting relative to something.
# Both evaluators run with this as the working directory, so an absolute
# path in one output is the same absolute path in the other.
fixture=$(mktemp -d)
trap 'rm -rf "$fixture"' EXIT
mkdir -p "$fixture/sub"
printf 'outer\n' >"$fixture/data.txt"
printf 'inner\n' >"$fixture/sub/data.txt"
printf 'q\n' >"$fixture/sub/useq.nix"
printf './data.txt\n' >"$fixture/sub/inner.nix"
printf '{ p = ./data.txt; }\n' >"$fixture/sub/attrs.nix"
printf 'p: p\n' >"$fixture/sub/id.nix"
printf 'builtins.readFile q\n' >"$fixture/sub/readq.nix"

# Upstream spells an unforced thunk <CODE>, and newer releases spell it with
# guillemets; this evaluator spells it <thunk>.  The spelling is not what is
# under test, so all three become one word.  The guillemet form is built with
# escapes to keep this file ASCII, as the tree requires.
guillemetThunk=$(printf '\302\253thunk\302\273')
normalise() {
  sed -e 's/<CODE>/<thunk>/g' -e "s/${guillemetThunk}/<thunk>/g"
}

checked=0
failures=0

check() {
  label=$1
  expr=$2
  checked=$((checked + 1))
  upstream=$(cd "$fixture" && nix-instantiate --eval -E "$expr" 2>&1 | normalise)
  nova=$(cd "$fixture" && "$novaBin" eval --expr "$expr" 2>&1 | normalise)
  if [ "$upstream" = "$nova" ]; then
    printf '  ok    %s\n' "$label"
  else
    failures=$((failures + 1))
    printf '  DIFF  %s\n' "$label"
    printf '          expression  %s\n' "$expr"
    printf '          upstream    %s\n' "$upstream"
    printf '          nova-nix    %s\n' "$nova"
  fi
}

echo "fixture: $fixture"
echo "upstream: $(nix-instantiate --version 2>&1 | head -n1)"
echo

echo "== where a path literal resolves =="
check "literal in an imported file" 'import ./sub/inner.nix'
check "literal deferred in an imported attrset" '(import ./sub/attrs.nix).p'
check "literal passed into an import" '(import ./sub/id.nix) ./data.txt'
check "literal forced inside scopedImport" 'builtins.scopedImport { q = ./data.txt; } ./sub/useq.nix'
check "literal read through scopedImport" 'builtins.scopedImport { q = ./data.txt; } ./sub/readq.nix'
check "literal against the working directory" './data.txt'
echo

echo "== what a value shows before it is forced =="
check "int literals in a list" '[ 1 2 3 ]'
check "float literals in a list" '[ 1.5 2.5 ]'
check "string literals in a list" '[ "a" "b" ]'
check "bool and null in a list" '[ true false null ]'
check "path literal in a list" '[ ./data.txt ]'
check "a nested list" '[ [ 1 2 ] ]'
check "an empty list as an element" '[ [ ] ]'
check "attrset values" '{ a = 1; b = "s"; }'
check "map over a list" 'builtins.map (x: x * x) [ 1 2 3 ]'
echo

printf '%d checked, %d differing\n' "$checked" "$failures"
[ "$failures" -eq 0 ]
