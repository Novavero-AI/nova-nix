#!/usr/bin/env bash
# Diff nova-nix's evaluation against upstream Nix's, one expression at a time.
#
# The drvPath cases beside this prove the two agree on whole closures.  What
# they cannot do is say WHERE inside one a disagreement lives.  This runs a
# small set of expressions through both evaluators and names the ones that
# differ, so a divergence arrives as the expression that demonstrates it.
#
# NOVA_NIX_BIN names the executable under test; nix-instantiate comes from
# PATH.  Portable to bash 3.2, which is what macOS ships: no mapfile, no
# associative arrays.
#
# No set -e: a mismatch is the output, not a reason to stop before the rest
# of the set has run.
set -uo pipefail

novaBin=${NOVA_NIX_BIN:?set NOVA_NIX_BIN to the nova-nix executable}
repoRoot=$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)
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
# Indented strings whose whitespace upstream strips (#220).  Tabs and
# newlines are printf escapes so each fixture's exact bytes are visible.
printf "''\n\n        ''\n" >"$fixture/ind-blank-then-spaces.nix"
printf "''\n   ''\n" >"$fixture/ind-spaces-only.nix"
printf "''   ''\n" >"$fixture/ind-spaces-same-line.nix"
printf "''\n  a\n\n    ''\n" >"$fixture/ind-closer-spaces.nix"
printf "''\n  \${\"v\"}\n   ''\n" >"$fixture/ind-closer-after-interp.nix"
printf "''\n\t\ta\n\t\tb\n''\n" >"$fixture/ind-tabs.nix"
printf "''   \n  a\n''\n" >"$fixture/ind-opener-spaces.nix"
# A string holding a byte that is not UTF-8, and a name past ASCII.
printf 'a\374b' >"$fixture/latin1.txt"
printf '\303\251' >"$fixture/e-acute.txt"
cp "$repoRoot/pkgs/windows/fetchurl.nix" "$fixture/fetchurl.nix"
cp "$repoRoot/.github/parity/fetchurl-cases.nix" "$fixture/fetchurl-cases.nix"

# Outputs are compared byte for byte, markers included: this evaluator
# prints an unforced element and a function with the markers upstream's
# printAmbiguous uses (<CODE>, <LAMBDA>, <PRIMOP>, <PRIMOP-APP>).
checked=0
failures=0

check() {
  label=$1
  expr=$2
  checked=$((checked + 1))
  upstream=$(cd "$fixture" && nix-instantiate --eval -I "nix=$repoRoot/data/nix" -E "$expr" 2>&1)
  upstreamStatus=$?
  nova=$(cd "$fixture" && "$novaBin" eval --nix-path "nix=$repoRoot/data/nix" --expr "$expr" 2>&1)
  novaStatus=$?
  if [ "$upstreamStatus" -eq 0 ] && [ "$novaStatus" -eq 0 ] && [ "$upstream" = "$nova" ]; then
    printf '  ok    %s\n' "$label"
  else
    failures=$((failures + 1))
    printf '  DIFF  %s\n' "$label"
    printf '          expression  %s\n' "$expr"
    printf '          upstream    (exit %s) %s\n' "$upstreamStatus" "$upstream"
    printf '          nova-nix    (exit %s) %s\n' "$novaStatus" "$nova"
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

echo "== what a string and an attribute name print as =="
check "a byte that is not UTF-8" 'builtins.readFile ./latin1.txt'
# The ${ is Nix's, escaped in the string under test, not the shell's.
# shellcheck disable=SC2016
check "the characters a string escapes" '"\" \\ \n \r \t \${ $"'
check "names the lexer cannot read bare" '{ "a b" = 1; "if" = 2; "" = 3; "1x" = 4; or = 5; _x = 6; }'
check "a name past ASCII" 'builtins.listToAttrs [ { name = builtins.readFile ./e-acute.txt; value = 1; } ]'
echo

echo "== what a float and a function print as =="
check "floats in a default ostream's six-digit form" '[ 1.0 1.5 0.1 1.0e20 1.23456789 1234567.0 0.00001 ]'
check "a negative zero" 'builtins.fromJSON "-0.0"'
check "a lambda" 'x: x'
check "a primop" 'builtins.map'
check "a partially applied primop" 'builtins.map (x: x)'
echo

echo "== whitespace an indented string strips =="
check "blank line then a spaces-only line" 'import ./ind-blank-then-spaces.nix'
check "spaces-only line" 'import ./ind-spaces-only.nix'
check "spaces between opener and closer" 'import ./ind-spaces-same-line.nix'
check "closing indentation after a blank line" 'import ./ind-closer-spaces.nix'
check "closing indentation after an interpolation" 'import ./ind-closer-after-interp.nix'
check "tabs are content" 'import ./ind-tabs.nix'
check "spaces before the opener's newline" 'import ./ind-opener-spaces.nix'
echo

echo "== what builtins.toXML writes =="
check "nesting and the escapes of a value" 'builtins.toXML { s = "a<b>&\"q\n\tz"; l = [ 1 2.5 null true [ ] { } ]; }'
check "argument patterns and primops" 'builtins.toXML [ (x: x) (args@{ z, ... }: 1) ({ b, a ? 1 }: a) ({ ... }: 1) builtins.map (builtins.map (x: x)) builtins.derivation ]'
check "a derivation in full once, then repeated" 'let d = derivation { name = "x"; system = "x86_64-linux"; builder = "/bin/sh"; outputs = [ "out" "dev" ]; }; in builtins.toXML { inherit d; again = d; dev = [ d.dev ]; }'
# The ${ is Nix's interpolation, not the shell's.
# shellcheck disable=SC2016
check "the contexts of the strings it writes" 'let d = derivation { name = "x"; system = "x86_64-linux"; builder = "/bin/sh"; }; in builtins.getContext (builtins.toXML { s = "${d}"; inherit d; })'
echo

echo "== Nova's mirror interface preserves fixed-output identity =="
for caseName in legacy sha256 sri order invalidUrls invalidHashes escapedUrl unicodeUrl; do
  check "fetchurl $caseName" "(import ./fetchurl-cases.nix).$caseName"
done
echo

printf '%d checked, %d differing\n' "$checked" "$failures"
[ "$failures" -eq 0 ]
