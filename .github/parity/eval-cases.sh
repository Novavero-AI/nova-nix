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
# Indented strings whose whitespace upstream strips.  Tabs and
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

# The lines of the input that end in the text given.
endingIn() {
  awk -v suffix="$1" 'length($0) >= length(suffix) && substr($0, length($0) - length(suffix) + 1) == suffix'
}

# An expression both evaluators must refuse, on the same final line.  What
# comes before it is upstream's trace, which nova-nix reproduces only for
# the lines that have no position: given a marker, the line ending in it
# must be in upstream's output and the same in nova-nix's.
refused() {
  label=$1
  expr=$2
  marker=${3-}
  checked=$((checked + 1))
  upstream=$(cd "$fixture" && nix-instantiate --eval -I "nix=$repoRoot/data/nix" -E "$expr" 2>&1)
  upstreamStatus=$?
  nova=$(cd "$fixture" && "$novaBin" eval --nix-path "nix=$repoRoot/data/nix" --expr "$expr" 2>&1)
  novaStatus=$?
  upstreamError=$(printf '%s\n' "$upstream" | tail -n1 | sed 's/^ *//')
  novaError=$(printf '%s\n' "$nova" | tail -n1 | sed 's/^ *//')
  upstreamContext=""
  novaContext=""
  if [ -n "$marker" ]; then
    upstreamContext=$(printf '%s\n' "$upstream" | endingIn " $marker")
    novaContext=$(printf '%s\n' "$nova" | endingIn " $marker")
  fi
  if [ "$upstreamStatus" -ne 0 ] && [ "$novaStatus" -ne 0 ] && [ "$upstreamError" = "$novaError" ] &&
    [ "$upstreamContext" = "$novaContext" ] && { [ -z "$marker" ] || [ -n "$upstreamContext" ]; }; then
    printf '  ok    %s\n' "$label"
  else
    failures=$((failures + 1))
    printf '  DIFF  %s\n' "$label"
    printf '          expression  %s\n' "$expr"
    printf '          upstream    (exit %s) %s\n' "$upstreamStatus" "$upstreamError"
    printf '          nova-nix    (exit %s) %s\n' "$novaStatus" "$novaError"
    if [ -n "$marker" ]; then
      printf '          upstream    context %s\n' "$upstreamContext"
      printf '          nova-nix    context %s\n' "$novaContext"
    fi
  fi
}

# Whether the Nix under comparison is at least the named release, for a
# behaviour upstream changed and nova-nix follows.
upstreamVersion=$(nix-instantiate --version 2>&1 | head -n1 | sed 's/.* //')
upstreamAtLeast() {
  [ "$(printf '%s\n%s\n' "$1" "$upstreamVersion" | sort -V | head -n1)" = "$1" ]
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

echo "== which keys written as a string are names =="
# Each ${ is Nix's, not the shell's.
# shellcheck disable=SC2016
{
  check "a plain string binds in a let" 'let "a" = 1; in a'
  check "an interpolated plain string binds in a let" 'let ${"a"} = 1; in a'
  check "a parenthesized plain string binds in a let" 'let ${("a")} = 1; in a'
  check "a URI binds in a let" 'let ${a:b} = 1; in 2'
  check "an interpolated plain string is inherited" 'let a = 1; in { inherit ${"a"}; }.a'
  check "an interpolated plain string is in a rec set's scope" '(rec { ${"a"} = 1; b = a; }).b'
  check "a path through one merges with the name written bare" 'builtins.attrNames { ${"a"}.b = 1; a.c = 2; }.a'
}
echo

echo "== what a float and a function print as =="
check "floats in a default ostream's six-digit form" '[ 1.0 1.5 0.1 1.0e20 1.23456789 1234567.0 0.00001 ]'
check "a negative zero" 'builtins.fromJSON "-0.0"'
check "a lambda" 'x: x'
check "a primop" 'builtins.map'
check "a partially applied primop" 'builtins.map (x: x)'
echo

echo "== what + makes of a path it coerces =="
check "an outPath left of + is its own text, not a store copy" 'let s = { outPath = ./data.txt; } + "x"; in builtins.toJSON [ s (builtins.getContext s) ]'
echo

echo "== what builtins.path copies =="
# 2.34.0 refuses a directory through another read, in other words.
if ! upstreamAtLeast 2.34.0; then
  refused "a directory, flat" 'builtins.path { path = ./sub; recursive = false; }'
fi
# An attribute fromJSON makes has no position, so the refusal is the last
# line in both.  2.26.0 names builtins.path where 2.24.9 and nova-nix name
# addPath.
if ! upstreamAtLeast 2.26.0; then
  refused "an argument it does not take" 'builtins.path ({ path = ./data.txt; } // builtins.fromJSON "{\"foo\": 1}")'
fi
echo

echo "== arithmetic and comparison as upstream's primops do them =="
check "unary minus is 0 - e, so a float zero negates to positive zero" '[ (-(0.0)) (-(-0.0)) ]'
check "division by a float, and integer division truncating toward zero" '[ (1 / 4.0) (7 / 2) ((-7) / 2) ]'
check "a list comparison passes over equal elements of an unordered type" '[ ([ { } ] < [ { } ]) ([ { } 1 ] < [ { } 2 ]) ]'
echo

echo "== when builtins.addErrorContext forces its message =="
check "not when its value evaluates" 'builtins.addErrorContext (throw "a") 1'
check "a throw inside it stays catchable" '(builtins.tryEval (builtins.addErrorContext "c" (throw "x"))).success'
refused "a throw inside it is the failure, with the message a line of its trace" 'builtins.addErrorContext "ctx-marker" (throw "x")' ctx-marker
refused "a message that throws is the failure in its place" 'builtins.addErrorContext (throw "a") (throw "b")'
refused "a message that does not coerce escapes tryEval" 'builtins.tryEval (builtins.addErrorContext 1 (throw "x"))'
echo

echo "== what fromJSON reads =="
check "every escape JSON defines" 'builtins.toJSON (builtins.fromJSON "\"\\\"\\\\\\/\\b\\f\\n\\r\\t\\u00e9\\ud83d\\ude00\"")'
check "numbers at the Nix integer bounds" 'builtins.fromJSON "[ -0, 1E2, -9223372036854775808, 9223372036854775807, 18446744073709551616, 1.5e-3 ]"'
# 2.24.9 wraps these to negative ints.
if upstreamAtLeast 2.25.0; then
  refused "an unsigned number past the Nix integer range" 'builtins.fromJSON "18446744073709551615"'
  refused "the first such number in the document, not in key order" 'builtins.fromJSON "{ \"b\": 18446744073709551615, \"a\": 9223372036854775808 }"'
  refused "such a number ahead of a later syntax error" 'builtins.fromJSON "[ 18446744073709551615, nonsense"'
fi
echo

echo "== what a builtin reading a plain string makes of string context =="
withContext='builtins.appendContext "a" { "/nix/store/00000000000000000000000000000000-a" = { path = true; }; }'
# 2.34.0 names the refused context element by its full store path, where
# 2.24.9 and nova-nix name it by its base name.
if ! upstreamAtLeast 2.34.0; then
  refused "a regex with context" "builtins.match ($withContext) \"a\""
  refused "a toFile name with context" "builtins.toFile ($withContext) \"x\""
  refused "a second version with context" "builtins.compareVersions \"1\" ($withContext)"
fi
check "split gives back a string its regex does not match, context and all" "builtins.hasContext (builtins.head (builtins.split \"x\" ($withContext)))"
check "the pieces of a string split matches carry no context" "builtins.toJSON (builtins.map builtins.hasContext (builtins.filter builtins.isString (builtins.split \"a\" ($withContext))))"
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

echo "== what makes a derivation fixed-output, and what its hash reads as =="
# An outputHash makes a derivation fixed-output whatever it holds; an empty
# one is the all-zero hash of outputHashAlgo's algorithm, with a warning.
check "an empty outputHash and its warning" '(derivation { name = "t"; system = "x86_64-linux"; builder = "/bin/sh"; outputHash = ""; outputHashAlgo = "sha256"; }).drvPath'
check "a null outputHash coerced to the empty one" '(derivation { name = "t"; system = "x86_64-linux"; builder = "/bin/sh"; outputHash = null; outputHashAlgo = "sha256"; }).drvPath'
check "a fixed output whose outputs are out alone" '(derivation { name = "t"; system = "x86_64-linux"; builder = "/bin/sh"; outputs = [ "out" ]; outputHash = "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="; }).drvPath'
refused "an empty outputHash with no algorithm" '(derivation { name = "t"; system = "x86_64-linux"; builder = "/bin/sh"; outputHash = ""; }).drvPath'
refused "an integer outputHash read as its decimal" '(derivation { name = "t"; system = "x86_64-linux"; builder = "/bin/sh"; outputHash = 5; outputHashAlgo = "sha256"; }).drvPath'
refused "an integer outputHash with no algorithm" '(derivation { name = "t"; system = "x86_64-linux"; builder = "/bin/sh"; outputHash = 5; }).drvPath'
refused "a fixed output with a second output" '(derivation { name = "t"; system = "x86_64-linux"; builder = "/bin/sh"; outputs = [ "out" "dev" ]; outputHash = "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="; }).drvPath'
refused "a fixed output whose one output is not out" '(derivation { name = "t"; system = "x86_64-linux"; builder = "/bin/sh"; outputs = [ "dev" ]; outputHash = "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="; }).drvPath'
echo

echo "== what an outputHashMode names =="
# nar and recursive hash a NAR and flat the bytes.  The mode is read for
# every derivation, so one with no outputHash refuses a mode too.
check "nar, recursive and flat" 'builtins.toJSON (map (mode: (derivation { name = "t"; system = "x86_64-linux"; builder = "/bin/sh"; outputHashMode = mode; outputHash = "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="; }).outPath) [ "nar" "recursive" "flat" ])'
refused "an unknown mode with no outputHash" '(derivation { name = "t"; system = "x86_64-linux"; builder = "/bin/sh"; outputHashMode = "foo"; }).drvPath'
refused "git, behind its experimental feature" '(derivation { name = "t"; system = "x86_64-linux"; builder = "/bin/sh"; outputHashMode = "git"; }).drvPath'
# 2.33.0 adds the derivation and its mode to this refusal.
if ! upstreamAtLeast 2.33.0; then
  refused "text, behind its experimental feature" '(derivation { name = "t"; system = "x86_64-linux"; builder = "/bin/sh"; outputHashMode = "text"; }).drvPath'
fi
echo

echo "== Nova's mirror interface preserves fixed-output identity =="
for caseName in legacy sha256 sri order invalidUrls invalidHashes escapedUrl unicodeUrl; do
  check "fetchurl $caseName" "(import ./fetchurl-cases.nix).$caseName"
done
echo

printf '%d checked, %d differing\n' "$checked" "$failures"
[ "$failures" -eq 0 ]
