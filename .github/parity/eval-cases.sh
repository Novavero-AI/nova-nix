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
# nova-nix's store under it holds read-only trees, so the cleanup makes it
# writable before removing it.  What checkValue's evaluators print on
# stderr is kept apart, outside the tree.
fixture=$(mktemp -d)
streams=$(mktemp -d)
trap 'chmod -R u+w "$fixture" 2>/dev/null; rm -rf "$fixture" "$streams"' EXIT
mkdir -p "$fixture/sub"
printf 'outer\n' >"$fixture/data.txt"
printf 'inner\n' >"$fixture/sub/data.txt"
printf 'q\n' >"$fixture/sub/useq.nix"
printf './data.txt\n' >"$fixture/sub/inner.nix"
printf '{ p = ./data.txt; }\n' >"$fixture/sub/attrs.nix"
printf 'p: p\n' >"$fixture/sub/id.nix"
printf 'builtins.readFile q\n' >"$fixture/sub/readq.nix"
# A position names a file: an expression given on the command line has none.
printf 'let s = { a = 1; }; in s // { b = 2; }\n' >"$fixture/layered.nix"
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

# Count a row and print its verdict: ok when both evaluators succeeded and
# printed the same, the expression and each side's exit and output when
# not, returning 1.
verdict() {
  checked=$((checked + 1))
  if [ "$upstreamStatus" -eq 0 ] && [ "$novaStatus" -eq 0 ] && [ "$upstream" = "$nova" ]; then
    printf '  ok    %s\n' "$label"
  else
    failures=$((failures + 1))
    printf '  DIFF  %s\n' "$label"
    printf '          expression  %s\n' "$expr"
    printf '          upstream    (exit %s) %s\n' "$upstreamStatus" "$upstream"
    printf '          nova-nix    (exit %s) %s\n' "$novaStatus" "$nova"
    return 1
  fi
}

# An expression both evaluators must print the same for, stderr included.
check() {
  label=$1
  expr=$2
  upstream=$(cd "$fixture" && nix-instantiate --eval -I "nix=$repoRoot/data/nix" -E "$expr" 2>&1)
  upstreamStatus=$?
  nova=$(cd "$fixture" && "$novaBin" eval ${novaStore:+--store "$novaStore"} --nix-path "nix=$repoRoot/data/nix" --expr "$expr" 2>&1)
  novaStatus=$?
  verdict
}

# An expression both evaluators must give the same value for, compared on
# stdout alone, for a row about the value where upstream also reports its
# own progress on stderr: a fetch that unpacks into its Git cache says so,
# and nova-nix has no such cache.  What each printed on stderr is shown
# when the row differs.
checkValue() {
  label=$1
  expr=$2
  upstream=$(cd "$fixture" && nix-instantiate --eval -I "nix=$repoRoot/data/nix" -E "$expr" 2>"$streams/upstream")
  upstreamStatus=$?
  nova=$(cd "$fixture" && "$novaBin" eval ${novaStore:+--store "$novaStore"} --nix-path "nix=$repoRoot/data/nix" --expr "$expr" 2>"$streams/nova")
  novaStatus=$?
  verdict || {
    printf '          upstream stderr  %s\n' "$(cat "$streams/upstream")"
    printf '          nova-nix stderr  %s\n' "$(cat "$streams/nova")"
  }
}

# The lines of the input that end in the text given.
endingIn() {
  awk -v suffix="$1" 'length($0) >= length(suffix) && substr($0, length($0) - length(suffix) + 1) == suffix'
}

# The final line of an evaluator's output, unindented.
finalLine() {
  printf '%s\n' "$1" | tail -n1 | sed 's/^ *//'
}

# The line of an evaluator's output that states its error: the last that
# starts with `error: ` once unindented.
messageLine() {
  printf '%s\n' "$1" | sed 's/^ *//' | grep '^error: ' | tail -n1
}

# An expression both evaluators must refuse, on the same final line.  What
# comes before it is upstream's trace, which nova-nix reproduces only for
# the lines that have no position: given a marker, the line ending in it
# must be in upstream's output and the same in nova-nix's.
refused() {
  refusedOn finalLine "$@"
}

# An expression both evaluators must refuse with the same message, where
# upstream reports the error at a position: it follows the message with the
# source there and any suggestion, which nova-nix does not print.  A
# marker is checked as refused checks it.
refusedAtPosition() {
  refusedOn messageLine "$@"
}

# refused and refusedAtPosition, the first argument naming the function
# that picks the line the two outputs must agree on.  Upstream runs with
# show-trace off, its default, whatever nix.conf sets: the installer the
# parity workflow uses writes show-trace = true, under which upstream adds
# a line of context for each function call a failure passes through, and
# nova-nix has no show-trace.
refusedOn() {
  pick=$1
  label=$2
  expr=$3
  marker=${4-}
  checked=$((checked + 1))
  upstream=$(cd "$fixture" && nix-instantiate --option show-trace false --eval -I "nix=$repoRoot/data/nix" -E "$expr" 2>&1)
  upstreamStatus=$?
  nova=$(cd "$fixture" && "$novaBin" eval --nix-path "nix=$repoRoot/data/nix" --expr "$expr" 2>&1)
  novaStatus=$?
  upstreamError=$("$pick" "$upstream")
  novaError=$("$pick" "$nova")
  upstreamContext=""
  novaContext=""
  if [ -n "$marker" ]; then
    upstreamContext=$(printf '%s\n' "$upstream" | endingIn " $marker")
    novaContext=$(printf '%s\n' "$nova" | endingIn " $marker")
  fi
  if [ "$upstreamStatus" -ne 0 ] && [ "$novaStatus" -ne 0 ] && [ -n "$upstreamError" ] && [ "$upstreamError" = "$novaError" ] &&
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

# An expression both evaluators must refuse to parse, at the same line and
# column.  The two word a syntax error differently, so only its place is
# compared: upstream prints it as "at <origin>:LINE:COLUMN:" under the
# message (error.cc), nova-nix in its parse error's peLine and peCol.
unparsed() {
  label=$1
  expr=$2
  checked=$((checked + 1))
  upstream=$(cd "$fixture" && nix-instantiate --eval -I "nix=$repoRoot/data/nix" -E "$expr" 2>&1)
  upstreamStatus=$?
  nova=$(cd "$fixture" && "$novaBin" eval --nix-path "nix=$repoRoot/data/nix" --expr "$expr" 2>&1)
  novaStatus=$?
  upstreamAt=$(printf '%s\n' "$upstream" | sed -n 's/^ *at [^ ]*:\([0-9][0-9]*\):\([0-9][0-9]*\):$/\1:\2/p' | head -n1)
  novaAt=$(printf '%s\n' "$nova" | sed -n 's/^parse error: .*peLine = \([0-9][0-9]*\), peCol = \([0-9][0-9]*\),.*/\1:\2/p' | head -n1)
  if [ "$upstreamStatus" -ne 0 ] && [ "$novaStatus" -ne 0 ] && [ -n "$upstreamAt" ] && [ "$upstreamAt" = "$novaAt" ]; then
    printf '  ok    %s\n' "$label"
  else
    failures=$((failures + 1))
    printf '  DIFF  %s\n' "$label"
    printf '          expression  %s\n' "$expr"
    printf '          upstream    (exit %s) at %s: %s\n' "$upstreamStatus" "$upstreamAt" "$(printf '%s\n' "$upstream" | head -n1)"
    printf '          nova-nix    (exit %s) at %s: %s\n' "$novaStatus" "$novaAt" "$(printf '%s\n' "$nova" | head -n1)"
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

echo "== what baseNameOf makes of a trailing slash =="
check "baseNameOf drops one trailing slash, not every one" 'let v = [ (baseNameOf "a/") (baseNameOf "a//") (baseNameOf "/") (baseNameOf "/a/") ]; in builtins.deepSeq v v'
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

echo "== which sets print as repeated =="
check "an update with an empty operand is the other operand" 'let a = { x = 1; }; l = [ a (a // { }) ({ } // a) ]; in builtins.deepSeq l l'
check "a layered set printed twice and beside the set under it" 'let b = { x = 1; y = 2; }; l = b // { z = 3; }; v = [ l l b ]; in builtins.deepSeq v v'
echo

# 2.34 layers a right operand of at most 16 attributes on the left, eight
# sets to a chain, and 2.24 copies both: a read of the result answers the
# same either way.  Eight updates on one set layer seven times, then merge.
echo "== what // makes of a small right operand =="
chain='let s = { a = 0; b = 0; c = 0; }; l = ((((((((s // { a = 1; }) // { d = 1; }) // { b = 2; }) // { e = 1; }) // { a = 3; }) // { f = 1; }) // { c = 4; g = 1; }) // { d = 5; }); in'
check "a chain of eight updates prints as one set" "$chain builtins.deepSeq l l"
check "a chain of eight updates names its attributes" "$chain builtins.attrNames l"
check "a chain of eight updates lists its values" "$chain builtins.attrValues l"
check "an attribute keeps its position, from the set under a layer and from the layer" 'let l = import ./layered.nix; p = n: let q = builtins.unsafeGetAttrPos n l; in [ q.line q.column ]; v = [ (p "a") (p "b") ]; in builtins.deepSeq v v'
check "a layered set equals the set it reads as" 'let s = { a = 1; b = 2; }; v = [ (s // { c = 3; } == { a = 1; b = 2; c = 3; }) (s // { b = 3; } == { a = 1; b = 3; }) (s // { b = 3; } == s) ]; in builtins.deepSeq v v'
check "builtins that read a layered set" 'let l = { a = 1; b = 2; } // { b = 3; c = 4; }; v = [ (builtins.removeAttrs l [ "b" ]) (builtins.intersectAttrs l { a = 0; c = 0; z = 0; }) (builtins.intersectAttrs { b = 0; } l) (builtins.mapAttrs (n: x: x * 10) l) (builtins.listToAttrs (builtins.attrValues (builtins.mapAttrs (name: value: { inherit name value; }) l))) ]; in builtins.deepSeq v v'
check "with a layered set" 'let a = { x = 1; y = 2; }; v = with (a // { y = 3; }); [ x y ]; in builtins.deepSeq v v'
refusedAtPosition "an unexpected argument a layered set holds" '({ a }: a) ({ a = 1; } // { zz = 2; })' "from call site"
refused "deepSeq forces the set under a layer by its names" 'let b = { za = throw "base"; }; l = b // { zb = throw "layer"; }; in builtins.deepSeq l null'
refused "deepSeq forces a layer by its names" 'let early = { zg = 0; }; b = { zh = throw "base"; }; l = b // { zg = throw "layer"; }; in builtins.deepSeq l null'
check "a chain of updates reads the same however it nests" 'let a = { x = 1; y = 1; }; v = [ (a // { y = 2; } // { z = 3; } // { x = 4; }) (((a // { y = 2; }) // { z = 3; }) // { x = 4; }) ]; in builtins.deepSeq v v'
# 2.24.9 evaluates a chain's operands first to last, and 2.34.9 and later
# last to first (ExprOpUpdate::evalForUpdate in eval.cc), as nova-nix
# does.  Each of these holds one operand that fails, so every release
# agrees; the two after them hold two, and run from 2.34.0.
refused "a chain of updates fails at its first operand" 'throw "first" // { a = 1; } // { b = 2; }'
refused "a chain of updates fails at a middle operand" '{ a = 1; } // throw "middle" // { b = 2; }'
refused "a chain of updates fails at its last operand" '{ a = 1; } // { b = 2; } // throw "last"'
refused "a chain of updates refuses an operand that is not a set" '{ a = 1; } // 1 // { b = 2; }'
if upstreamAtLeast 2.34.0; then
  refused "a chain of updates fails at the last of two operands that fail" 'throw "first" // { a = 1; } // throw "last"'
  refused "an update of two operands that are not sets refuses the right" '1 // "x"'
fi
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

echo "== which keywords are attribute names =="
check "true, false and null, which upstream reads as identifiers" 'let s = { true = 1; false = 2; null = 3; }; in s.true + s.false + s.null'
unparsed "if as a binding's name" '{ if = 1; }'
unparsed "then after a dot" 'x.then'
unparsed "else in a has-attribute path" 'x ? else'
unparsed "rec as an inherited name" '{ inherit rec; }'
unparsed "inherit in an attribute path" '{ a.inherit = 1; }'
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
# nix-instantiate --eval runs read-only (main_nix_instantiate in
# nix-instantiate.cc), so it computes a copy's path without writing it
# (FetchMode::DryRun in primops.cc's addPath) and a toFile result's without
# adding the text (prim_toFile); nova-nix's eval computes them the same
# way, writing nothing to the store, which upstream manages here.
check "a file, by its NAR" 'builtins.path { path = ./data.txt; }'
check "a file, flat" 'builtins.path { path = ./data.txt; recursive = false; }'
check "a directory, by its NAR" 'builtins.path { path = ./sub; }'
check "a directory, filtered" 'builtins.path { path = ./sub; filter = p: t: baseNameOf p != "useq.nix"; }'
check "a toFile result" 'builtins.toFile "parity-text" "some text\n"'
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

echo "== what a path inside a store path coerces to =="
# A path value is copied to the store under its base name wherever it is
# (copyPathToStore in eval.cc): one inside a store path too, and a store path
# itself under its <hash>-<name> base name.  The tree is added as a fetch or a
# channel leaves one, and drv.nix is a derivation in a file imported from it,
# as nixpkgs' are once nixpkgs is fetched; read-only, both evaluators compute
# each copy's path without writing it.
mkdir -p "$fixture/in-store"
printf 'in the store\n' >"$fixture/in-store/data.txt"
printf "\"\${./data.txt}\"\n" >"$fixture/in-store/use.nix"
printf 'derivation { name = "parity-in-store"; system = "x86_64-linux"; builder = "/bin/sh"; args = [ ./data.txt ]; src = ./.; }\n' >"$fixture/in-store/drv.nix"
ln -s data.txt "$fixture/in-store/link"
mkdir -p "$fixture/in-store/sub"
printf 'below a link\n' >"$fixture/in-store/sub/inner.txt"
ln -s sub "$fixture/in-store/dirlink"
inStore=$(nix-store --add "$fixture/in-store")
check "a path inside it, interpolated" "\"\${$inStore/data.txt}\""
check "the store path itself, interpolated" "\"\${$inStore}\""
check "a path inside it, in concatStringsSep" "builtins.concatStringsSep \":\" [ $inStore/data.txt \"y\" ]"
check "a path inside it, in toJSON" "builtins.toJSON [ $inStore/data.txt ]"
check "a path literal in a file imported from it" "import $inStore/use.nix"
check "a derivation in a file imported from it, naming path literals in its args and environment" "(import $inStore/drv.nix).drvPath"
# A link inside it is followed as one outside it is.  builtins.path copies
# what the link points to at every tag (addPath in primops.cc), storePath
# resolves a path inside a store path in full (prim_storePath in
# primops.cc), and a coerced path has the links among its ancestors
# followed at every tag.
check "builtins.path of a link inside it" "builtins.path { path = $inStore/link; }"
check "storePath of a link inside it" "builtins.storePath $inStore/link"
check "a path below a link inside it, interpolated" "\"\${$inStore/dirlink/inner.txt}\""
echo

echo "== what a coerced path that is a symlink is copied as =="
# From 2.34.9 copyPathToStore in eval.cc resolves only a path's ancestors
# (SymlinkResolution::Ancestors) and copies a path that is a link as the
# link, which nova-nix follows.  2.24.9 resolved the path in full
# (path.resolveSymlinks()) and copied what the link points to, so its row
# has none of these.
ln -s data.txt "$fixture/link-to-file"
ln -s sub "$fixture/link-to-dir"
if upstreamAtLeast 2.34.0; then
  check "a link inside a store path, interpolated" "\"\${$inStore/link}\""
  # Each ${ is Nix's, not the shell's.
  # shellcheck disable=SC2016
  {
    check "a link to a file, interpolated" '"${./link-to-file}"'
    check "a link to a directory, interpolated" '"${./link-to-dir}"'
  }
fi
echo

echo "== how tightly operators bind =="
# Inside a let, since an expression that starts with a dash reads as a flag
# on the command line.
check "negation binds tighter than ?" 'let x = -5 ? a; in x'
check "a second ? applies to the first's whole" '{ a = { }; } ? a ? b'
# 1 * ([ ] ++ throw "c") reaches the throw; (1 * [ ]) ++ throw "c" would
# fail on the product first, which tryEval does not catch.
check "++ binds tighter than *" 'builtins.tryEval (1 * [ ] ++ throw "c")'
check "! as the operand of a tighter operator parses" 'let x = 1 + !true; in 2'
check "! takes in the tighter operators after it" '!{ } ? a'
unparsed "== does not chain" '1 == 2 == 3'
unparsed "< does not chain" '1 < 2 > 3'
echo

echo "== arithmetic and comparison as upstream's primops do them =="
check "unary minus is 0 - e, so a float zero negates to positive zero" '[ (-(0.0)) (-(-0.0)) ]'
check "division by a float, and integer division truncating toward zero" '[ (1 / 4.0) (7 / 2) ((-7) / 2) ]'
check "a list comparison passes over equal elements of an unordered type" '[ ([ { } ] < [ { } ]) ([ { } 1 ] < [ { } 2 ]) ]'
# Two sets of one size compare pair by pair in the order their names were
# interned, which for these names, interned by neither evaluator at
# startup, is the order the expression gives them.
check "a set comparison decides at the pair whose name was interned first" '{ zb = 1; za = throw "a"; } == { zb = 2; za = 1; }'
refused "a set comparison compares a pair's values before a later pair's names" '{ zq1 = throw "x"; zq2 = 1; } == { zq1 = 1; zq3 = 1; }'
echo

echo "== which argument a builtin checks first =="
# The earlier argument is of the wrong type and the later one throws, so
# the error says which one the builtin checked first.
for call in 'builtins.hasAttr 1' 'builtins.getAttr 1' 'builtins.catAttrs 1' 'builtins.match 1' \
  'builtins.split 1' 'builtins.compareVersions 1' 'builtins.toFile 1' 'builtins.concatStringsSep 1' \
  'builtins.appendContext 1' 'builtins.bitAnd "x"' 'builtins.bitOr "x"' 'builtins.bitXor "x"' \
  'builtins.removeAttrs 1' 'builtins.intersectAttrs 1' 'builtins.findFile [ 1 ]' 'builtins.substring 0 "x"' \
  'builtins.replaceStrings [ ] [ "a" ]' 'builtins.concatMap 1' 'builtins.any 1' 'builtins.all 1' \
  'builtins.partition 1' 'builtins.groupBy 1' "builtins.foldl' 1 0"; do
  refused "$call, before the argument after it" "$call (throw \"later\")"
done
refused "substring checks its start before it forces the length" 'builtins.substring (-1) (throw "later") "s"'
refused "concatMap checks its function for an empty list" 'builtins.concatMap 1 [ ]'
refused "foldl' checks its function for an empty list" "builtins.foldl' 1 0 [ ]"
refused "findFile reads an entry with no prefix" 'builtins.findFile [ { path = throw "path"; } ] "x"'
echo

echo "== which builtins call a set with __functor =="
check "a set with __functor is a function to the builtins that call one" 'let f = { __functor = self: x: x > 1; }; v = [ (builtins.filter f [ 1 2 ]) (builtins.any f [ 2 ]) (builtins.partition f [ 1 2 ]) ]; in builtins.deepSeq v v'
echo

echo "== what partition forces =="
refused "partition forces each element" 'builtins.partition (x: true) [ (throw "element") ]'
echo

echo "== what a call, a select and abort refuse =="
refusedAtPosition "a missing argument is reported before an unexpected one" '({ a }: a) { b = 1; }' "from call site"
refusedAtPosition "a missing argument names an anonymous function" '({ a }: a) { }' "from call site"
refusedAtPosition "a function a let binds is named by its binding" 'let f = { a }: a; in f { }' "from call site"
refusedAtPosition "a function a set binds names the function its body is too" '{ f = x: { a }: a; }.f 1 { }' "from call site"
refusedAtPosition "a function under an assert is anonymous" 'let f = assert true; { a }: a; in f { }' "from call site"
refusedAtPosition "a function a formal defaults to is anonymous" '({ g ? { a }: a }: g { }) { }' "from call site"
refused "an argument that is not a set" '({ a }: a) 1' "while evaluating the value passed for the lambda argument"
refusedAtPosition "a select names the attribute its set lacks" '{ a = 1; }.b'
refused "a select into a value that is not a set" 'let v = 1; in v.a' "while selecting an attribute"
# The ${ is Nix's interpolation, not the shell's.
# shellcheck disable=SC2016
refused "a select reads a name before the value it selects from" '{ a = throw "x"; }.a.${throw "y"}'
refused "abort reports the message it was given" 'abort "boom"'
echo

# Upstream prints a function's name as its binding spells it, and an
# argument's or attribute's name as it prints a symbol: bare when it is an
# identifier, otherwise as a string literal.  A formal is always an
# identifier.
echo "== how a call and a select print a name =="
refusedAtPosition "a function is named by its binding as written" '{ "a b" = { a }: a; }."a b" { }' "from call site"
refusedAtPosition "a missing argument prints bare" "({ a'-b }: 1) { }" "from call site"
refusedAtPosition "an unexpected argument that is not an identifier prints as a string" '({ }: null) { "a b" = 1; }' "from call site"
refusedAtPosition "an unexpected argument that starts with a digit prints as a string" '({ }: null) { "1x" = 1; }' "from call site"
refusedAtPosition "an unexpected argument that is a keyword prints as a string" '({ }: null) { "if" = 1; }' "from call site"
# The \${ is Nix's escaped interpolation, not the shell's.
# shellcheck disable=SC2016
refusedAtPosition "an unexpected argument prints with its quote and interpolation escaped" '({ }: null) { "a\"b\${c" = 1; }' "from call site"
refusedAtPosition "a select names a missing attribute that is not an identifier as a string" '{ a = 1; }."b c"'
refusedAtPosition "a select names a missing keyword attribute as a string" '{ a = 1; }."if"'
# shellcheck disable=SC2016
refusedAtPosition "a select names a missing dynamic attribute as a string" '{ a = 1; }.${"1x"}'
echo

# The argument a call names is the first of its kind in symbol order, and
# a name's place there is where the evaluator first interned it: these
# names at startup, in another order than the source gives them and, but
# for x and the empty name, than their own.
echo "== the order upstream interns names as it starts =="
refusedAtPosition "a name of the derivation wrapper before a program's" '({ }: null) { zzz = 1; x = 2; }' "from call site"
refusedAtPosition "a static symbol before a primop's name" '({ }: null) { map = 1; outPath = 2; }' "from call site"
refusedAtPosition "a static symbol of the parser's before a constant's name" '({ }: null) { null = 1; or = 2; }' "from call site"
refusedAtPosition "primops in the order a file registers them" '({ }: null) { seq = 1; throw = 2; }' "from call site"
# A file's primops register as its static initializers run.  The release
# builds of 2.27.0 through 2.34 compile libexpr's files apart and link
# them under GCC's link-time optimization, which merges every file's
# initializers into one function that runs them in the reverse of link
# order (compare_ctor, gcc/ipa.cc): 2.34.9 registers fetchTree.cc's
# primops, then fetchMercurial.cc's, then context.cc's, then primops.cc's.
# 2.24.9 links the files without it, and 2.35.0 compiles them as one unity
# build, both in the order nova-nix follows.
if ! upstreamAtLeast 2.27.0 || upstreamAtLeast 2.35.0; then
  refusedAtPosition "primops.cc's primops before primops/context.cc's" '({ }: null) { getContext = 1; toString = 2; }' "from call site"
  refusedAtPosition "primops/context.cc's primops before fetchMercurial.cc's" '({ }: null) { fetchMercurial = 1; unsafeDiscardStringContext = 2; }' "from call site"
  refusedAtPosition "fetchMercurial.cc's primop before fetchTree.cc's" '({ }: null) { fetchGit = 1; fetchMercurial = 2; }' "from call site"
fi
refusedAtPosition "a primop's name before the derivation wrapper's" '({ }: null) { commonAttrs = 1; toString = 2; }' "from call site"
refusedAtPosition "the empty name as a static symbol" '({ }: null) { zz = 1; "" = 2; }' "from call site"
refusedAtPosition "the first missing argument in symbol order" '({ aa, value }: 1) { }' "from call site"
echo

# listToAttrs, groupBy and fromJSON intern each name of the set they build as
# they meet it, rather than in name order, so the argument a call names and
# the pair a set comparison decides at are the first the builtin met.
# Neither evaluator interns these names at startup.
echo "== the order a builtin interns the names of a set it builds =="
refusedAtPosition "listToAttrs in list order" '({ }: null) (builtins.listToAttrs [ { name = "zb"; value = 1; } { name = "za"; value = 2; } ])' "from call site"
refusedAtPosition "groupBy in the order its function names the groups" '({ }: null) (builtins.groupBy (x: x) [ "zb" "za" ])' "from call site"
refusedAtPosition "fromJSON in document order" '({ }: null) (builtins.fromJSON "{\"zb\": 1, \"za\": 2}")' "from call site"
refusedAtPosition "fromJSON a member's name before the names in its value" '({ }: null) (let j = builtins.fromJSON "{\"zb\": {\"za\": 1}}"; in j // builtins.head (builtins.attrValues j))' "from call site"
refused "a set comparison over two sets listToAttrs built" 'builtins.listToAttrs [ { name = "zb"; value = throw "b"; } { name = "za"; value = 1; } ] == builtins.listToAttrs [ { name = "zb"; value = 1; } { name = "za"; value = throw "a"; } ]'
echo

echo "== when builtins.addErrorContext forces its message =="
check "not when its value evaluates" 'builtins.addErrorContext (throw "a") 1'
check "a throw inside it stays catchable" '(builtins.tryEval (builtins.addErrorContext "c" (throw "x"))).success'
refused "a throw inside it is the failure, with the message a line of its trace" 'builtins.addErrorContext "ctx-marker" (throw "x")' ctx-marker
# 2.36.0 keeps the value's failure whatever evaluating the message does,
# with a line naming the message's own failure (prim_addErrorContext in
# primops.cc), as nova-nix does; earlier releases let the message's failure
# escape in its place.
if upstreamAtLeast 2.36.0; then
  refused "a message that throws leaves the failure, with a line naming its own" 'builtins.addErrorContext (throw "a") (throw "b")' "another error occurred: a"
  check "a message that does not coerce leaves a throw catchable" 'builtins.tryEval (builtins.addErrorContext 1 (throw "x"))'
fi
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

echo "== what fromTOML makes of a date =="
refused "fromTOML refuses a date" 'builtins.fromTOML "d = 1979-05-27"'
refused "fromTOML refuses a time" 'builtins.fromTOML "t = 07:32:00"'
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

echo "== a path reader realises its argument's context =="
# nix-instantiate --eval runs read-only (main_nix_instantiate in
# nix-instantiate.cc), so derivationStrict computes a derivation's path
# without writing it, and an import of its output finds the derivation not
# valid (EvalState::realiseContext in primops.cc).  The name is the case's
# own, so no other case has made the derivation valid.  2.34.9 names the
# path by its base name alone (the InvalidPathError constructor in
# eval-error.cc at 2.34.9), where 2.24.9 and 2.35.4 name the whole store
# path, as nova-nix does.
if ! upstreamAtLeast 2.34.0 || upstreamAtLeast 2.35.0; then
  refused "an import from a derivation in a read-only evaluation" 'import (derivation { name = "parity-ifd-read-only"; system = "x86_64-linux"; builder = "/bin/sh"; })'
fi
echo

echo "== what a fetcher returns =="
# A fetched store path is a string carrying itself as context
# (allowAndSetStorePathString in fetchTree.cc's fetch), and so is fetchGit's
# outPath (mkStorePathString in emitTreeAttrs): it prints quoted, and a
# coercion names the store path rather than copying it again as it would a
# path.  A fetcher writes in any mode, and nova-nix refuses to write into a
# store upstream's database manages, so it fetches into a store of its own;
# both print the logical /nix/store path.  Each row compares the value alone:
# upstream's first fetch of the tarball prints "unpacking '<url>' into the Git
# cache..." on stderr at 2.24.9, 2.34.9 and 2.35.4.
printf 'fetched\n' >"$fixture/parity-fetched"
mkdir -p "$fixture/tarball/tree"
printf 'in a tarball\n' >"$fixture/tarball/tree/f"
tar -C "$fixture/tarball" -czf "$fixture/parity.tar.gz" tree
git init --quiet "$fixture/repo"
printf 'in a repository\n' >"$fixture/repo/f"
git -C "$fixture/repo" add f
git -C "$fixture/repo" -c user.name=parity -c user.email=parity@localhost commit --quiet -m f
# nova-nix's store for one call.
withNovaStore() {
  local novaStore="$fixture/nova-store"
  "$@"
}
fetchedFile="builtins.fetchurl \"file://$fixture/parity-fetched\""
fetchedTarball="builtins.fetchTarball \"file://$fixture/parity.tar.gz\""
fetchedRepo="(builtins.fetchGit { url = \"$fixture/repo\"; }).outPath"
withNovaStore checkValue "what fetchurl returns" "$fetchedFile"
withNovaStore checkValue "the type of what fetchurl returns" "builtins.typeOf ($fetchedFile)"
withNovaStore checkValue "what fetchTarball returns" "$fetchedTarball"
withNovaStore checkValue "the type of what fetchTarball returns" "builtins.typeOf ($fetchedTarball)"
withNovaStore checkValue "fetchGit's outPath" "$fetchedRepo"
withNovaStore checkValue "the type of fetchGit's outPath" "builtins.typeOf $fetchedRepo"
echo

echo "== a pinned fetch the store holds already =="
# A pinned fetch whose path is valid returns it without downloading or
# writing anything (the pinned early exit of fetch in fetchTree.cc).  2.34.9
# ensures the path instead, which for a valid one returns at once without
# asking a substituter (Store::ensurePath in build/entry-points.cc at
# 2.34.9, LocalBuilder::ensurePath at 2.36.0).  nix-store --add-fixed makes
# the path valid with nothing fetched, and the URL names no file, so a
# download would fail.
pinnedFile="$fixture/parity-pinned"
printf 'pinned\n' >"$pinnedFile"
pinnedHash=$(nix-hash --type sha256 --flat "$pinnedFile")
nix-store --add-fixed sha256 "$pinnedFile" >/dev/null
check "a pinned fetchurl whose path is valid" "builtins.fetchurl { url = \"file://$fixture/absent/parity-pinned\"; sha256 = \"$pinnedHash\"; }"
echo

echo "== what allow-import-from-derivation and trace-import-from-derivation say =="
# The settings act once the derivation is valid, so it has to be in the store
# already: a fixed-output one, whose .drv nix-instantiate writes (it runs
# read-write without --eval, main_nix_instantiate in nix-instantiate.cc) and
# whose output nix-store --add-fixed adds where outputHash says it goes, with
# nothing built.  nova-nix's store here is upstream's, where a tree that
# exists counts as valid.  Each row passes its setting through NIX_CONFIG,
# which both evaluators read.
ifdOutput="$fixture/parity-ifd-settings"
printf '"read from a derivation"' >"$ifdOutput"
ifdHash=$(nix-hash --type sha256 --flat "$ifdOutput")
ifdDrv="derivation { name = \"parity-ifd-settings\"; system = \"x86_64-linux\"; builder = \"/bin/sh\"; outputHashMode = \"flat\"; outputHashAlgo = \"sha256\"; outputHash = \"$ifdHash\"; }"
(cd "$fixture" && nix-instantiate -E "$ifdDrv" >/dev/null)
nix-store --add-fixed sha256 "$ifdOutput" >/dev/null
# A setting added to whatever NIX_CONFIG already holds, for one call.
newline=$'\n'
withConfig() {
  local NIX_CONFIG="${NIX_CONFIG:+$NIX_CONFIG$newline}$1"
  export NIX_CONFIG
  shift
  "$@"
}
# With the derivation and its output valid, both evaluators import from it
# and build nothing.
check "an import from a derivation whose output is valid" "import ($ifdDrv)"
# The refusal comes after the validity checks and before any build, so an
# output already valid is refused too (EvalState::realiseContext in
# primops.cc).
withConfig "allow-import-from-derivation = false" refused "an import from a derivation whose output is valid, with allow-import-from-derivation off" "import ($ifdDrv)"
# trace-import-from-derivation arrives after 2.24.9 (EvalState::realiseContext
# in primops.cc), which names it an unknown setting.
if upstreamAtLeast 2.34.0; then
  withConfig "trace-import-from-derivation = true" check "an import from a derivation, warned of under trace-import-from-derivation" "import ($ifdDrv)"
fi
echo

echo "== Nova's mirror interface preserves fixed-output identity =="
for caseName in legacy sha256 sri order invalidUrls invalidHashes escapedUrl unicodeUrl; do
  check "fetchurl $caseName" "(import ./fetchurl-cases.nix).$caseName"
done
echo

printf '%d checked, %d differing\n' "$checked" "$failures"
[ "$failures" -eq 0 ]
