#!/usr/bin/env bash
# Check that upstream Nix substitutes what `nova-nix push` writes to a
# file:// binary cache under the key that signed it, and refuses it under
# another.
#
# nova-nix builds a derivation whose output references another's and pushes
# the two-path closure to a directory, once uncompressed and once with zstd,
# signed with a key that upstream's `nix-store --generate-binary-cache-key`
# makes for the run.  Each cache is then made read-only, as a reader without
# write access sees it: upstream's file:// store creates its directories in
# every cache it opens, and drops one where it cannot
# (LocalBinaryCacheStore::init in local-binary-cache-store.cc, and
# getDefaultSubstituters, in store-api.cc at 2.24.9 and in
# store-registration.cc at 2.36.0).  For each cache, upstream realises the
# output into a store of its own, with that directory as its one substituter
# and signatures required: first trusting a second key of the same name,
# under which it must ignore the substitute for want of a trusted signature
# (PathSubstitutionGoal's signature check in substitution-goal.cc), then
# trusting the signing key, under which the closure must become valid there
# with the bytes and references nova-nix built.
#
# NOVA_NIX_BIN names the executable under test; nix-store comes from PATH.
# The secret keys stay in their files and are never printed.
#
# No set -e: each step's failure is reported with what it printed.
set -uo pipefail

novaBin=${NOVA_NIX_BIN:?set NOVA_NIX_BIN to the nova-nix executable}
repoRoot=$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)
if ! command -v nix-store >/dev/null 2>&1; then
  echo "nix-store is not on PATH; this script needs a real Nix to substitute with" >&2
  exit 1
fi

# A registered store path is read-only, so the cleanup makes the scratch
# tree writable before removing it.
scratch=$(mktemp -d) || exit 1
trap 'chmod -R u+w "$scratch" 2>/dev/null; rm -rf "$scratch"' EXIT

# Report a failed step with the log it left, and stop.
fail() {
  echo "FAIL  $1" >&2
  if [ -n "${2:-}" ] && [ -r "$2" ]; then sed 's/^/      /' "$2" >&2; fi
  exit 1
}

# Every upstream command runs on a store rooted in the scratch directory,
# whose files live under ROOT/nix/store (StoreReference::parse in
# store-reference.cc and LocalFSStoreConfig::realStoreDir in
# local-fs-store.hh), so neither the daemon nor its configured substituters
# take part.
upstreamRealise() {
  root=$1
  cache=$2
  publicKey=$3
  path=$4
  nix-store --store "$root" \
    --option substituters "file://$cache" \
    --option trusted-public-keys "$(cat "$publicKey")" \
    --option require-sigs true \
    --realise "/nix/store/$path"
}

echo "upstream: $(nix-store --version 2>&1 | head -n1)"
echo "nova-nix: $("$novaBin" --version 2>&1 | head -n1)"
echo

# Two keys under one name, so the refusal under the second is the
# signature's and not the name's.
keyName=nova-nix-parity-1
mkdir "$scratch/keys"
for key in signing other; do
  nix-store --store "$scratch/keygen" --generate-binary-cache-key "$keyName" \
    "$scratch/keys/$key-secret" "$scratch/keys/$key-public" >"$scratch/keygen.log" 2>&1 ||
    fail "upstream generating the $key key" "$scratch/keygen.log"
done

# $out and ${dep} are the derivation's, not the shell's.
# shellcheck disable=SC2016
expr='let
  dep = derivation { name = "push-dep"; system = builtins.currentSystem; builder = "/bin/sh"; args = [ "-c" "echo dep > $out" ]; };
in derivation { name = "push-top"; system = builtins.currentSystem; builder = "/bin/sh"; args = [ "-c" "echo ${dep} > $out" ]; }'
novaStore=$scratch/nova
built=$("$novaBin" build --store "$novaStore" --nix-path "nix=$repoRoot/data/nix" --expr "$expr" 2>"$scratch/build.log") ||
  fail "nova-nix build" "$scratch/build.log"
top=$(printf '%s\n' "$built" | tail -n1)
top=${top##*/}
case "$top" in
*-push-top) ;;
*) fail "nova-nix build printed '$top', not push-top's output" "$scratch/build.log" ;;
esac
set -- "$novaStore"/*-push-dep
[ "$#" -eq 1 ] && [ -e "$1" ] || fail "nova-nix's store holds no single push-dep output: $*"
dep=${1##*/}
echo "built:    $top, referencing $dep"
echo

for compression in none zstd; do
  cache=$scratch/cache-$compression
  "$novaBin" push --store "$novaStore" --cache "file://$cache" \
    --secret-key-file "$scratch/keys/signing-secret" --compression "$compression" "$top" \
    >"$scratch/push.out" 2>"$scratch/push.log" ||
    fail "nova-nix push --compression $compression" "$scratch/push.log"
  grep -qx 'pushed 2 path(s), 0 already cached' "$scratch/push.out" ||
    fail "nova-nix push --compression $compression pushed other than the two paths" "$scratch/push.out"
  grep -qx "Compression: $compression" "$cache/${top%%-*}.narinfo" ||
    fail "the narinfo of $top names another compression than $compression" "$cache/${top%%-*}.narinfo"
  chmod -R a-w "$cache"

  refused=$scratch/upstream-$compression-other
  if upstreamRealise "$refused" "$cache" "$scratch/keys/other-public" "$top" >"$scratch/refused.log" 2>&1; then
    fail "upstream substituted $top from the $compression cache trusting a key that did not sign it" "$scratch/refused.log"
  fi
  grep -q "as it's not signed by any of the keys in 'trusted-public-keys'" "$scratch/refused.log" ||
    fail "upstream refused the $compression cache under the other key for a reason other than its signature" "$scratch/refused.log"
  [ ! -e "$refused/nix/store/$top" ] ||
    fail "upstream left $top in its store after refusing the $compression cache"

  root=$scratch/upstream-$compression
  upstreamRealise "$root" "$cache" "$scratch/keys/signing-public" "$top" >"$scratch/realise.log" 2>&1 ||
    fail "upstream substituting $top from the $compression cache trusting the signing key" "$scratch/realise.log"
  nix-store --store "$root" --verify-path "/nix/store/$dep" "/nix/store/$top" >"$scratch/verify.log" 2>&1 ||
    fail "upstream verifying the paths substituted from the $compression cache" "$scratch/verify.log"
  for path in "$dep" "$top"; do
    cmp -s "$novaStore/$path" "$root/nix/store/$path" ||
      fail "$path substituted from the $compression cache differs from what nova-nix built"
  done
  references=$(nix-store --store "$root" --query --references "/nix/store/$top" 2>"$scratch/query.log") ||
    fail "upstream querying the references of $top" "$scratch/query.log"
  [ "$references" = "/nix/store/$dep" ] ||
    fail "upstream registered $top from the $compression cache with references '$references', not /nix/store/$dep"
  printf '  ok    %s: refused under another key, substituted under the signing key\n' "$compression"
done
