#!/usr/bin/env bash
# Record the NAR hash of each of a stage's rungs of the seed ladder, for
# Ladder (NAR hashes) in seed-ladder.yml to compare across the Windows legs.
#
# A rung's store path is the same on both legs, since its derivation is.
# Its NAR hash is the same only if its build wrote the same bytes on each,
# which a rung that depends on the machine, the user or the time does not.
# The hash is the NarHash of the rung's narinfo in the stage's handoff
# (handoff-serve.sh), which nova-nix push wrote from the path in the store
# when the job pushed its store there.
#
# The record's first line names every stage pkgs/windows/rungs.nix lists,
# so that the comparison knows of a stage that no leg recorded:
#   stages STAGE...
# and each line after it is one rung:
#   rung LEG STAGE ATTR NARHASH STOREPATH
# with the store path as the narinfo spells it.
#
# Usage: rung-hashes.sh NOVA_NIX STAGE LEG RECORD
#   NOVA_NIX   the nova-nix executable
#   STAGE      the stage whose rungs to record, an attribute of rungs.nix
#   LEG        the leg, as its runner image names it
#   RECORD     the file to write
# Required environment:
#   CACHE_URL  the address of the cache that holds the stage's rungs
set -euo pipefail

nova_nix=${1:?the nova-nix executable}
stage=${2:?the stage whose rungs to record}
leg=${3:?the leg}
record=${4:?the file to write}
: "${CACHE_URL:?}"

readonly PACKAGES='import ./pkgs/windows/default.nix'
readonly RUNGS='import ./pkgs/windows/rungs.nix'
# The hash part of a store path: 32 characters of Nix's base-32 alphabet,
# which has no e, o, t or u.
readonly HASH_PATTERN='^[0-9a-df-np-sv-z]{32}$'
# A narinfo is a few hundred bytes, and the handoff is on the loopback.
readonly QUERY_SECONDS=30

fail() {
  echo "::error::$*" >&2
  exit 1
}

# What `nova-nix eval` prints for an expression that is a string, less its
# quotes and the carriage return the executable ends a line with on
# Windows.
evaluate() {
  "$nova_nix" eval --expr "$1" | tr -d '"\r'
}

stages=$(evaluate "builtins.concatStringsSep \" \" (builtins.attrNames ($RUNGS))")
attrs=$(evaluate "builtins.concatStringsSep \" \" ($RUNGS).$stage")
# An empty list would record nothing, and the comparison would take the
# stage for one this leg never built.
[ -n "$attrs" ] || fail "rungs.nix lists no rung for the $stage stage"

{
  echo "stages $stages"
  for attr in $attrs; do
    out=$(evaluate "($PACKAGES).$attr.outPath")
    base=${out##*/}
    hash=${base%%-*}
    # A hash misread here would name a narinfo the cache lacks.
    [[ $hash =~ $HASH_PATTERN ]] || fail "$attr: no store path hash in $out"
    narinfo=$(curl --fail --silent --show-error --max-time "$QUERY_SECONDS" "$CACHE_URL/$hash.narinfo") ||
      fail "$attr: the cache at $CACHE_URL has no narinfo for $base"
    store_path=$(printf '%s\n' "$narinfo" | sed -n 's/^StorePath: //p' | tr -d '\r')
    nar_hash=$(printf '%s\n' "$narinfo" | sed -n 's/^NarHash: //p' | tr -d '\r')
    # The narinfo answers for the path whose hash it is named by, and a
    # record of another path's bytes would compare the wrong rung.
    [ "${store_path##*/}" = "$base" ] || fail "$attr: the narinfo for $base names ${store_path:-no path}"
    [ -n "$nar_hash" ] || fail "$attr: the narinfo for $base states no NarHash"
    echo "rung $leg $stage $attr $nar_hash $store_path"
  done
} > "$record"
cat "$record"
