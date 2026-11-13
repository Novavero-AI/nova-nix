#!/usr/bin/env bash
# Realise a leg's rungs of the seed ladder with the public cache as a
# substituter, and fail if a rung the cache holds was built all the same.
#
# A rung whose derivation a pull request leaves alone has the store path a
# push to main published, so it is downloaded.  One the pull request changes
# has a store path the cache lacks, so it builds.  A cache that answers and
# cannot be used looks like the second case: nova-nix passes over a narinfo
# no trusted key signs, or one whose archive is gone, and builds, as
# upstream does.  The job would stay green while every run built the ladder
# from the seed.  So each rung's narinfo is asked for here, and a rung the
# cache holds has to arrive with no [build] line.
#
# The rungs the cache holds are realised first, ahead of any build.  Reached
# first as the input of a rung that is building, one that could not be
# substituted would be built there, among the lines of a build that is
# expected, and would be valid by its own turn.
#
# The caller names the cache: NIX_CONFIG, which the job sets for every step
# after this one too, so what is checked is the configuration those steps
# run under.  They find each rung valid and build none.
#
# pkgs/windows/rungs.nix lists each leg's rungs.
#
# Usage: substitute-rungs.sh NOVA_NIX LEG [ARGUMENT ...]
#   NOVA_NIX   the nova-nix executable
#   LEG        the leg whose rungs to realise, an attribute of rungs.nix
#   ARGUMENT   passed to each `nova-nix eval` and `nova-nix build`, as
#              --store DIR is
# Required environment:
#   CACHE_URL    the cache's address, for the narinfo queries
#   NIX_CONFIG   nix.conf settings naming the cache as a substituter and the
#                key it signs with
#
# Portable to bash 3.2, which is what macOS ships.
set -euo pipefail

nova_nix=${1:?the nova-nix executable}
leg=${2:?the leg whose rungs to realise}
shift 2
: "${CACHE_URL:?}" "${NIX_CONFIG:?}"

readonly PACKAGES='import ./pkgs/windows/default.nix'
readonly RUNGS="(import ./pkgs/windows/rungs.nix).$leg"
# The hash part of a store path: 32 characters of Nix's base-32 alphabet,
# which has no e, o, t or u.
readonly HASH_PATTERN='^[0-9a-df-np-sv-z]{32}$'
# A narinfo is a few hundred bytes.  curl retries a query that timed out or
# met a 5xx.
readonly QUERY_SECONDS=30
readonly QUERY_RETRIES=3

fail() {
  echo "::error::$*" >&2
  exit 1
}

# What `nova-nix eval` prints for an expression, less a string's quotes and
# the carriage return the executable ends a line with on Windows.
evaluate() {
  local expr=$1
  shift
  "$nova_nix" eval --expr "$expr" "$@" | tr -d '"\r'
}

log=$(mktemp)
trap 'rm -f "$log"' EXIT

# realise ATTR [ARGUMENT ...]: build a rung, and set `realised` to how
# nova-nix says it came to be valid.  The build's stdout is the out path.
# Its stderr is the builders' output and a line for each derivation saying
# how it was realised: shown as it comes, and kept to be read.
realise() {
  local attr=$1 start=$SECONDS
  shift
  "$nova_nix" build pkgs/windows/default.nix -A "$attr" "$@" 2>&1 > /dev/null | tee "$log" >&2
  if grep -q -F '[build]' "$log"; then
    realised=built
  elif grep -q -F '[subst]' "$log"; then
    realised=substituted
  elif grep -q -F '[cached]' "$log"; then
    realised='already valid'
  else
    fail "$attr: nova-nix did not say how it was realised, so this script no longer reads its output"
  fi
  echo "== $attr: $realised in $((SECONDS - start)) s"
}

count=$(evaluate "builtins.length ($RUNGS)" "$@")
# An empty list would pass having checked nothing.
[ "$count" -gt 0 ] || fail "rungs.nix lists no rung for the $leg leg"
held=0
# The rungs the cache lacks, each preceded by a space: an attribute path
# has none.
lacked=

for ((index = 0; index < count; index++)); do
  attr=$(evaluate "builtins.elemAt ($RUNGS) $index" "$@")
  out=$(evaluate "($PACKAGES).$attr.outPath" "$@")
  name=${out##*/}
  hash=${name%%-*}
  # A hash misread here would name a narinfo the cache lacks, and the rung
  # would pass for one the pull request changes.
  [[ $hash =~ $HASH_PATTERN ]] || fail "$attr: no store path hash in $out"

  narinfo="$CACHE_URL/$hash.narinfo"
  status=$(curl --silent --show-error --max-time "$QUERY_SECONDS" --retry "$QUERY_RETRIES" \
    --output /dev/null --write-out '%{http_code}' "$narinfo") || fail "$attr: could not ask for $narinfo"
  # Only a 404 says the cache lacks the path.  Any other answer is a cache
  # that cannot be asked, and taking it for a miss would build the rung.
  case $status in
    200)
      echo "== $attr: held by the cache"
      realise "$attr" "$@"
      [ "$realised" != built ] || fail "$attr was built, and the cache holds it: $narinfo"
      held=$((held + 1))
      ;;
    404)
      echo "== $attr: not held by the cache"
      lacked="$lacked $attr"
      ;;
    *) fail "$attr: $narinfo answered $status" ;;
  esac
done
echo "the cache held $held of the $leg leg's $count rungs"

for attr in $lacked; do
  realise "$attr" "$@"
done
