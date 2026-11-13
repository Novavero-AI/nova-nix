#!/usr/bin/env bash
# Realise every source pkgs/windows fetches, taking each from the public
# cache, before a job builds anything.
#
# A source the cache holds is downloaded from it.  One it lacks, as a pull
# request's new source is until a push to main publishes it, is fetched from
# its upstream host.  A cache that cannot be asked fails the job: nova-nix
# stops at a failed query unless told to fall back, and nothing here tells
# it to, so an outage of the cache is never made up for with downloads from
# the upstream hosts.
#
# A job's builds after this find every source already in the store.  They
# name no substituter and substitute nothing, so they build what they are
# there to build, unless the job names one for them, as the Mes and
# Bootstrap legs do in substitute mode (substitute-rungs.sh).
#
# pkgs/windows/sources.nix is a list and nova-nix builds one derivation, so
# the list is realised an element at a time.
#
# Usage: realise-sources.sh NOVA_NIX [BUILD-ARGUMENT ...]
#   NOVA_NIX         the nova-nix executable
#   BUILD-ARGUMENT   passed to each `nova-nix build`, as --store DIR is
# Required environment:
#   CACHE_URL          the cache's address
#   CACHE_PUBLIC_KEY   the key the cache signs with
set -euo pipefail

nova_nix=${1:?the nova-nix executable}
shift
: "${CACHE_URL:?}" "${CACHE_PUBLIC_KEY:?}"

readonly SOURCES='import ./pkgs/windows/sources.nix'

# The executable ends its lines as the platform does.
count=$("$nova_nix" eval --expr "builtins.length ($SOURCES)" | tr -d '\r')

for ((index = 0; index < count; index++)); do
  "$nova_nix" build --expr "builtins.elemAt ($SOURCES) $index" \
    --substituter "$CACHE_URL" --trusted-key "$CACHE_PUBLIC_KEY" "$@" > /dev/null
done
echo "realised $count sources"
