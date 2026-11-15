#!/usr/bin/env bash
# Publish a ladder stage to the public cache, as its x64 job built it and
# its checks ran on it.
#
# --all is every valid path but the derivations in a store the job filled
# from empty: the stage's rungs, what it took from the handoff and the
# public cache, and its checks' outputs, which go with it and are never
# taken back, since each forbids substitutes (test/data/check.nix).  push
# skips what the cache already serves, as it does every source.  The job
# runs this after its checks, so a stage that fails one publishes nothing.
#
# Usage: publish.sh NOVA_NIX
#   NOVA_NIX   the nova-nix executable
# Required environment:
#   CACHE_URL            the cache's address
#   NOVA_CACHE_API_KEY   its write token.  A job that references no
#                        environment holding it gets it empty, and push
#                        refuses an empty key file.
#   RUNNER_TEMP          the runner's temporary directory, a Windows path
#                        nova-nix reads the key file from
set -euo pipefail

nova_nix=${1:?the nova-nix executable}
: "${CACHE_URL:?}" "${NOVA_CACHE_API_KEY?}" "${RUNNER_TEMP:?}"

keyfile=$(mktemp "$RUNNER_TEMP/cache-key.XXXXXX")
trap 'rm -f "$keyfile"' EXIT
printf '%s' "$NOVA_CACHE_API_KEY" > "$keyfile"
"$nova_nix" push --store 'C:\nix\store' --cache "$CACHE_URL" --key-file "$keyfile" \
  --compression zstd --all
