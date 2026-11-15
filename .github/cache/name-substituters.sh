#!/usr/bin/env bash
# Name binary caches as the substituters of a job's later steps, asked in
# the order given.  nova-nix reads `substituters` and `trusted-public-keys`
# from NIX_CONFIG, gives every cache named there one priority, and asks
# caches of one priority in the order they are named.  Every key is trusted
# for every cache, as upstream's are.
#
# A step writes NIX_CONFIG because the job's own env cannot: the workflow's
# env, which holds the public cache's address and key, is not a context
# that jobs.<job_id>.env may read, and a handoff's address and key are only
# known once a step has served it.
#
# Usage: name-substituters.sh URL KEY [URL KEY ...]
#   URL   a cache's address
#   KEY   the key it signs with, NAME:BASE64
# Required environment:
#   GITHUB_ENV   the file a step appends to, to set the environment of the
#                job's later steps
#
# Portable to bash 3.2, which is what macOS ships.
set -euo pipefail

: "${GITHUB_ENV:?}"
if [ $# -eq 0 ] || [ $(($# % 2)) -ne 0 ]; then
  echo "usage: name-substituters.sh URL KEY [URL KEY ...]" >&2
  exit 1
fi

urls=
keys=
while [ $# -gt 0 ]; do
  urls="$urls${urls:+ }$1"
  keys="$keys${keys:+ }$2"
  shift 2
done

{
  echo 'NIX_CONFIG<<END_OF_NIX_CONFIG'
  echo "substituters = $urls"
  echo "trusted-public-keys = $keys"
  echo 'END_OF_NIX_CONFIG'
} >> "$GITHUB_ENV"
echo "substituters = $urls"
