#!/usr/bin/env bash
# Decide how a pull request's Mes and Bootstrap legs come by the seed ladder,
# from the files the pull request changes.
#
# Prints one mode:
#   skip         every changed file is documentation: the legs do not run
#   seed         a changed file can change the nova-nix the legs run: the
#                legs build every rung from the seed and name no substituter
#   substitute   anything else: the legs take each rung the public cache
#                holds from it and build the ones it lacks, which are the
#                ones the pull request changes
#
# A file under a builder path is seed whatever its name ends in, and one
# such file decides the mode however many others there are.  No changed file
# at all is seed too: skip and substitute both rest on the list, and an
# empty list is also what a diff that went wrong looks like.
#
# Each path is printed on stderr with its class, so the job log shows what
# decided the mode.
#
# Usage: ladder-mode.sh < PATHS
#   PATHS   the changed files, each ended by a NUL, as
#           `git diff --name-only --no-renames -z` prints them.  --no-renames
#           matters: it lists a renamed file under both of its names, where
#           the default lists the new one alone.
#
# Portable to bash 3.2, which is what macOS ships.
set -euo pipefail

# What a change to a path can change:
#   builder   the nova-nix the legs run: its sources, its C data layer, the
#             files it ships, its package description and build plan
#   docs      nothing a leg reads: the changelog fragments, the license and
#             its notice, and the Markdown at the repository's root.
#             Markdown anywhere else is not docs, since a derivation reads
#             whatever is in a directory it takes as a source.
#   other     the package set, the checks, the workflows and the rest: a
#             rung whose derivation it changes has a store path the cache
#             lacks.  A workflow is here and not with the builder, since a
#             pull request that adds a rung adds its steps to one, and is
#             the pull request that most needs the rungs below it from the
#             cache.
classify() {
  case $1 in
    src/* | cbits/* | app/* | cli/* | data/* | *.cabal | cabal.project*) echo builder ;;
    changelog.d/* | LICENSE | NOTICE) echo docs ;;
    */*) echo other ;;
    *.md) echo docs ;;
    *) echo other ;;
  esac
}

builder=0
docs=0
other=0
# The second test keeps a last path that no NUL ends.
while IFS= read -r -d '' path || [ -n "$path" ]; do
  class=$(classify "$path")
  printf '%-8s %s\n' "$class" "$path" >&2
  case $class in
    builder) builder=$((builder + 1)) ;;
    docs) docs=$((docs + 1)) ;;
    other) other=$((other + 1)) ;;
  esac
done
changed=$((builder + docs + other))

if [ "$changed" -eq 0 ] || [ "$builder" -gt 0 ]; then
  echo seed
elif [ "$other" -gt 0 ]; then
  echo substitute
else
  echo skip
fi
