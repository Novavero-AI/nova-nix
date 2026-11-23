#!/usr/bin/env bash
# Decide how a Seed ladder run's jobs come by the seed ladder, from the
# event that started the run, its ref, and for a pull request the files it
# changes.  The merge queue runs no ladder job, Ladder (mode) among them, so
# this is not asked about it.
#
# Prints one mode:
#   skip         the ladder jobs do not run: a pull request whose every
#                changed file is documentation
#   substitute   the x64 leg takes each rung the public cache holds from it
#                and builds the ones it lacks: a pull request that changes
#                anything else, and every push to main, whatever it
#                changes, so that a push after one whose run was cancelled
#                while it waited publishes what that one would have
#   seed         both legs build every rung from the seed and name no
#                substituter but the handoff each stage leaves the next:
#                any other event or ref, which is the daily run seed.yml
#                calls, a dispatched run and a release's tag
#
# A pull request with no changed file at all is substitute: skip rests on
# the list, and an empty list is also what a diff that went wrong looks
# like.
#
# Each path is printed on stderr with its class, so the job log shows what
# decided the mode.
#
# Usage: ladder-mode.sh EVENT REF < PATHS
#   EVENT   the event that started the run, as github.event_name names it
#   REF     the ref the run is on, as github.ref names it
#   PATHS   for a pull request, the changed files, each ended by a NUL, as
#           `git diff --name-only --no-renames -z` prints them.
#           --no-renames matters: it lists a renamed file under both of its
#           names, where the default lists the new one alone.  Read for a
#           pull request alone.
#
# Portable to bash 3.2, which is what macOS ships.
set -euo pipefail

if [ $# -ne 2 ]; then
  echo "usage: ladder-mode.sh EVENT REF < PATHS" >&2
  exit 1
fi
event=$1
ref=$2

readonly MAIN_REF=refs/heads/main

# What a change to a path can change:
#   docs    nothing a job reads: the changelog fragments, the license and
#           its notice, and the Markdown at the repository's root.
#           Markdown anywhere else is not docs, since a derivation reads
#           whatever is in a directory it takes as a source.
#   other   everything else: a rung's derivation, the nova-nix that builds
#           it, the checks and the workflows.
classify() {
  case $1 in
    changelog.d/* | LICENSE | NOTICE) echo docs ;;
    */*) echo other ;;
    *.md) echo docs ;;
    *) echo other ;;
  esac
}

# substitute unless every changed path is documentation.
pull_request_mode() {
  local path class docs=0 other=0
  # The second test keeps a last path that no NUL ends.
  while IFS= read -r -d '' path || [ -n "$path" ]; do
    class=$(classify "$path")
    printf '%-8s %s\n' "$class" "$path" >&2
    case $class in
      docs) docs=$((docs + 1)) ;;
      other) other=$((other + 1)) ;;
    esac
  done
  if [ "$docs" -gt 0 ] && [ "$other" -eq 0 ]; then
    echo skip
  else
    echo substitute
  fi
}

case $event in
  pull_request) pull_request_mode ;;
  push)
    if [ "$ref" = "$MAIN_REF" ]; then
      echo substitute
    else
      echo seed
    fi
    ;;
  *) echo seed ;;
esac
