#!/usr/bin/env bash
# Concatenate changelog.d/ fragments in the order they were added, for pasting
# under a version heading in CHANGELOG.md when cutting a release.
#
# Order comes from git rather than from filenames: each fragment is added by
# exactly one commit, so merge order is available and needs no coordination
# between branches (a numeric prefix would just move the collision onto the
# prefix).  A shallow clone cannot answer that question and the failure mode is
# a silently empty release section, so the count is asserted against the
# working tree rather than trusted.
#
# Portable to bash 3.2, which is what macOS ships: no mapfile, no readarray.
set -euo pipefail

cd "$(git rev-parse --show-toplevel)"

fragmentDir=changelog.d

onDiskCount=$(find "$fragmentDir" -type f -name '*.md' | wc -l | tr -d ' ')
if [ "$onDiskCount" -eq 0 ]; then
  echo "no fragments in $fragmentDir/" >&2
  exit 1
fi

# Fragments named by git, oldest commit first, each at the first commit that
# added its name, filtered to the ones that still exist.  --no-renames lists a
# rename as its old name removed and its new name added, so a renamed fragment
# is ordered by its rename; git's default would list the old name alone, which
# no longer exists.  A fragment removed and added again is named twice and kept
# once.
ordered=""
orderedCount=0
while IFS= read -r fragment; do
  if [ -f "$fragment" ]; then
    ordered="${ordered}${fragment}"$'\n'
    orderedCount=$((orderedCount + 1))
  fi
done < <(
  git log --reverse --no-renames --diff-filter=A --format= --name-only -- "$fragmentDir" \
    | grep -E "^${fragmentDir}/.*\.md$" \
    | awk '!seen[$0]++' || true
)

if [ "$orderedCount" -ne "$onDiskCount" ]; then
  echo "git names ${orderedCount} fragment(s) but ${onDiskCount} exist on disk." >&2
  echo "A shallow clone cannot see when a fragment was added: fetch full history" >&2
  echo "(git fetch --unshallow) and run this again.  An uncommitted fragment is" >&2
  echo "invisible here too." >&2
  exit 1
fi

printf '%s' "$ordered" | while IFS= read -r fragment; do
  cat "$fragment"
  # A fragment need not end in a newline; the separator must not depend on it.
  if [ -n "$(tail -c 1 "$fragment")" ]; then
    echo
  fi
done
