#!/usr/bin/env bash
# Compare the NAR hash of every rung of the seed ladder across the legs
# that built it, from the records rung-hashes.sh writes, and fail on a rung
# whose hash differs, naming it and each leg's hash.
#
# Every leg has to have recorded every rung of every stage the records'
# stages lines name, so a stage that one leg did not reach, or whose job
# recorded nothing, fails the comparison rather than passing for one that
# matches.  The legs must also agree on each rung's store path: two paths
# are two derivations, and their hashes would compare nothing.
#
# Usage: compare-rung-hashes.sh DIR LEG...
#   DIR   a directory of records, one file each, as rung-hashes.sh writes
#   LEG   each leg whose records to compare, as its runner image names it
#
# Portable to bash 3.2, which is what macOS ships.
set -euo pipefail

dir=${1:?the directory of records}
shift
[ $# -ge 2 ] || {
  echo "usage: compare-rung-hashes.sh DIR LEG LEG..." >&2
  exit 1
}

shopt -s nullglob
records=("$dir"/*)
[ ${#records[@]} -gt 0 ] || {
  echo "::error::$dir holds no record of a rung's NAR hash"
  exit 1
}

cat "${records[@]}" | awk -v legs="$*" '
  function error(message) {
    print "::error::" message
    failed = 1
  }
  BEGIN {
    nlegs = split(legs, leg, " ")
    for (i = 1; i <= nlegs; i++) known[leg[i]] = 1
  }
  $1 == "stages" {
    list = $0
    sub(/^stages /, "", list)
    if (stages == "") stages = list
    else if (list != stages) error("the records name different stages: " stages "; " list)
    next
  }
  $1 == "rung" && NF == 6 {
    l = $2; key = $3 " " $4
    if (!(l in known)) { error("a record names " l ", which is not a leg compared here"); next }
    if ((key, l) in hash) { error($4 " (" $3 "): recorded twice on " l); next }
    if (!(key in seen)) { seen[key] = 1; order[++nkeys] = key }
    hash[key, l] = $5
    path[key, l] = $6
    recorded[$3, l] = 1
    next
  }
  { error("a record line this script does not read: " $0) }
  END {
    if (stages == "") error("no record names the stages")
    nstages = split(stages, stage, " ")
    for (s = 1; s <= nstages; s++)
      for (i = 1; i <= nlegs; i++)
        if (!((stage[s], leg[i]) in recorded))
          error("the " stage[s] " stage: no rung recorded on " leg[i])
    matched = 0
    for (k = 1; k <= nkeys; k++) {
      key = order[k]
      split(key, part, " ")
      name = part[2] " (" part[1] ")"
      first = ""
      complete = 1
      for (i = 1; i <= nlegs; i++)
        if (!((key, leg[i]) in hash)) {
          if (((part[1], leg[i]) in recorded)) error(name ": not recorded on " leg[i])
          complete = 0
        }
      if (!complete) continue
      same = 1
      for (i = 2; i <= nlegs; i++) {
        if (path[key, leg[i]] != path[key, leg[1]]) {
          error(name ": " leg[1] " built " path[key, leg[1]] " and " leg[i] " built " path[key, leg[i]])
          same = 0
        } else if (hash[key, leg[i]] != hash[key, leg[1]]) {
          error(name ": NAR hash " hash[key, leg[1]] " on " leg[1] " and " hash[key, leg[i]] " on " leg[i] ", " path[key, leg[1]])
          same = 0
        }
      }
      if (same) {
        matched++
        print "ok    " name " " hash[key, leg[1]]
      }
    }
    print matched " of " nkeys " rungs recorded have one NAR hash on " legs
    exit failed
  }
'
