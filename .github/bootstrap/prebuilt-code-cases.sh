#!/usr/bin/env bash
# What prebuilt-code.sh decides for trust reports, one case per rule.  The
# Stdenv job runs these before the check, since a check that cannot fail
# is a run that tests nothing and stays green.
#
# Needs jq, as prebuilt-code.sh does.  Portable to bash 3.2, which is what
# macOS ships.
#
# No set -e: a mismatch is the output, not a reason to stop before the rest
# of the cases have run.
set -uo pipefail

script=$(dirname "${BASH_SOURCE[0]}")/prebuilt-code.sh
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
checked=0
failures=0

readonly SEED=/nix/store/0123456789abcdfghijklmnpqrsvwxyz-hex0-seed.exe
readonly HEX0=/nix/store/1123456789abcdfghijklmnpqrsvwxyz-hex0.exe
readonly ARCHIVE=/nix/store/2123456789abcdfghijklmnpqrsvwxyz-tool-1.0.tar.xz
readonly TREE=/nix/store/3123456789abcdfghijklmnpqrsvwxyz-repo-0123456.tar.gz
readonly OTHER=/nix/store/4123456789abcdfghijklmnpqrsvwxyz-other.exe

# A report that passes: the seed and hex0 run as builders, an archive
# holds a listed object file, and another the seed's own copy.
readonly REPORT='{
  "version": 1,
  "fixedOutputInputs": {
    "'"$SEED"'": {"class": "runAsBuilder", "inStore": true, "sameHash": ["'"$HEX0"'"], "prebuiltCode": [{"kind": "peImage", "file": null, "member": null, "sameBytesAs": ["'"$SEED"'", "'"$HEX0"'"]}], "notRead": []},
    "'"$HEX0"'": {"class": "runAsBuilder", "inStore": true, "sameHash": ["'"$SEED"'"], "prebuiltCode": [{"kind": "peImage", "file": null, "member": null, "sameBytesAs": ["'"$SEED"'", "'"$HEX0"'"]}], "notRead": []},
    "'"$ARCHIVE"'": {"class": "holdsPrebuiltCode", "inStore": true, "sameHash": [], "prebuiltCode": [{"kind": "coffObject", "file": null, "member": "tool-1.0/contrib/x.obj", "sameBytesAs": []}], "notRead": []},
    "'"$TREE"'": {"class": "holdsPrebuiltCode", "inStore": true, "sameHash": [], "prebuiltCode": [{"kind": "peImage", "file": null, "member": "repo-0123456/seeds/hex0-seed.exe", "sameBytesAs": ["'"$SEED"'", "'"$HEX0"'"]}], "notRead": []}
  },
  "buildersOutsideClosure": {},
  "buildersInInputSources": {},
  "builtinBuilders": {"builtin:fetchurl": 4},
  "inputSources": []
}'
readonly LIST='[{"archive": "tool-1.0.tar.xz", "why": "unused", "paths": ["tool-1.0/contrib/x.obj"]}]'

# expect STATUS TEXT LABEL [EDIT [LIST]]: prebuilt-code.sh's verdict on the
# report EDIT, a jq filter, makes of REPORT, against LIST, exits STATUS and
# prints a line holding TEXT.
expect() {
  local out got
  checked=$((checked + 1))
  jq "${4:-.}" <<<"$REPORT" > "$work/report.json"
  printf '%s\n' "${5:-$LIST}" > "$work/list.json"
  out=$(bash "$script" "$work/report.json" "$work/list.json" "$SEED" 2>&1)
  got=$?
  if [ "$got" = "$1" ] && printf '%s\n' "$out" | grep -qF -- "$2"; then
    printf 'ok    %s\n' "$3"
  else
    printf 'FAIL  %s: exited %s, wanted %s and a line holding %s, printed:\n' "$3" "$got" "$1" "$2"
    printf '%s\n' "$out" | sed 's/^/      /'
    failures=$((failures + 1))
  fi
}

expect 0 "4 fixed-output inputs, 2 of them run as builders; 2 files of prebuilt code in the others, 1 of them copies of an input run as a builder and 1 on the list" \
  "the seed, a reproduction of it, a listed file and the seed's copy in a tree pass"

expect 1 "::error::$OTHER is run as a builder, and is not the seed and has not its hash" \
  "another input run as a builder fails" \
  ".fixedOutputInputs[\"$OTHER\"] = {class: \"runAsBuilder\", inStore: true, sameHash: [], prebuiltCode: [], notRead: []}"

expect 1 "::error::the seed, $SEED, is not an input the report says a build runs as its builder" \
  "a report without the seed run as a builder fails" \
  "del(.fixedOutputInputs[\"$SEED\"])"

expect 1 "::error::C:\\Windows\\System32\\cmd.exe is the builder of 2 derivations, and the closure neither builds nor fetches it" \
  "a builder from outside the closure fails" \
  '.buildersOutsideClosure = {"C:\\Windows\\System32\\cmd.exe": 2}'

expect 1 "::error::/nix/store/5123456789abcdfghijklmnpqrsvwxyz-tool.exe is the builder of 1 derivations, and is an input source" \
  "a builder that is an input source fails" \
  '.buildersInInputSources = {"/nix/store/5123456789abcdfghijklmnpqrsvwxyz-tool.exe": 1}'

expect 1 "::error::$OTHER was not read to its end at tool/lib.a: its member is too large" \
  "an input not read to its end fails" \
  ".fixedOutputInputs[\"$OTHER\"] = {class: \"notFullyRead\", inStore: true, sameHash: [], prebuiltCode: [], notRead: [{file: \"tool/lib.a\", reason: \"its member is too large\"}]}"

expect 1 "::error::$ARCHIVE was not read to its end at the fetched file: it ends inside a member" \
  "an input that holds listed code and was not read to its end fails" \
  ".fixedOutputInputs[\"$ARCHIVE\"].notRead = [{file: null, reason: \"it ends inside a member\"}]"

expect 1 "::error::$OTHER is in no substituter, so the report could not read it" \
  "an input not in the store fails" \
  ".fixedOutputInputs[\"$OTHER\"] = {class: \"notFullyRead\", inStore: false, sameHash: [], prebuiltCode: [], notRead: []}"

expect 1 "::error::tool-1.0.tar.xz holds prebuilt code, elfImage, at tool-1.0/test/a.out, which the list does not name" \
  "a prebuilt file the list does not name fails" \
  ".fixedOutputInputs[\"$ARCHIVE\"].prebuiltCode += [{kind: \"elfImage\", file: null, member: \"tool-1.0/test/a.out\", sameBytesAs: []}]"

expect 1 "::error::other.exe holds prebuilt code, peImage, at the fetched file itself, which the list does not name" \
  "a fetched program no build runs fails" \
  ".fixedOutputInputs[\"$OTHER\"] = {class: \"holdsPrebuiltCode\", inStore: true, sameHash: [], prebuiltCode: [{kind: \"peImage\", file: null, member: null, sameBytesAs: []}], notRead: []}"

expect 1 "::error::the list names tool-1.0/contrib/y.obj in tool-1.0.tar.xz, which the report does not find" \
  "a listed file the report does not find fails" \
  . '[{"archive": "tool-1.0.tar.xz", "why": "unused", "paths": ["tool-1.0/contrib/x.obj", "tool-1.0/contrib/y.obj"]}]'

expect 1 "::error::tool-1.0.tar.xz holds prebuilt code, coffObject, at tool-1.0/contrib/x.obj, which the list does not name" \
  "a file listed under another archive's name fails" \
  . '[{"archive": "tool-2.0.tar.xz", "why": "unused", "paths": ["tool-1.0/contrib/x.obj"]}]'

expect 1 "::error::the report is version 2, and this check reads version 1" \
  "a report of another version fails" \
  '.version = 2'

if [ "$failures" -gt 0 ]; then
  echo "$checked cases, $failures failed"
  exit 1
fi
echo "$checked cases, 0 failed"
