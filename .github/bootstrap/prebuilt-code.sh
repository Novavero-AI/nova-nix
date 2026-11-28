#!/usr/bin/env bash
# Fail unless nova-nix trust's report of a closure shows the seed as the
# only prebuilt code a build runs, and every other prebuilt file in a
# source archive on pkgs/windows/unused-prebuilt.json.
#
# The report is trust's --json form, and it fails here where:
#   - an input a build runs as its builder is not the seed and has not
#     the seed's hash (hex0, which reproduces the seed, has it);
#   - a builder is neither built in the closure nor fetched, or is an
#     input source;
#   - an input the builds do not run was not read to its end, or not at
#     all, so what it holds is unknown;
#   - a file of prebuilt code in an input is not a copy of an input run
#     as a builder (the seed's own copy in stage0-pe32's tree is one) and
#     is not the list's, by its archive's name and its path there;
#   - the list names a file the report does not find, so that it holds
#     exactly the files the builds remove.
# prebuilt-code-cases.sh shows each of these failing.
#
# Usage: prebuilt-code.sh REPORT LIST SEED
#   REPORT  what nova-nix trust --json printed
#   LIST    pkgs/windows/unused-prebuilt.json
#   SEED    the seed's store path, as nova-nix eval prints it
#
# Needs jq.
set -euo pipefail

report=${1:?the trust report}
list=${2:?the list of unused prebuilt files}
seed=${3:?the store path of the seed}

# The report version this reads: trust raises it when a field it holds
# changes meaning.
readonly REPORT_VERSION=1

# Each problem as an ::error:: line, then a summary.  An input's archive
# is its store path's name, after the 32 characters of its hash and a
# hyphen.  jq, not the shell, expands the $ names in it.
# shellcheck disable=SC2016
readonly FILTER='
def archive: sub("^.*/[0-9a-df-np-sv-z]{32}-"; "");
def place:
  if .file != null and .member != null then "\(.member) in \(.file)"
  elif .member != null then .member
  elif .file != null then .file
  else "the fetched file itself" end;
$groups[0] as $groups
| [$groups[] | .archive as $archive | .paths[] | {archive: $archive, path: .}] as $listed
| .fixedOutputInputs as $inputs
| [$inputs | to_entries[] | select(.value.class == "holdsPrebuiltCode")
   | (.key | archive) as $archive
   | .value.prebuiltCode[] | {archive: $archive, kind, file, member, copy: ((.sameBytesAs | length) > 0)}] as $found
| [$found[] | select(.copy | not)] as $uncopied
| (
    (if .version != $version then "the report is version \(.version), and this check reads version \($version)" else empty end),
    (if ($inputs[$seed].class // null) != "runAsBuilder" then "the seed, \($seed), is not an input the report says a build runs as its builder" else empty end),
    ($inputs | to_entries[] | select(.value.class == "runAsBuilder")
     | select(.key != $seed and (any(.value.sameHash[]; . == $seed) | not))
     | "\(.key) is run as a builder, and is not the seed and has not its hash"),
    (.buildersOutsideClosure | to_entries[] | "\(.key) is the builder of \(.value) derivations, and the closure neither builds nor fetches it"),
    (.buildersInInputSources | to_entries[] | "\(.key) is the builder of \(.value) derivations, and is an input source"),
    ($inputs | to_entries[] | select(.value.class != "runAsBuilder")
     | if .value.inStore then (.key as $input | .value.notRead[] | "\($input) was not read to its end at \(.file // "the fetched file"): \(.reason)")
       else "\(.key) is in no substituter, so the report could not read it" end),
    ($uncopied[] | select(.file != null or .member == null or ({archive, path: .member} | IN($listed[]) | not))
     | "\(.archive) holds prebuilt code, \(.kind), at \(place), which the list does not name"),
    ($listed[] | . as $entry
     | select(any($uncopied[]; .file == null and .archive == $entry.archive and .member == $entry.path) | not)
     | "the list names \(.path) in \(.archive), which the report does not find")
  | "::error::" + .
  ),
  "\($inputs | length) fixed-output inputs, \([$inputs[] | select(.class == "runAsBuilder")] | length) of them run as builders; \($found | length) files of prebuilt code in the others, \([$found[] | select(.copy)] | length) of them copies of an input run as a builder and \($listed | length) on the list"
'

lines=$(jq -r --arg seed "$seed" --argjson version "$REPORT_VERSION" --slurpfile groups "$list" "$FILTER" "$report")
printf '%s\n' "$lines"
! grep -q '^::error::' <<<"$lines"
