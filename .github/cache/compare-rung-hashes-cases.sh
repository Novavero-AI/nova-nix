#!/usr/bin/env bash
# What compare-rung-hashes.sh decides for each kind of record set, one case
# per rule.  Ladder (NAR hashes) runs these before it lets the script
# compare a run's records, since a comparison that passes what it should
# fail is a check that stays green and tests nothing.
#
# Portable to bash 3.2, which is what macOS ships.
#
# No set -e: a mismatch is the output, not a reason to stop before the rest
# of the cases have run.
set -uo pipefail

script=$(dirname "${BASH_SOURCE[0]}")/compare-rung-hashes.sh
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
checked=0
failures=0

readonly X64=windows-latest
readonly ARM64=windows-11-arm
readonly STAGES='stages gcc46 mes'
readonly H1=sha256:1ycwnvwac4470l6ialrggk0iygz71b05mbqxbylm1n7rvfr78ih0
readonly H2=sha256:0p5gafaggdgx0ylxmqqyqbsjxxrxmhgwqncmcgczy4fxpb5vppdx
readonly KAEM=/nix/store/01y11bqlxxgqg5vlj83sk6njz3m7695s-mescc-tools-bin.kaem
readonly GCC=/nix/store/04w7pj0cn20pa07nk0z0iwyalz99k496-gcc-core-4.6.4
readonly OTHER=/nix/store/078nlm1vpczznqfznnb99n1rdxdrk7w6-gcc-core-4.6.4

# records NAME LINE...: a fresh set of records, NAME its label, each record
# a file whose lines are LINE, the files separated by a -- argument.
records() {
  label=$1
  shift
  rm -rf "$work/records"
  mkdir "$work/records"
  local file=0
  : > "$work/records/$file"
  for line in "$@"; do
    if [ "$line" = -- ]; then
      file=$((file + 1))
      : > "$work/records/$file"
    else
      printf '%s\n' "$line" >> "$work/records/$file"
    fi
  done
}

# expect STATUS TEXT: the comparison of the current records exits STATUS
# and prints a line holding TEXT.
expect() {
  local want=$1 text=$2 got out
  checked=$((checked + 1))
  out=$(bash "$script" "$work/records" "$X64" "$ARM64" 2>&1)
  got=$?
  if [ "$got" = "$want" ] && printf '%s\n' "$out" | grep -qF -- "$text"; then
    printf 'ok    %s\n' "$label"
  else
    printf 'FAIL  %s: exited %s, wanted %s and a line holding %s, printed:\n' "$label" "$got" "$want" "$text"
    printf '%s\n' "$out" | sed 's/^/      /'
    failures=$((failures + 1))
  fi
}

records "every rung of every stage matches on both legs" \
  "$STAGES" "rung $X64 mes stage0-pe32.kaem $H1 $KAEM" "rung $X64 gcc46 gcc-core $H2 $GCC" -- \
  "$STAGES" "rung $ARM64 mes stage0-pe32.kaem $H1 $KAEM" "rung $ARM64 gcc46 gcc-core $H2 $GCC"
expect 0 "2 of 2 rungs recorded have one NAR hash"

records "a rung whose NAR hash differs is named with both hashes" \
  "$STAGES" "rung $X64 mes stage0-pe32.kaem $H1 $KAEM" "rung $X64 gcc46 gcc-core $H1 $GCC" -- \
  "$STAGES" "rung $ARM64 mes stage0-pe32.kaem $H1 $KAEM" "rung $ARM64 gcc46 gcc-core $H2 $GCC"
expect 1 "::error::gcc-core (gcc46): NAR hash $H1 on $X64 and $H2 on $ARM64, $GCC"

records "the stages both legs recorded are compared when one leg stopped early" \
  "$STAGES" "rung $X64 mes stage0-pe32.kaem $H1 $KAEM" "rung $X64 gcc46 gcc-core $H2 $GCC" -- \
  "$STAGES" "rung $ARM64 mes stage0-pe32.kaem $H1 $KAEM"
expect 1 "ok    stage0-pe32.kaem (mes) $H1"
expect 1 "::error::the gcc46 stage: no rung recorded on $ARM64"

records "a stage the stages line names and no leg recorded fails" \
  "$STAGES" "rung $X64 mes stage0-pe32.kaem $H1 $KAEM" -- \
  "$STAGES" "rung $ARM64 mes stage0-pe32.kaem $H1 $KAEM"
expect 1 "::error::the gcc46 stage: no rung recorded on $X64"

records "a rung one leg recorded and the other did not, in a stage both recorded, fails" \
  "$STAGES" "rung $X64 mes stage0-pe32.kaem $H1 $KAEM" "rung $X64 gcc46 gcc-core $H2 $GCC" "rung $X64 gcc46 binutils $H1 $OTHER" -- \
  "$STAGES" "rung $ARM64 mes stage0-pe32.kaem $H1 $KAEM" "rung $ARM64 gcc46 gcc-core $H2 $GCC"
expect 1 "::error::binutils (gcc46): not recorded on $ARM64"

records "a rung the legs built at different store paths fails" \
  "$STAGES" "rung $X64 mes stage0-pe32.kaem $H1 $KAEM" "rung $X64 gcc46 gcc-core $H2 $GCC" -- \
  "$STAGES" "rung $ARM64 mes stage0-pe32.kaem $H1 $KAEM" "rung $ARM64 gcc46 gcc-core $H2 $OTHER"
expect 1 "::error::gcc-core (gcc46): $X64 built $GCC and $ARM64 built $OTHER"

records "a rung recorded twice on one leg fails" \
  "$STAGES" "rung $X64 mes stage0-pe32.kaem $H1 $KAEM" "rung $X64 mes stage0-pe32.kaem $H1 $KAEM" "rung $X64 gcc46 gcc-core $H2 $GCC" -- \
  "$STAGES" "rung $ARM64 mes stage0-pe32.kaem $H1 $KAEM" "rung $ARM64 gcc46 gcc-core $H2 $GCC"
expect 1 "::error::stage0-pe32.kaem (mes): recorded twice on $X64"

records "records whose stages lines differ fail" \
  "$STAGES" "rung $X64 mes stage0-pe32.kaem $H1 $KAEM" "rung $X64 gcc46 gcc-core $H2 $GCC" -- \
  "stages mes" "rung $ARM64 mes stage0-pe32.kaem $H1 $KAEM" "rung $ARM64 gcc46 gcc-core $H2 $GCC"
expect 1 "::error::the records name different stages"

records "a record of a leg not compared fails" \
  "$STAGES" "rung $X64 mes stage0-pe32.kaem $H1 $KAEM" "rung $X64 gcc46 gcc-core $H2 $GCC" -- \
  "$STAGES" "rung $ARM64 mes stage0-pe32.kaem $H1 $KAEM" "rung $ARM64 gcc46 gcc-core $H2 $GCC" "rung windows-2022 mes stage0-pe32.kaem $H1 $KAEM"
expect 1 "::error::a record names windows-2022, which is not a leg compared here"

records "a line the comparison does not read fails" \
  "$STAGES" "rung $X64 mes stage0-pe32.kaem $H1 $KAEM" "rung $X64 gcc46 gcc-core $H2 $GCC" "rung $X64 gcc46 gcc-core" -- \
  "$STAGES" "rung $ARM64 mes stage0-pe32.kaem $H1 $KAEM" "rung $ARM64 gcc46 gcc-core $H2 $GCC"
expect 1 "::error::a record line this script does not read: rung $X64 gcc46 gcc-core"

records "records with no stages line fail" \
  "rung $X64 mes stage0-pe32.kaem $H1 $KAEM" -- \
  "rung $ARM64 mes stage0-pe32.kaem $H1 $KAEM"
expect 1 "::error::no record names the stages"

label="no records at all fail"
rm -rf "$work/records"
mkdir "$work/records"
expect 1 "holds no record of a rung's NAR hash"

echo "$checked cases, $failures failed"
[ "$failures" -eq 0 ]
