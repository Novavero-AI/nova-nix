#!/usr/bin/env bash
# What no-build-path.sh decides for programs written to name, or not to
# name, the store directory and their build directory.  CI runs these
# before the checks that use it, since a check that cannot fail is a run
# that tests nothing and stays green.
#
# Needs cygpath, as no-build-path.sh does: Git for Windows' bash.
#
# No set -e: a mismatch is the output, not a reason to stop before the rest
# of the cases have run.
set -uo pipefail

script=$(dirname "${BASH_SOURCE[0]}")/no-build-path.sh
root=$(mktemp -d)
trap 'rm -rf "$root"' EXIT

# A store path's hash is 32 characters of Nix's base-32 alphabet.
hash=0123456789abcdfghijklmnpqrsvwxyz
out="$root/nix/store/$hash-case"
mkdir -p "$out/bin"
store=$(cygpath -m "$root/nix/store")
storeBackslashed=$(cygpath -w "$root/nix/store")

checked=0
failures=0

# expect WANT LABEL CONTENTS: no-build-path.sh's status for a program
# holding CONTENTS between NULs.
expect() {
  printf 'MZ\0%s\0' "$3" > "$out/bin/program.exe"
  bash "$script" case "$(cygpath -w "$out")" bin/program.exe > /dev/null 2>&1
  report "$1" "$?" "$2"
}

# report WANT GOT LABEL
report() {
  checked=$((checked + 1))
  if [ "$2" = "$1" ]; then
    printf 'ok    %s  %s\n' "$2" "$3"
  else
    printf 'FAIL  %s  %s (expected %s)\n' "$2" "$3" "$1"
    failures=$((failures + 1))
  fi
}

expect 1 "the store directory with slashes" "$store/x.h"
expect 1 "the store directory with backslashes" "$storeBackslashed\\x.h"
expect 1 "the store directory with backslashes, then a slash" "$storeBackslashed/x.h"
expect 1 "the store directory in upper case" "$(tr '[:lower:]' '[:upper:]' <<<"$store")/x.h"
expect 1 "the build directory, then a slash" "C:/Users/u/Temp/$hash/src/a.c"
expect 1 "the build directory, then a backslash" "C:\\Users\\u\\Temp\\$hash\\src\\a.c"
expect 0 "the store path in its logical spelling" "/nix/store/$hash-case"
expect 0 "neither" "Hello, world!"

rm -f "$out/bin/program.exe"
bash "$script" case "$(cygpath -w "$out")" bin/program.exe > /dev/null 2>&1
report 1 "$?" "a program that is not there"

echo "$checked cases, $failures failed"
[ "$failures" -eq 0 ]
