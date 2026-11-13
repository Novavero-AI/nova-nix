#!/usr/bin/env bash
# Fail if a program a seed ladder rung built names the store directory or
# its own build directory.
#
# tcc's debugging information names the directory tcc ran in and each file
# by the path it opened, a header from the store by the store's physical
# path, so a program that carried it would differ with the store's drive
# and with the user whose temporary directory the build ran in.  The store
# directory is matched with either separator, since tcc writes some of
# those paths with backslashes and some with slashes, and without regard
# to case, as Windows compares paths.  The build directory is matched by
# OUT's hash and a separator: the builder names the directory after the
# hash of the derivation's first output, which for the single-output
# programs checked here is OUT's, where a store path follows a hash with a
# hyphen.  no-build-path-cases.sh shows each of these matches failing.
#
# Usage: no-build-path.sh LABEL OUT PROGRAM
#   LABEL    what the messages call the output
#   OUT      the output's store path, as nova-nix build prints it
#   PROGRAM  the file to read, relative to OUT
set -uo pipefail

label=$1
path=$(cygpath -m "$2")
program=$3
store=${path%/*}
base=${path##*/}
hash=${base%%-*}

found=$(grep -aoiF -e "$store" -e "$(cygpath -w "$store")" -e "$hash/" -e "$hash\\" "$path/$program")
case $? in
  0)
    echo "::error::$label's $program names the store or its build directory $(wc -l <<<"$found") times, most often:"
    sort <<<"$found" | uniq -c | sort -rn | head -n 3
    exit 1
    ;;
  1) echo "ok    $label's $program names neither" ;;
  *)
    echo "::error::$label's $program could not be read"
    exit 1
    ;;
esac
