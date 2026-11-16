#!/usr/bin/env bash
# Exit 0 when a GHC compiles a program, links it with the toolchain the
# runner has now, and runs it.  ghcup configures an installation for the
# toolchain of the image it is made on, which a later image can change, and
# `ghc --numeric-version` runs without linking anything.
#
# Usage: ghc-links.sh GHC
#   GHC   the compiler to try
#
# Portable to bash 3.2, which is what macOS ships.
set -euo pipefail

ghc=${1:?the compiler to try}

work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
printf 'main :: IO ()\nmain = putStrLn "linked"\n' > "$work/Main.hs"
"$ghc" -outputdir "$work" -o "$work/linked" "$work/Main.hs"
[ "$("$work/linked")" = linked ]
