# nova-nix stage-2 stdenv: writes the stdenv's compiler and linker
# wrappers into $out/bin, from the templates cc-wrapper.sh and
# ld-wrapper.sh.
#
# Run by busybox-w32's sh as the builder of default.nix's cc, which passes:
#   $compilers   gcc-native-x86_64's bin directory
#   $linker      binutils-native-x86_64's ld.exe
#   $specs       link.specs
#   $ccWrapper   the template of gcc, cc, g++ and c++
#   $ldWrapper   the template of ld
#
# Each wrapper is a script that begins with #!/bin/sh, by which busybox
# and GNU make run it, whatever its mode (default.nix says how).
set -e

# The builder spells the store C:\nix\store, and sed reads a backslash in a
# replacement as an escape, so the paths are written into the wrappers
# with forward slashes, which every Windows API takes as well.
slashed() {
  printf '%s\n' "$1" | sed 's|\\|/|g'
}
compilers=$(slashed "$compilers")
linker=$(slashed "$linker")
specs=$(slashed "$specs")

# The words between @ signs are replaced on a template's line that begins
# with exec alone, since its comments name them too.
mkdir -p "$out/bin"
# gcc and cc compile C at -std=gnu17, and g++ and c++ C++ at the
# compiler's default (cc-wrapper.sh says why).
for name in gcc cc; do
  sed -e "/^exec /s|@driver@|$compilers/gcc.exe|" -e '/^exec /s|@std@|-std=gnu17|' -e "/^exec /s|@specs@|$specs|" "$ccWrapper" > "$out/bin/$name"
done
for name in g++ c++; do
  sed -e "/^exec /s|@driver@|$compilers/g++.exe|" -e '/^exec /s| @std@||' -e "/^exec /s|@specs@|$specs|" "$ccWrapper" > "$out/bin/$name"
done
sed -e "/^exec /s|@linker@|$linker|" "$ldWrapper" > "$out/bin/ld"
