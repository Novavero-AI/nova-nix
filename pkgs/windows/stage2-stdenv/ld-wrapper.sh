#!/bin/sh
# nova-nix stage-2 stdenv: binutils-native-x86_64's linker, as the
# stdenv's builds run it under the name ld.  wrap.sh writes it from this
# template, with the linker as @linker@.
#
# ld writes the time of the link into the header of the PE image it links,
# SOURCE_DATE_EPOCH where the environment sets it, and 0 with
# --no-insert-timestamp (ld/emultempl/pep.em:188 and 1548-1551,
# bfd/peXXigen.c:850-858 and bfd/bfdio.c:938-963 in binutils 2.46), which
# leaves the image the same whenever, and under whatever
# SOURCE_DATE_EPOCH, it was linked.  The flag precedes the package's
# arguments, so its own --insert-timestamp wins.
exec '@linker@' --no-insert-timestamp "$@"
