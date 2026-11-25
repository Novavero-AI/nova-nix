#!/bin/sh
# nova-nix stdenv: binutils-native-x86_64's linker, as the
# stdenv's builds run it under the name ld.  wrap.sh writes it from this
# template, with the linker as @linker@.
#
# ld writes the time of the link into the header of the PE image it links,
# SOURCE_DATE_EPOCH where the environment sets it, and 0 with
# --no-insert-timestamp, which leaves the image the same whenever, and
# under whatever SOURCE_DATE_EPOCH, it was linked.  The flag precedes the
# package's arguments, so its own --insert-timestamp wins.
exec '@linker@' --no-insert-timestamp "$@"
