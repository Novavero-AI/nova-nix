# Fixture for the loop's --check case: a derivation whose build comes out
# the same every time, so building it again reproduces the bytes the cache
# holds.
#
# It records the base name of its own path.  A check builds beside the
# valid output, so the builder is handed a scratch path, and the check has
# to rewrite that path's hash to the output's own before the two trees
# compare equal.  Only the base name, because a builder is handed its
# output under the directory its store's files are in, and each of the
# loop's stores is in a directory of its own: the whole path would differ
# between the store that pushed the output and the one that checks it.
derivation {
  name = "e2e-loop-stable";
  system = builtins.currentSystem;
  builder = "/bin/sh";
  args = [
    "-c"
    ''
      set -eu
      mkdir -p "$out"
      printf 'the same bytes every build\n' > "$out/data.txt"
      printf '%s\n' "''${out##*/}" > "$out/self"
    ''
  ];
}
