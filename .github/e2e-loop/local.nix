# Fixture for the loop's allowSubstitutes case: a derivation that forbids
# substitutes, so a store that lacks its output builds it even when the
# cache holds one.
#
# The nonce is read from /dev/urandom at BUILD time, as small.nix's is: a
# build cannot reproduce the bytes the cache holds, so comparing nonces
# tells a build from a substitution.
derivation {
  name = "e2e-loop-local";
  system = builtins.currentSystem;
  builder = "/bin/sh";
  args = [
    "-c"
    ''
      set -eu
      mkdir -p "$out"
      head -c 16 /dev/urandom | od -An -tx1 | tr -d ' \n' > "$out/nonce"
    ''
  ];
  allowSubstitutes = false;
}
