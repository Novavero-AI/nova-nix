# Identity fixture for the push/substitute loop: one small tree carrying
# every NAR node kind the loop must round-trip - a regular file, an
# executable (the one permission bit NAR encodes, which a lossy unpack
# would drop), a symlink, and a nested directory.
#
# The nonce is read from /dev/urandom at BUILD time.  The derivation hash
# is unaffected (the recipe text is fixed), but a local rebuild cannot
# reproduce the bytes, so the final byte compare distinguishes a real
# substitution from a silent fallback build producing a plausible tree.
derivation {
  name = "e2e-loop-small";
  system = builtins.currentSystem;
  builder = "/bin/sh";
  args = [
    "-c"
    ''
      set -eu
      mkdir -p "$out/bin"
      printf 'payload for the substitute loop\n' > "$out/data.txt"
      head -c 16 /dev/urandom | od -An -tx1 | tr -d ' \n' > "$out/nonce"
      printf '#!/bin/sh\necho hello from the loop\n' > "$out/bin/hello"
      chmod +x "$out/bin/hello"
      ln -s ../data.txt "$out/bin/link-to-data"
    ''
  ];
}
