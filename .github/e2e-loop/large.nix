# Memory-gate fixture: a NAR far larger than the substituter's RTS heap
# cap, so any reintroduced whole-archive retention aborts the gate instead
# of shipping.  The ballast is zeros: compressibility keeps the zstd and
# hashing legs cheap enough for CI while the DECOMPRESSED stream - the
# side a buffering regression would hold - is still hundreds of MB.
#
# The nonce serves the same purpose as in small.nix: a rebuild cannot
# reproduce it, so comparing it across the two stores proves the bytes
# crossed the wire.
let
  ballastMiB = 320;
in
derivation {
  name = "e2e-loop-large";
  system = builtins.currentSystem;
  builder = "/bin/sh";
  # A build's PATH is /path-not-set unless its derivation sets one, as
  # upstream's is, so the tools the script runs are named here.
  PATH = "/usr/bin:/bin";
  args = [
    "-c"
    ''
      set -eu
      mkdir -p "$out"
      dd if=/dev/zero of="$out/ballast" bs=1048576 count=${toString ballastMiB} 2>/dev/null
      head -c 16 /dev/urandom | od -An -tx1 | tr -d ' \n' > "$out/nonce"
    ''
  ];
}
