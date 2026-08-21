#!/usr/bin/env bash
# End-to-end push/substitute loop against a real nova-cache-server.
#
# The unit suite never opens a socket, so chunked transfer encoding,
# connection reuse, the signing handshake, and the nova-nix/nova-cache
# interop only get exercised here.  The loop:
#
#   1. builds a small fixture into scratch store A
#   2. pushes it (--compression zstd, authenticated) to a signing server
#   3. substitutes it into fresh scratch store B with signature
#      verification on
#   4. asserts the two trees byte-identical via NAR serialisation compare
#   5. repeats with a multi-hundred-MB fixture, running the substitution
#      under a hard RTS heap cap so whole-archive retention aborts
#
# Required environment:
#   NOVA_NIX_BIN           the nova-nix executable under test
#   NOVA_CACHE_SERVER_BIN  a nova-cache-server executable
# Optional:
#   E2E_WORK_DIR           scratch root (default: a fresh mktemp dir)
set -euo pipefail

# --------------------------------------------------------------------------
# Constants
# --------------------------------------------------------------------------

# High ports, unlikely to collide with runner services; three servers
# because the push planner skips any store-path hash the target cache
# already holds, so each tree that must be serialised independently
# needs its own cache store (see the NAR compare below).
LOOP_PORT=18730
DUMP_A_PORT=18731
DUMP_B_PORT=18732

KEY_NAME="e2e-loop"
API_KEY="e2e-loop-write-key"

# The substituter must stay bounded regardless of archive size; the cap
# is far below the large fixture's NAR, so buffering the archive (or any
# whole file in it) aborts the run instead of passing on a big runner.
RTS_HEAP_CAP="-M128m"

READY_ATTEMPTS=100
READY_SLEEP_SECONDS=0.2

SCRIPT_DIR=$(cd "$(dirname "$0")" && pwd)

fail() {
  echo "FAIL: $*" >&2
  exit 1
}

[ -x "${NOVA_NIX_BIN:-}" ] || fail "NOVA_NIX_BIN is not an executable: ${NOVA_NIX_BIN:-unset}"
[ -x "${NOVA_CACHE_SERVER_BIN:-}" ] || fail "NOVA_CACHE_SERVER_BIN is not an executable: ${NOVA_CACHE_SERVER_BIN:-unset}"

WORK=${E2E_WORK_DIR:-$(mktemp -d)}
STORE_A="$WORK/store-a"
STORE_B="$WORK/store-b"
LOOP_STORE="$WORK/cache-loop"
DUMP_A_STORE="$WORK/cache-dump-a"
DUMP_B_STORE="$WORK/cache-dump-b"
LOG_DIR="$WORK/logs"
mkdir -p "$STORE_A" "$STORE_B" "$LOG_DIR"

LOOP_URL="http://127.0.0.1:$LOOP_PORT"
DUMP_A_URL="http://127.0.0.1:$DUMP_A_PORT"
DUMP_B_URL="http://127.0.0.1:$DUMP_B_PORT"

# --------------------------------------------------------------------------
# Keys
# --------------------------------------------------------------------------

# The signing key uses the libsodium secret-key layout (seed || public
# half) that nix-store --generate-binary-cache-key emits.  nova-cache
# re-derives the public half from the seed for both signing and the
# published trust anchor, so the trailing 32 bytes are inert and 64
# random bytes form a valid key without any Ed25519 tooling here.
SIGNING_KEY_FILE="$WORK/signing.key"
printf '%s:%s' "$KEY_NAME" "$(openssl rand 64 | base64 | tr -d '\n')" > "$SIGNING_KEY_FILE"

API_KEY_FILE="$WORK/api.key"
printf '%s' "$API_KEY" > "$API_KEY_FILE"

# --------------------------------------------------------------------------
# Servers
# --------------------------------------------------------------------------

SERVER_PIDS=""
cleanup() {
  for pid in $SERVER_PIDS; do
    kill "$pid" 2>/dev/null || true
  done
  wait 2>/dev/null || true
}
trap cleanup EXIT

start_server() {
  port=$1
  store=$2
  log=$3
  CACHE_API_KEY="$API_KEY" SIGNING_KEY_FILE="$SIGNING_KEY_FILE" \
    "$NOVA_CACHE_SERVER_BIN" --host 127.0.0.1 --port "$port" --store "$store" \
    > "$log" 2>&1 &
  SERVER_PIDS="$SERVER_PIDS $!"
}

wait_ready() {
  url=$1
  attempt=0
  until curl -fs -o /dev/null "$url/"; do
    attempt=$((attempt + 1))
    [ "$attempt" -lt "$READY_ATTEMPTS" ] || fail "server at $url never became ready"
    sleep "$READY_SLEEP_SECONDS"
  done
}

echo "== starting three nova-cache-server instances =="
start_server "$LOOP_PORT" "$LOOP_STORE" "$LOG_DIR/loop-server.log"
start_server "$DUMP_A_PORT" "$DUMP_A_STORE" "$LOG_DIR/dump-a-server.log"
start_server "$DUMP_B_PORT" "$DUMP_B_STORE" "$LOG_DIR/dump-b-server.log"
wait_ready "$LOOP_URL"
wait_ready "$DUMP_A_URL"
wait_ready "$DUMP_B_URL"

# The server derives and prints the trust anchor at startup; reading it
# back from the log keeps the client verifying against the exact key the
# server signs with, not a copy this script could let drift.
PUB_KEY=$(sed -n 's/^public key: //p' "$LOG_DIR/loop-server.log" | head -n 1)
[ -n "$PUB_KEY" ] || fail "could not read the public key from the loop server log"
echo "trusted key: $PUB_KEY"

# --------------------------------------------------------------------------
# Loop plumbing
# --------------------------------------------------------------------------

# Build FIXTURE into STORE and echo the resulting output path.
build_into() {
  store=$1
  fixture=$2
  "$NOVA_NIX_BIN" build "$SCRIPT_DIR/$fixture" --store "$store" | tail -n 1
}

push_from() {
  store=$1
  url=$2
  compression=$3
  path=$4
  "$NOVA_NIX_BIN" push --store "$store" --cache "$url" \
    --key-file "$API_KEY_FILE" --compression "$compression" "$path"
}

# Substitute FIXTURE into STORE from the loop cache; extra arguments are
# appended to the invocation (the memory gate passes the RTS cap), and
# MEASURE_PREFIX (a word-split command prefix, /usr/bin/time when set)
# wraps the process so its report lands in the same stderr log.
# Asserts the path arrived by substitution, never by the local-build
# fallback, which would produce a fresh (nonce-differing) tree.
MEASURE_PREFIX=""
substitute_into() {
  store=$1
  fixture=$2
  shift 2
  stderr_log="$LOG_DIR/substitute-$fixture.log"
  # shellcheck disable=SC2086
  $MEASURE_PREFIX "$NOVA_NIX_BIN" build "$SCRIPT_DIR/$fixture" --store "$store" \
    --substituter "$LOOP_URL" --trusted-key "$PUB_KEY" "$@" \
    2> "$stderr_log" | tail -n 1
  grep -Fq '[subst]' "$stderr_log" || {
    cat "$stderr_log" >&2
    fail "$fixture did not substitute"
  }
  ! grep -Fq '[build]' "$stderr_log" || {
    cat "$stderr_log" >&2
    fail "$fixture fell back to a local build"
  }
}

# The one NAR file a dump cache holds after a single-path push.
sole_nar() {
  store=$1
  set -- "$store"/nar/*
  [ "$#" -eq 1 ] && [ -f "$1" ] || fail "expected exactly one NAR in $store/nar"
  echo "$1"
}

# --------------------------------------------------------------------------
# Identity loop: small fixture
# --------------------------------------------------------------------------

echo "== small fixture: build, push (zstd), substitute, compare =="
OUT_A=$(build_into "$STORE_A" small.nix)
echo "store A path: $OUT_A"

push_from "$STORE_A" "$LOOP_URL" zstd "$OUT_A"

# The wire really must be zstd: a server that stored the artifact as
# identity would still round-trip, hiding the compression path.
PATH_HASH=$(basename "$OUT_A" | cut -d- -f1)
grep -q '^Compression: zstd' "$LOOP_STORE/narinfo/$PATH_HASH"* \
  || fail "loop cache narinfo does not declare zstd"

OUT_B=$(substitute_into "$STORE_B" small.nix)
echo "store B path: $OUT_B"

# NAR serialisation compare: pushing each tree with --compression none
# stores its raw NAR (serialised independently from each store's disk,
# executable bit and symlinks included), so comparing the two cache
# objects byte for byte is the tree-identity assert.
push_from "$STORE_A" "$DUMP_A_URL" none "$OUT_A"
push_from "$STORE_B" "$DUMP_B_URL" none "$OUT_B"
NAR_A=$(sole_nar "$DUMP_A_STORE")
NAR_B=$(sole_nar "$DUMP_B_STORE")
[ "$(basename "$NAR_A")" = "$(basename "$NAR_B")" ] \
  || fail "NAR object names differ: $(basename "$NAR_A") vs $(basename "$NAR_B")"
cmp "$NAR_A" "$NAR_B" || fail "NAR serialisations differ between the two stores"
echo "NAR compare: identical ($(wc -c < "$NAR_A" | tr -d ' ') bytes)"

# Direct spot checks on the substituted tree, so a failure names the
# regressed property instead of just "NARs differ".
cmp "$OUT_A/nonce" "$OUT_B/nonce" || fail "nonce differs: store B rebuilt instead of substituting"
[ -x "$OUT_B/bin/hello" ] || fail "executable bit lost in substitution"
[ -L "$OUT_B/bin/link-to-data" ] || fail "symlink lost in substitution"

# --------------------------------------------------------------------------
# Memory gate: large fixture under a hard RTS heap cap
# --------------------------------------------------------------------------

echo "== large fixture: push (zstd), substitute under +RTS $RTS_HEAP_CAP =="
OUT_LARGE_A=$(build_into "$STORE_A" large.nix)
echo "store A path: $OUT_LARGE_A"
push_from "$STORE_A" "$LOOP_URL" zstd "$OUT_LARGE_A"

# GNU/BSD time reports the peak RSS the RTS cap cannot: the cap bounds
# the Haskell heap, and the measurement shows what the whole process
# (heap, decoder buffers, RTS overhead) actually held.  Informational
# only - the gate is the cap - so a missing /usr/bin/time downgrades to
# a note, never a failure.
if [ -x /usr/bin/time ]; then
  case "$(uname)" in
    Darwin) MEASURE_PREFIX="/usr/bin/time -l" ;;
    *) MEASURE_PREFIX="/usr/bin/time -v" ;;
  esac
fi

OUT_LARGE_B=$(substitute_into "$STORE_B" large.nix +RTS "$RTS_HEAP_CAP" -RTS)
echo "store B path: $OUT_LARGE_B"
if [ -n "$MEASURE_PREFIX" ]; then
  echo "peak memory of the substituting process:"
  grep -i 'maximum resident' "$LOG_DIR/substitute-large.nix.log" \
    || echo "  (no RSS line found; see $LOG_DIR/substitute-large.nix.log)"
else
  echo "peak memory: /usr/bin/time unavailable, not measured"
fi

cmp "$OUT_LARGE_A/nonce" "$OUT_LARGE_B/nonce" || fail "large nonce differs: store B rebuilt"
cmp "$OUT_LARGE_A/ballast" "$OUT_LARGE_B/ballast" || fail "large ballast differs after substitution"

echo "== loop OK =="
