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
#   5. pushes a fixture that forbids substitutes, and asserts a store that
#      lacks it builds it, and substitutes it under always-allow-substitutes
#   6. checks a substituted output against a rebuild (build --check): one
#      that builds the same every time passes, and one that does not fails
#      with status 104 and leaves the store's copy alone
#   7. repeats with a multi-hundred-MB fixture, running the substitution
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

# What nova-nix build --check exits with when a rebuilt output differs,
# upstream's status for it.
CHECK_MISMATCH_STATUS=104

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
STORE_C="$WORK/store-c"
STORE_D="$WORK/store-d"
LOOP_STORE="$WORK/cache-loop"
DUMP_A_STORE="$WORK/cache-dump-a"
DUMP_B_STORE="$WORK/cache-dump-b"
LOG_DIR="$WORK/logs"
mkdir -p "$STORE_A" "$STORE_B" "$STORE_C" "$STORE_D" "$LOG_DIR"

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
# allowSubstitutes = false: built where the cache holds the output
# --------------------------------------------------------------------------

echo "== local fixture: push, build beside the cache, substitute under always-allow-substitutes =="
OUT_LOCAL_A=$(build_into "$STORE_A" local.nix)
echo "store A path: $OUT_LOCAL_A"
push_from "$STORE_A" "$LOOP_URL" zstd "$OUT_LOCAL_A"

# The cache holds the output and is named as a substituter, and the
# derivation forbids substitutes, so store B builds it.
LOCAL_LOG="$LOG_DIR/build-local.nix.log"
OUT_LOCAL_B=$("$NOVA_NIX_BIN" build "$SCRIPT_DIR/local.nix" --store "$STORE_B" \
  --substituter "$LOOP_URL" --trusted-key "$PUB_KEY" 2> "$LOCAL_LOG" | tail -n 1) || {
  cat "$LOCAL_LOG" >&2
  fail "local.nix failed in store B"
}
echo "store B path: $OUT_LOCAL_B"
grep -Fq '[build]' "$LOCAL_LOG" || {
  cat "$LOCAL_LOG" >&2
  fail "local.nix was not built"
}
! grep -Fq '[subst]' "$LOCAL_LOG" || {
  cat "$LOCAL_LOG" >&2
  fail "local.nix was substituted, and it forbids substitutes"
}
# cmp answers "differ" for a file it cannot read, so each nonce is checked
# to be there before the two are compared.
for nonce in "$OUT_LOCAL_A/nonce" "$OUT_LOCAL_B/nonce"; do
  [ -s "$nonce" ] || fail "no nonce at $nonce"
done
if cmp -s "$OUT_LOCAL_A/nonce" "$OUT_LOCAL_B/nonce"; then
  fail "local nonce is the cache's: store B substituted instead of building"
fi

# The setting overrides the attribute, so a third store takes the output
# from the cache, which shows the cache did hold it for store B to refuse.
OUT_LOCAL_C=$(NIX_CONFIG='always-allow-substitutes = true' substitute_into "$STORE_C" local.nix)
echo "store C path: $OUT_LOCAL_C"
cmp "$OUT_LOCAL_A/nonce" "$OUT_LOCAL_C/nonce" \
  || fail "local nonce differs under always-allow-substitutes: store C rebuilt"

# --------------------------------------------------------------------------
# build --check: a substituted output held to a rebuild
# --------------------------------------------------------------------------

echo "== stable fixture: push, then substitute and check in one build --check =="
OUT_STABLE_A=$(build_into "$STORE_A" stable.nix)
echo "store A path: $OUT_STABLE_A"
push_from "$STORE_A" "$LOOP_URL" zstd "$OUT_STABLE_A"

# Store D holds nothing, so the check first substitutes the output and
# then builds it again beside the substituted one: both tags are in the
# log, and the ordinary build's is not.
STABLE_LOG="$LOG_DIR/check-stable.nix.log"
OUT_STABLE_D=$("$NOVA_NIX_BIN" build "$SCRIPT_DIR/stable.nix" --check --store "$STORE_D" \
  --substituter "$LOOP_URL" --trusted-key "$PUB_KEY" 2> "$STABLE_LOG" | tail -n 1) || {
  cat "$STABLE_LOG" >&2
  fail "the check of stable.nix failed in store D"
}
echo "store D path: $OUT_STABLE_D"
grep -Fq '[subst]' "$STABLE_LOG" || {
  cat "$STABLE_LOG" >&2
  fail "stable.nix was not substituted before the check"
}
grep -Fq '[check]' "$STABLE_LOG" || {
  cat "$STABLE_LOG" >&2
  fail "stable.nix was not built again and compared"
}
! grep -Fq '[build]' "$STABLE_LOG" || {
  cat "$STABLE_LOG" >&2
  fail "stable.nix was built as an ordinary build, not checked"
}
[ "$(basename "$OUT_STABLE_A")" = "$(basename "$OUT_STABLE_D")" ] \
  || fail "the check printed $(basename "$OUT_STABLE_D"), not the output path"
# The output records its own base name, so the rebuild, which wrote the
# scratch path's, matched only once the check had rewritten that hash.
[ "$(cat "$OUT_STABLE_D/self")" = "$(basename "$OUT_STABLE_D")" ] \
  || fail "stable.nix does not record its own base name in store D"

echo "== small fixture: a check of the substituted output finds the rebuild different =="
# small.nix reads its nonce from /dev/urandom, so no rebuild reproduces the
# bytes the cache served store B: the check has to fail with its own
# status, name the output, and leave store B holding what it substituted.
SMALL_CHECK_LOG="$LOG_DIR/check-small.nix.log"
small_check_status=0
"$NOVA_NIX_BIN" build "$SCRIPT_DIR/small.nix" --check --store "$STORE_B" \
  > /dev/null 2> "$SMALL_CHECK_LOG" || small_check_status=$?
[ "$small_check_status" -eq "$CHECK_MISMATCH_STATUS" ] || {
  cat "$SMALL_CHECK_LOG" >&2
  fail "the check of small.nix exited $small_check_status, not $CHECK_MISMATCH_STATUS"
}
grep -Fq "may not be deterministic: output '$OUT_B' differs" "$SMALL_CHECK_LOG" || {
  cat "$SMALL_CHECK_LOG" >&2
  fail "the check of small.nix did not name the output that differs"
}
cmp "$OUT_A/nonce" "$OUT_B/nonce" || fail "the check changed the output store B substituted"

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
