#!/usr/bin/env bash
# Serve a stage's handoff on the runner's loopback: the binary cache in
# which each stage of a Windows leg of the seed ladder hands the store
# paths it built to the stage after it.
#
# Each stage's job runs on a runner of its own and starts from the store
# paths the job before it built.  nova-nix takes a store path in from a
# binary cache alone, as a narinfo and a NAR, and gives one out by pushing
# to a nova-cache server, so the handoff is a nova-cache server's
# directory: a job serves the one the job before it uploaded, substitutes
# from it what its builds reach, pushes its own store to it and uploads it
# for the job after it.  A NAR keeps what a copy of the store directory
# would lose: the executable bit, which nova-nix keeps on Windows in an
# NTFS alternate data stream, and which an artifact's zip does not carry.
#
# The server signs what is pushed with the handoff's own key, which the
# first stage makes and the handoff carries.  It vouches for nothing past
# one leg of one run: the server listens on the loopback alone, and no job
# but that leg's serves the files it signed.
#
# Usage: handoff-serve.sh SERVER DIR
#   SERVER   the nova-cache-server executable
#   DIR      the handoff: the server's files in DIR/cache and its signing
#            key in DIR/signing.key, each made when absent.  The server's
#            log is DIR.log and its write key DIR.api-key, beside DIR and
#            so outside what a job uploads.
# Required environment:
#   GITHUB_ENV   the file a step appends to, to set the environment of the
#                job's later steps.  Set there:
#                  HANDOFF_URL            the server's address
#                  HANDOFF_PUBLIC_KEY     the key it signs with
#                  HANDOFF_API_KEY_FILE   the file holding its write key
#                  HANDOFF_SERVER_PID     its Windows process ID
set -euo pipefail

server=${1:?the nova-cache-server executable}
dir=${2:?the handoff directory}
dir=${dir%/}
: "${GITHUB_ENV:?}"

readonly PORT=18740
readonly URL="http://127.0.0.1:$PORT"
readonly KEY_NAME=ladder-handoff
# The write key guards a server on the loopback of a runner that one job
# has, so it is a fixed word and no secret.
readonly API_KEY=ladder-handoff-write
# The libsodium secret-key layout, a seed and its public half: nova-cache
# derives the public half from the seed, so 64 random bytes are a key, as
# .github/e2e-loop/loop.sh makes its own.
readonly KEY_BYTES=64
# The server's first answer is waited for a second at a time: curl
# retries a refused connection under --retry-connrefused.  It prints no
# error of its own, as it would for each refused attempt before the one
# that is answered; a server that never answers prints its log here.
readonly READY_ATTEMPTS=60
readonly READY_DELAY_SECONDS=1

keyfile="$dir/signing.key"
log="$dir.log"
api_key_file="$dir.api-key"

mkdir -p "$dir/cache"
if [ ! -e "$keyfile" ]; then
  printf '%s:%s' "$KEY_NAME" "$(head -c "$KEY_BYTES" /dev/urandom | base64 | tr -d '\n')" > "$keyfile"
fi
printf '%s' "$API_KEY" > "$api_key_file"

# Output to the log and no input: a process holding the step's pipes would
# keep the step from ending, as the runner waits for them to close.  The
# server outlives the step, and the job's last step stops it.  A server
# that never answers is left to the runner, which ends every process a job
# started when the job ends.
CACHE_API_KEY=$API_KEY SIGNING_KEY_FILE=$keyfile \
  "$server" --host 127.0.0.1 --port "$PORT" --store "$dir/cache" > "$log" 2>&1 < /dev/null &

if ! curl --fail --silent --output /dev/null \
  --retry "$READY_ATTEMPTS" --retry-delay "$READY_DELAY_SECONDS" --retry-connrefused \
  "$URL/nix-cache-info"; then
  cat "$log" >&2
  echo "::error::the handoff server did not answer at $URL" >&2
  exit 1
fi

# The server prints the key it signs with as it starts, so the steps after
# this one trust the key it uses rather than a copy that could differ.  On
# Windows it ends each line with a carriage return.
public_key=$(sed -n 's/^public key: //p' "$log" | head -n 1 | tr -d '\r')
if [ -z "$public_key" ]; then
  cat "$log" >&2
  echo "::error::the handoff server's log names no public key" >&2
  exit 1
fi

# The server's Windows process ID, which the step that stops it takes, is
# that of the process listening on the port: the ID the shell gives a job
# it starts is MSYS2's own.
if ! server_pid=$(pwsh -NoProfile -Command "(Get-NetTCPConnection -LocalPort $PORT -State Listen).OwningProcess" | tr -d '\r') ||
  ! [[ $server_pid =~ ^[0-9]+$ ]]; then
  echo "::error::no one process is listening on port $PORT: $server_pid" >&2
  exit 1
fi

{
  echo "HANDOFF_URL=$URL"
  echo "HANDOFF_PUBLIC_KEY=$public_key"
  echo "HANDOFF_API_KEY_FILE=$api_key_file"
  echo "HANDOFF_SERVER_PID=$server_pid"
} >> "$GITHUB_ENV"
echo "serving $dir/cache at $URL, signed by $public_key"
