#!/usr/bin/env bash
# profiling/bench.sh — Reproducible benchmark runner for nova-nix memory profiling
#
# Usage:
#   ./profiling/bench.sh <label> [rts-flags...]
#
# Examples:
#   ./profiling/bench.sh baseline                        # Just -s stats, -M4G
#   ./profiling/bench.sh iter6-trim -hT                  # Type-based heap profile
#   ./profiling/bench.sh iter6-trim -hc                  # Cost-centre (needs -prof build)
#   ./profiling/bench.sh iter6-trim -hr                  # Retainer (needs -prof build)
#   ./profiling/bench.sh scaling -M6G                    # Custom heap cap
#   ./profiling/bench.sh iter6-trim -hT -M8G             # Combine flags
#
# Output:
#   profiling/results/<label>/
#     rts-stats.txt    — full +RTS -s output
#     summary.txt      — parsed key metrics
#     heap-profile.hp  — .hp file (if heap profiling enabled)
#     heap-top.txt     — top 20 heap categories (if .hp present)
#
# The script always adds +RTS -s and the heap cap (-M4G default).
# It does NOT build — build separately before running.

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
NIXPKGS_PATH="C:\\Users\\devon\\nixpkgs-nixos-24.11"
EVAL_EXPR='builtins.length (builtins.attrNames (import <nixpkgs> {}))'
DEFAULT_HEAP_CAP="-M4G"

# --- Args ---
if [[ $# -lt 1 ]]; then
  echo "Usage: $0 <label> [rts-flags...]"
  echo "  label: name for this run (e.g. 'baseline', 'iter6-trim')"
  echo "  rts-flags: additional +RTS flags (e.g. -hT, -hc, -hr, -M8G)"
  exit 1
fi

LABEL="$1"
shift
RTS_EXTRA=("$@")

# Check if a custom -M flag was passed
HEAP_CAP="$DEFAULT_HEAP_CAP"
for flag in "${RTS_EXTRA[@]+"${RTS_EXTRA[@]}"}"; do
  if [[ "$flag" == -M* ]]; then
    HEAP_CAP="$flag"
  fi
done

# Remove any -M flags from extras (we add our own)
RTS_FILTERED=()
for flag in "${RTS_EXTRA[@]+"${RTS_EXTRA[@]}"}"; do
  if [[ "$flag" != -M* ]]; then
    RTS_FILTERED+=("$flag")
  fi
done

# --- Setup output dir ---
RESULTS_DIR="$REPO_ROOT/profiling/results/$LABEL"
mkdir -p "$RESULTS_DIR"

# --- Find executable ---
EXE=$(cd "$REPO_ROOT" && cabal list-bin nova-nix 2>/dev/null)
if [[ ! -f "$EXE" ]]; then
  echo "ERROR: nova-nix executable not found at: $EXE"
  echo "Run 'cabal build' first."
  exit 1
fi
echo "Executable: $EXE"

# --- Build RTS flags ---
RTS_FLAGS=("-s" "$HEAP_CAP" "${RTS_FILTERED[@]+"${RTS_FILTERED[@]}"}")
echo "RTS flags: ${RTS_FLAGS[*]}"
echo "Heap cap: $HEAP_CAP"
echo "Output: $RESULTS_DIR/"
echo ""

# --- Record metadata ---
cat > "$RESULTS_DIR/meta.txt" <<EOF
label: $LABEL
date: $(date -Iseconds)
exe: $EXE
heap_cap: $HEAP_CAP
rts_flags: ${RTS_FLAGS[*]}
expr: $EVAL_EXPR
nixpkgs: $NIXPKGS_PATH
git_commit: $(cd "$REPO_ROOT" && git rev-parse --short HEAD)
git_dirty: $(cd "$REPO_ROOT" && git diff --quiet && echo "no" || echo "yes")
EOF

# --- Run benchmark ---
echo "Running benchmark..."
echo ""

# GHC always writes .hp to cwd as <exe-name>.hp. Clean before, move after.
HP_CWD="$REPO_ROOT/nova-nix.hp"
rm -f "$HP_CWD"

cd "$REPO_ROOT"
set +e
NIX_PATH="nixpkgs=$NIXPKGS_PATH" \
  "$EXE" eval --expr "$EVAL_EXPR" \
  +RTS "${RTS_FLAGS[@]}" -RTS \
  > "$RESULTS_DIR/eval-output.txt" \
  2> "$RESULTS_DIR/rts-stats.txt"
EXIT_CODE=$?
set -e

echo "Exit code: $EXIT_CODE"

# --- Move .hp file if generated ---
HP_FILE="$RESULTS_DIR/heap-profile.hp"
if [[ -f "$HP_CWD" ]]; then
  mv "$HP_CWD" "$HP_FILE"
  echo "Heap profile saved."

  # Parse top 20 heap categories from .hp file
  "$REPO_ROOT/profiling/parse-hp.sh" "$HP_FILE" > "$RESULTS_DIR/heap-top.txt"
  echo "Heap top 20 parsed."
fi

# --- Move .prof file if generated (profiling builds produce this) ---
PROF_CWD="$REPO_ROOT/nova-nix.prof"
if [[ -f "$PROF_CWD" ]]; then
  mv "$PROF_CWD" "$RESULTS_DIR/cost-centre.prof"
  echo "Cost-centre profile saved."
fi

# --- Parse key metrics from RTS stats ---
STATS_FILE="$RESULTS_DIR/rts-stats.txt"
{
  echo "=== Key Metrics ==="
  echo ""

  # Total allocated
  alloc=$(grep -oP '[\d,]+\s+bytes allocated in the heap' "$STATS_FILE" 2>/dev/null | head -1 || true)
  echo "Allocated: $alloc"

  # Max residency
  resid=$(grep -oP '[\d,]+\s+bytes maximum residency' "$STATS_FILE" 2>/dev/null | head -1 || true)
  echo "Max residency: $resid"

  # Productivity
  prod=$(grep -oP 'Productivity\s+[\d.]+%' "$STATS_FILE" 2>/dev/null | head -1 || true)
  echo "$prod"

  # GC stats
  gc=$(grep -oP 'GC\s+time\s+[\d.]+s' "$STATS_FILE" 2>/dev/null | head -1 || true)
  echo "$gc"

  # Exit code
  echo "Exit code: $EXIT_CODE"

  if [[ $EXIT_CODE -ne 0 ]]; then
    echo ""
    echo "=== Error output (last 5 lines) ==="
    tail -5 "$STATS_FILE"
  fi
} > "$RESULTS_DIR/summary.txt"

echo ""
echo "=== Results ==="
cat "$RESULTS_DIR/summary.txt"
echo ""

# Show heap top if available
if [[ -f "$RESULTS_DIR/heap-top.txt" ]]; then
  echo "=== Heap Top 20 ==="
  cat "$RESULTS_DIR/heap-top.txt"
fi

echo ""
echo "Full results in: $RESULTS_DIR/"
