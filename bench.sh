#!/bin/bash
# Quick benchmark pipeline for nova-nix eval performance.
# Usage: bash bench.sh [label]
# Runs stress.nix + nixpkgs with -M4G cap, prints key metrics.
# Safe: hard 4GB memory cap, 120s timeout.

set -euo pipefail

LABEL="${1:-baseline}"
EXE=$(cabal list-bin nova-nix 2>/dev/null)
NIXPKGS="${NIXPKGS_PATH:-}"
TIMEOUT_SEC=120

echo "=== [$LABEL] ==="

# --- Test 1: stress.nix (fast, repeatable) ---
echo ""
echo "--- stress.nix ---"
timeout $TIMEOUT_SEC "$EXE" eval stress.nix +RTS -s -M4G 2>&1 | \
  grep -iE "(allocated|residency|Productivity|Total   time)" || echo "FAILED/OOM"

# --- Test 2: nixpkgs attrNames (the real target) ---
echo ""
echo "--- nixpkgs attrNames ---"
if [ -z "$NIXPKGS" ]; then echo "SKIPPED (set NIXPKGS_PATH)"; else
NIX_PATH="nixpkgs=$NIXPKGS" timeout $TIMEOUT_SEC "$EXE" eval \
  --expr "builtins.length (builtins.attrNames (import <nixpkgs> {}))" \
  +RTS -s -M4G 2>&1 | \
  grep -iE "(allocated|residency|Productivity|Total   time)" || echo "FAILED/OOM"
fi

echo ""
echo "=== done ==="
