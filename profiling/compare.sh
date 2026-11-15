#!/usr/bin/env bash
# profiling/compare.sh - Compare two benchmark runs side by side
#
# Usage: ./profiling/compare.sh <label-before> <label-after>
#
# Reads the summary.txt and heap-top.txt from both runs and shows
# a side-by-side comparison with deltas.

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"

if [[ $# -lt 2 ]]; then
  echo "Usage: $0 <label-before> <label-after>"
  echo ""
  echo "Available runs:"
  ls -1 "$REPO_ROOT/profiling/results/" 2>/dev/null || echo "  (none)"
  exit 1
fi

BEFORE="$REPO_ROOT/profiling/results/$1"
AFTER="$REPO_ROOT/profiling/results/$2"

for dir in "$BEFORE" "$AFTER"; do
  if [[ ! -d "$dir" ]]; then
    echo "ERROR: Run not found: $dir"
    echo ""
    echo "Available runs:"
    ls -1 "$REPO_ROOT/profiling/results/" 2>/dev/null || echo "  (none)"
    exit 1
  fi
done

echo "=== Comparison: $1 -> $2 ==="
echo ""

# --- Metadata ---
echo "--- Metadata ---"
echo "BEFORE: $(grep 'git_commit:' "$BEFORE/meta.txt" 2>/dev/null || echo 'unknown')"
echo "AFTER:  $(grep 'git_commit:' "$AFTER/meta.txt" 2>/dev/null || echo 'unknown')"
echo ""

# --- Summary comparison ---
echo "--- Summary ---"
paste <(cat "$BEFORE/summary.txt") <(cat "$AFTER/summary.txt") | \
  awk -F'\t' '{ printf "%-45s | %s\n", $1, $2 }'
echo ""

# --- Heap profile comparison ---
if [[ -f "$BEFORE/heap-top.txt" && -f "$AFTER/heap-top.txt" ]]; then
  echo "--- Heap Top (MB) ---"
  echo ""
  printf "%-10s %-10s %-8s  %s\n" "BEFORE" "AFTER" "DELTA" "CATEGORY"
  printf "%-10s %-10s %-8s  %s\n" "------" "-----" "-----" "--------"

  # Build associative arrays from both files, then merge and display.
  # parse-hp.sh output format: "%8.1f  %s" - $1 is MB, rest is category.
  # Use field-based parsing to handle category names with parens/spaces.
  awk '
    {
      gsub(/^[ \t]+/, "")
      mb = $1 + 0
      # Category is everything after the first field and its trailing spaces
      cat = $0
      sub(/^[^ \t]+[ \t]+/, "", cat)
    }
    FILENAME == ARGV[1] { before[cat] = mb }
    FILENAME == ARGV[2] { after[cat] = mb }
    END {
      for (k in before) seen[k] = 1
      for (k in after) seen[k] = 1

      for (k in seen) {
        b = before[k] + 0
        a = after[k] + 0
        if (b > 0) {
          delta = ((a - b) / b) * 100
          sign = (delta >= 0) ? "+" : ""
          printf "%8.1f  %8.1f  %s%.0f%%  %s\n", b, a, sign, delta, k | "sort -t\" \" -k2 -rn"
        } else {
          printf "%8.1f  %8.1f  NEW       %s\n", b, a, k | "sort -t\" \" -k2 -rn"
        }
      }
    }
  ' "$BEFORE/heap-top.txt" "$AFTER/heap-top.txt"
fi

echo ""
echo "Full results:"
echo "  Before: $BEFORE/"
echo "  After:  $AFTER/"
