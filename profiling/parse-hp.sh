#!/usr/bin/env bash
# profiling/parse-hp.sh — Parse .hp file and extract peak values per category
#
# Usage: ./profiling/parse-hp.sh <file.hp> [top-N]
#
# Reads a GHC .hp file, finds the peak byte value for each category
# across all samples, and outputs a sorted table (top N, default 20).
#
# Output format:
#   MB    Category
#   417.2 Env
#   360.1 STACK
#   ...

set -euo pipefail

if [[ $# -lt 1 ]]; then
  echo "Usage: $0 <file.hp> [top-N]"
  exit 1
fi

HP_FILE="$1"
TOP_N="${2:-20}"

if [[ ! -f "$HP_FILE" ]]; then
  echo "ERROR: File not found: $HP_FILE"
  exit 1
fi

# Parse .hp: each sample block has lines like "CategoryName\tBytes"
# We find the peak value for each category across all samples.
awk '
  /^BEGIN_SAMPLE/ { next }
  /^END_SAMPLE/ { next }
  /^JOB / { next }
  /^DATE / { next }
  /^SAMPLE_UNIT / { next }
  /^VALUE_UNIT / { next }
  /^$/ { next }
  {
    # Lines are: CategoryName\tBytes
    cat = $1
    val = $2 + 0
    if (val > peak[cat]) {
      peak[cat] = val
    }
  }
  END {
    for (cat in peak) {
      mb = peak[cat] / (1024 * 1024)
      printf "%8.1f  %s\n", mb, cat
    }
  }
' "$HP_FILE" | sort -rn | head -n "$TOP_N"
