#!/usr/bin/env bash
# profiling/bench.sh — Reproducible benchmark runner for nova-nix memory profiling
#
# Usage:
#   ./profiling/bench.sh <label> [rts-flags...] [--build] [--full]
#
# Examples:
#   ./profiling/bench.sh baseline                        # Just -s stats, -M4G
#   ./profiling/bench.sh iter6-trim -hT                  # Type-based heap profile
#   ./profiling/bench.sh iter6-trim -hc                  # Cost-centre (needs -prof build)
#   ./profiling/bench.sh iter6-trim -hr                  # Retainer (needs -prof build)
#   ./profiling/bench.sh scaling -M6G                    # Custom heap cap
#   ./profiling/bench.sh iter6-trim -hT -M8G             # Combine flags
#   ./profiling/bench.sh iter8 -hT --build               # Auto-build, then -hT
#   ./profiling/bench.sh iter8 --full                    # Full diagnostic suite
#
# Flags:
#   --build   Auto-build before running. Uses standard build for -hT,
#             profiling build (-fprof-late) for -hc/-hr/-hd/-hb/-p.
#   --full    Complete diagnostic suite: -hT (standard build), then
#             -hc -p and -hr (profiling build). Produces report.txt.
#
# Output:
#   profiling/results/<label>/
#     rts-stats.txt    — full +RTS -s output (single-run mode)
#     summary.txt      — parsed key metrics (single-run mode)
#     heap-profile.hp  — .hp file (if heap profiling enabled)
#     heap-top.txt     — top 20 heap categories (if .hp present)
#     hT/              — type breakdown (--full mode)
#     hc/              — cost centre heap + .prof (--full mode)
#     hr/              — retainer profile (--full mode)
#     report.txt       — combined report (--full mode)
#
# The script always adds +RTS -s and the heap cap (-M4G default).

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
NIXPKGS_PATH="C:\\Users\\devon\\nixpkgs-nixos-24.11"
EVAL_EXPR='builtins.length (builtins.attrNames (import <nixpkgs> {}))'
DEFAULT_HEAP_CAP="-M4G"
HEAP_CAP="$DEFAULT_HEAP_CAP"
BUILD_MODE="standard"   # tracks current build: "standard" or "profiling"
LAST_BUILD_MODE=""      # tracks previous build to detect mode switches

# ----------------------------------------------------------------
# Helpers
# ----------------------------------------------------------------

needs_profiling_build() {
  # Returns 0 (true) if any flag requires a profiling build
  for flag in "$@"; do
    case "$flag" in
      -hc|-hr|-hd|-hb|-p) return 0 ;;
    esac
  done
  return 1
}

do_build() {
  local mode="$1"  # "standard" or "profiling"
  BUILD_MODE="$mode"
  echo ""
  echo "=== Building ($mode) ==="
  # cabal doesn't relink the exe when switching between standard/profiling
  # modes, so we must clean the exe build dir before switching.
  if [[ "$mode" != "$LAST_BUILD_MODE" ]]; then
    local exe_build_dir="$REPO_ROOT/dist-newstyle/build/x86_64-windows/ghc-*/nova-nix-*/x/nova-nix"
    rm -rf $exe_build_dir 2>/dev/null || true
  fi
  LAST_BUILD_MODE="$mode"
  if [[ "$mode" == "profiling" ]]; then
    (cd "$REPO_ROOT" && cabal build --enable-profiling --ghc-options="-fprof-late")
  else
    (cd "$REPO_ROOT" && cabal build)
  fi
  echo "Build complete."
}

# Run the nova-nix executable via cabal run, which ensures the correct
# build variant (standard vs profiling) is used.
#   run_exe <args...>
run_exe() {
  if [[ "$BUILD_MODE" == "profiling" ]]; then
    cabal run --enable-profiling nova-nix -- "$@"
  else
    cabal run nova-nix -- "$@"
  fi
}

# Run a single profile pass.
#   run_one_profile <output-dir> <rts-flag...>
# Populates:
#   <output-dir>/rts-stats.txt, summary.txt, heap-profile.hp, heap-top.txt,
#   cost-centre.prof, eval-output.txt, meta.txt
run_one_profile() {
  local out_dir="$1"
  shift
  local rts_extra=("$@")

  mkdir -p "$out_dir"

  # Filter -M flags from extras; use our heap cap
  local rts_filtered=()
  for flag in "${rts_extra[@]+"${rts_extra[@]}"}"; do
    if [[ "$flag" != -M* ]]; then
      rts_filtered+=("$flag")
    fi
  done

  local rts_flags=("-s" "$HEAP_CAP" "${rts_filtered[@]+"${rts_filtered[@]}"}")
  echo "  RTS flags: ${rts_flags[*]}"
  echo "  Build mode: $BUILD_MODE"
  echo "  Output: $out_dir/"

  # Record metadata
  cat > "$out_dir/meta.txt" <<EOF
label: $(basename "$out_dir")
date: $(date -Iseconds)
build_mode: $BUILD_MODE
heap_cap: $HEAP_CAP
rts_flags: ${rts_flags[*]}
expr: $EVAL_EXPR
nixpkgs: $NIXPKGS_PATH
git_commit: $(cd "$REPO_ROOT" && git rev-parse --short HEAD)
git_dirty: $(cd "$REPO_ROOT" && git diff --quiet && echo "no" || echo "yes")
EOF

  # Clean stale output files
  local hp_cwd="$REPO_ROOT/nova-nix.hp"
  local prof_cwd="$REPO_ROOT/nova-nix.prof"
  rm -f "$hp_cwd" "$prof_cwd"

  # Run via cabal run (ensures correct standard/profiling binary)
  cd "$REPO_ROOT"
  set +e
  NIX_PATH="nixpkgs=$NIXPKGS_PATH" \
    run_exe eval --expr "$EVAL_EXPR" \
    +RTS "${rts_flags[@]}" -RTS \
    > "$out_dir/eval-output.txt" \
    2> "$out_dir/rts-stats.txt"
  local exit_code=$?
  set -e

  echo "  Exit code: $exit_code"

  # Move .hp file if generated
  if [[ -f "$hp_cwd" ]]; then
    mv "$hp_cwd" "$out_dir/heap-profile.hp"
    echo "  Heap profile saved."
    "$REPO_ROOT/profiling/parse-hp.sh" "$out_dir/heap-profile.hp" > "$out_dir/heap-top.txt"
    echo "  Heap top 20 parsed."
  fi

  # Move .prof file if generated
  if [[ -f "$prof_cwd" ]]; then
    mv "$prof_cwd" "$out_dir/cost-centre.prof"
    echo "  Cost-centre profile saved."
  fi

  # Parse key metrics from RTS stats
  local stats_file="$out_dir/rts-stats.txt"
  {
    echo "=== Key Metrics ==="
    echo ""

    local alloc resid prod gc
    alloc=$(grep -oP '[\d,]+\s+bytes allocated in the heap' "$stats_file" 2>/dev/null | head -1 || true)
    echo "Allocated: $alloc"

    resid=$(grep -oP '[\d,]+\s+bytes maximum residency' "$stats_file" 2>/dev/null | head -1 || true)
    echo "Max residency: $resid"

    prod=$(grep -oP 'Productivity\s+[\d.]+%' "$stats_file" 2>/dev/null | head -1 || true)
    echo "$prod"

    gc=$(grep -oP 'GC\s+time\s+[\d.]+s' "$stats_file" 2>/dev/null | head -1 || true)
    echo "$gc"

    echo "Exit code: $exit_code"

    if [[ $exit_code -ne 0 ]]; then
      echo ""
      echo "=== Error output (last 5 lines) ==="
      tail -5 "$stats_file"
    fi
  } > "$out_dir/summary.txt"
}

# Parse an RTS stats file for a specific metric, returning a human-readable value.
#   parse_metric <stats-file> <metric>
# Metrics: allocated, residency, productivity, gc_time
parse_metric() {
  local stats_file="$1"
  local metric="$2"
  case "$metric" in
    allocated)
      grep -oP '[\d,]+\s+bytes allocated in the heap' "$stats_file" 2>/dev/null | head -1 || true
      ;;
    residency)
      grep -oP '[\d,]+\s+bytes maximum residency' "$stats_file" 2>/dev/null | head -1 || true
      ;;
    productivity)
      grep -oP 'Productivity\s+[\d.]+%' "$stats_file" 2>/dev/null | head -1 || true
      ;;
    gc_time)
      grep -oP 'GC\s+time\s+[\d.]+s' "$stats_file" 2>/dev/null | head -1 || true
      ;;
  esac
}

# Generate a combined report from --full sub-runs.
#   generate_report <label-dir>
# Expects subdirs: hT/, hc/, hr/
generate_report() {
  local label_dir="$1"
  local report="$label_dir/report.txt"
  local git_commit
  git_commit=$(cd "$REPO_ROOT" && git rev-parse --short HEAD)

  {
    echo "=== nova-nix Profile Report: $(basename "$label_dir") ==="
    echo "Git: $git_commit"
    echo "Date: $(date -Iseconds)"
    echo "Heap cap: $HEAP_CAP"
    echo ""

    # Use hc stats as the primary metrics source (profiling build, most detailed)
    local primary_stats="$label_dir/hc/rts-stats.txt"
    if [[ -f "$primary_stats" ]]; then
      echo "--- Allocation & GC ---"
      local val
      val=$(parse_metric "$primary_stats" allocated)
      echo "Total allocated: $val"
      val=$(parse_metric "$primary_stats" residency)
      echo "Max residency: $val"
      val=$(parse_metric "$primary_stats" productivity)
      echo "$val"
      val=$(parse_metric "$primary_stats" gc_time)
      echo "$val"
      local ec
      ec=$(grep -oP 'Exit code: \d+' "$label_dir/hc/summary.txt" 2>/dev/null | head -1 || echo "Exit code: ?")
      echo "$ec"
      echo ""
    fi

    # Type breakdown from -hT
    if [[ -f "$label_dir/hT/heap-top.txt" ]]; then
      echo "--- Type Breakdown (top 15, from -hT) ---"
      head -15 "$label_dir/hT/heap-top.txt"
      echo ""
    fi

    # Cost centre heap from -hc
    if [[ -f "$label_dir/hc/heap-top.txt" ]]; then
      echo "--- Cost Centre Heap (top 15, from -hc) ---"
      head -15 "$label_dir/hc/heap-top.txt"
      echo ""
    fi

    # Retainer from -hr
    if [[ -f "$label_dir/hr/heap-top.txt" ]]; then
      echo "--- Retainer (top 15, from -hr) ---"
      head -15 "$label_dir/hr/heap-top.txt"
      echo ""
    fi

    # Time/alloc profile from .prof file
    local prof_file="$label_dir/hc/cost-centre.prof"
    if [[ -f "$prof_file" ]]; then
      echo "--- Time/Alloc Profile (top 20, from -p) ---"
      # GHC .prof files have a cost-centre breakdown table.
      # Extract lines that look like cost-centre entries (indented, with %time %alloc).
      # The table starts after a header line containing "COST CENTRE" and a blank line.
      awk '
        /^[ \t]*COST CENTRE/ { header = 1; print; next }
        header == 1 && /^[ \t]*$/ { next }
        header == 1 && /^[-]+$/ { next }
        header == 1 {
          count++
          if (count <= 20) print
        }
      ' "$prof_file"
      echo ""
    fi

  } > "$report"

  echo ""
  echo "=== Combined Report ==="
  cat "$report"
}

# ----------------------------------------------------------------
# Argument parsing
# ----------------------------------------------------------------

if [[ $# -lt 1 ]]; then
  echo "Usage: $0 <label> [rts-flags...] [--build] [--full]"
  echo ""
  echo "  label:      name for this run (e.g. 'baseline', 'iter6-trim')"
  echo "  rts-flags:  additional +RTS flags (e.g. -hT, -hc, -hr, -M8G)"
  echo "  --build:    auto-build before running (standard or profiling as needed)"
  echo "  --full:     complete diagnostic suite (-hT, -hc -p, -hr) with report"
  exit 1
fi

LABEL="$1"
shift

FLAG_BUILD=false
FLAG_FULL=false
RTS_EXTRA=()

for arg in "$@"; do
  case "$arg" in
    --build) FLAG_BUILD=true ;;
    --full)  FLAG_FULL=true ;;
    -M*)     HEAP_CAP="$arg" ;;
    *)       RTS_EXTRA+=("$arg") ;;
  esac
done

# ----------------------------------------------------------------
# Main
# ----------------------------------------------------------------

RESULTS_DIR="$REPO_ROOT/profiling/results/$LABEL"

if [[ "$FLAG_FULL" == true ]]; then
  # --full mode: run 3 passes (hT, hc+p, hr) with auto-build
  echo "=== Full Diagnostic Suite: $LABEL ==="
  echo "Heap cap: $HEAP_CAP"
  mkdir -p "$RESULTS_DIR"

  # Pass 1: -hT (standard build)
  echo ""
  echo "--- Pass 1/3: Type Breakdown (-hT) ---"
  do_build standard
  run_one_profile "$RESULTS_DIR/hT" -hT

  # Pass 2: -hc -p (profiling build — cost centres + time/alloc .prof)
  echo ""
  echo "--- Pass 2/3: Cost Centres + Time Profile (-hc -p) ---"
  do_build profiling
  run_one_profile "$RESULTS_DIR/hc" -hc -p

  # Pass 3: -hr (profiling build — retainers)
  echo ""
  echo "--- Pass 3/3: Retainers (-hr) ---"
  # No rebuild needed — already have profiling build
  run_one_profile "$RESULTS_DIR/hr" -hr

  # Generate combined report
  generate_report "$RESULTS_DIR"

  echo ""
  echo "Full results in: $RESULTS_DIR/"

else
  # Single-run mode (original behavior)

  # --build: auto-build if requested
  if [[ "$FLAG_BUILD" == true ]]; then
    if needs_profiling_build "${RTS_EXTRA[@]+"${RTS_EXTRA[@]}"}"; then
      do_build profiling
    else
      do_build standard
    fi
  fi

  run_one_profile "$RESULTS_DIR" "${RTS_EXTRA[@]+"${RTS_EXTRA[@]}"}"

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
fi
