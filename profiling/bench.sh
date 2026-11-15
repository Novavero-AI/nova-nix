#!/usr/bin/env bash
# profiling/bench.sh - Reproducible benchmark runner for nova-nix memory profiling
#
# Usage:
#   ./profiling/bench.sh <label> [rts-flags...] [--build] [--full]
#   ./profiling/bench.sh <label> --scoreboard [-M<cap>] [--build]
#   ./profiling/bench.sh <label> --resources [-M<cap>] [--build]
#
# Requires NIXPKGS_PATH in the environment: the nixpkgs checkout that
# <nixpkgs> in the eval expression resolves to, in the path spelling the
# platform's executable expects.
#
# The scoreboard walks the attributes `nix-env -qa` visits in the package
# set (profiling/scoreboard.nix says which) and counts the attributes whose
# drvPath nova-nix evaluates and, of those, the ones whose drvPath is the
# one upstream Nix computes from the same tree.  The second count needs
# nix-instantiate on PATH, which walks the set as the reference; without it
# that count is not measured.  A failure tryEval cannot catch costs only the
# attribute it occurs in; an evaluation that never ends is not a failure,
# and holds the walk until it is killed.  SCOREBOARD_BUDGET in the
# environment overrides how many attributes one evaluator process visits
# during a walk before the next process resumes it.
#
# --resources then takes a completed walk of the same label and forces
# every attribute that evaluated in one nova-nix process, under +RTS -s,
# for its max residency and GC productivity, with /usr/bin/time for its OS
# peak RSS.  It runs apart from the walk so that a pass that exhausts the
# machine costs only its own figures.  Run inside a cgroup v2 with a
# memory.max, it reports that bound and the processes the kernel's OOM
# killer ended during the pass.
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
#   ./profiling/bench.sh nixpkgs-24.11 --scoreboard      # Scoreboard
#   ./profiling/bench.sh nixpkgs-24.11 --resources       # Its resource pass
#
# Flags:
#   --build       Auto-build before running. Uses standard build for -hT,
#                 profiling build (-fprof-late) for -hc/-hr/-hd/-hb/-p.
#   --full        Complete diagnostic suite: -hT (standard build), then
#                 -hc -p and -hr (profiling build). Produces report.txt.
#   --scoreboard  The scoreboard, from the standard build.
#   --resources   The resource pass over the scoreboard's walk.
#
# Output:
#   profiling/results/<label>/
#     rts-stats.txt    - full +RTS -s output (single-run mode)
#     summary.txt      - parsed key metrics (single-run mode)
#     heap-profile.hp  - .hp file (if heap profiling enabled)
#     heap-top.txt     - top 20 heap categories (if .hp present)
#     hT/              - type breakdown (--full mode)
#     hc/              - cost centre heap + .prof (--full mode)
#     hr/              - retainer profile (--full mode)
#     report.txt       - combined report (--full mode)
#     nova-nix/        - nova-nix's walk (--scoreboard): results.txt, one
#                        attribute per line as "KIND PATH [DRVPATH]";
#                        errors.txt, "PATH EXIT LAST-LINE" for each failure
#                        tryEval cannot catch; retried.txt, "PATH WHY" for
#                        each attribute retried after its process ran out
#                        of memory; runs/, each process's output
#     upstream/        - upstream's walk, the same files (--scoreboard)
#     differences.txt  - each attribute the two walks disagree on
#     scoreboard.md    - the counts, as printed (--scoreboard)
#     resources/       - the one-process pass (--resources): rts-stats.txt,
#                        peak-rss.txt, and resources.md, its figures as
#                        printed
#
# Every nova-nix run gets the heap cap (-M4G default), and every run the
# script measures gets +RTS -s.

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
NIXPKGS_PATH="${NIXPKGS_PATH:?set NIXPKGS_PATH to a nixpkgs checkout}"
EVAL_EXPR='builtins.length (builtins.attrNames (import <nixpkgs> {}))'
DEFAULT_HEAP_CAP="-M4G"
HEAP_CAP="$DEFAULT_HEAP_CAP"
BUILD_MODE="standard"   # tracks current build: "standard" or "profiling"
LAST_BUILD_MODE=""      # tracks previous build to detect mode switches

SCOREBOARD_WALK="$REPO_ROOT/profiling/scoreboard.nix"
# What builtins.trace makes of the walk's reports, in both evaluators.
SCOREBOARD_MARKER="trace: nova-nix-scoreboard "
# Upstream's walk chunk: the attributes nixpkgs' own CI gives one evaluator
# process, to bound its memory (chunkSize in nixpkgs' ci/eval/default.nix).
DEFAULT_UPSTREAM_BUDGET=5000
# nova-nix's walk chunk.  Its C data layer frees nothing until the process
# exits, and the resource pass reached a 12 GiB bound after 5,973
# attributes of nixpkgs 24.11 and 8,867 of 26.05, about 2.1 and 1.4 MiB
# each, so a chunk of 2000 stays near 4 GiB beside upstream's.  It can rise
# to upstream's once the C layer frees what evaluation no longer reaches.
DEFAULT_NOVA_BUDGET=2000
UPSTREAM_BUDGET="${SCOREBOARD_BUDGET:-$DEFAULT_UPSTREAM_BUDGET}"
NOVA_BUDGET="${SCOREBOARD_BUDGET:-$DEFAULT_NOVA_BUDGET}"
# GHC's RTS exits with this status when the heap reaches its -M cap.
EXIT_HEAP_OVERFLOW=251
# The status bash gives a process SIGKILL ended (128 + 9), which is how the
# kernel's OOM killer ends one.
EXIT_KILLED=137
# The uncatchable failures the scoreboard lists, most frequent first.
SCOREBOARD_TOP_FAILURES=10
SCOREBOARD_SCRATCH=""
SCOREBOARD_UPSTREAM_PID=""
SCOREBOARD_NOVA=()

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
# Scoreboard
# ----------------------------------------------------------------

# The walk's reports in an evaluator's stderr, without the trace prefix.
#   scoreboard_reports <stderr-file>
scoreboard_reports() {
  awk -v prefix="$SCOREBOARD_MARKER" \
    'index($0, prefix) == 1 { print substr($0, length(prefix) + 1) }' "$1"
}

# Settled units, "KIND PATH ..." lines from the files named or stdin, as the
# JSON object scoreboard.nix reads.  Neither a path nor a kind holds a
# character JSON would escape.
settled_json() {
  awk 'BEGIN { printf "{" }
    { printf "%s\"%s\":\"%s\"", (NR > 1 ? "," : ""), $2, $1 }
    END { print "}" }' "$@"
}

# The line of an evaluator's stderr that names the failure that ended it:
# the last that opens with "error:" and says more, which in both
# evaluators is the innermost error, ahead of the position and source
# lines upstream prints after it.  A process that died without such a line
# (a crash) gets its last line that is not a walk report.
#   last_error_line <stderr-file>
last_error_line() {
  awk -v prefix="$SCOREBOARD_MARKER" \
    'index($0, prefix) == 1 || !NF { next }
    { last = $0 }
    /^[ \t]*error: *[^ \t]/ { error = $0 }
    END { line = (error != "") ? error : last; sub(/^[ \t]+/, "", line); print line }' "$1"
}

# The cgroup v2 directory this process runs in, from the "0::PATH" line of
# /proc/self/cgroup; nothing where there is none (macOS).
own_cgroup() {
  [[ -r /proc/self/cgroup ]] || return 0
  awk -F: '$1 == "0" && $2 == "" { print "/sys/fs/cgroup" $3 }' /proc/self/cgroup
}

# The processes the kernel's OOM killer has ended in this process's cgroup,
# from the oom_kill line of its memory.events; nothing without one.
cgroup_oom_kills() {
  local dir
  dir=$(own_cgroup)
  [[ -n "$dir" && -r "$dir/memory.events" ]] || return 0
  awk '$1 == "oom_kill" { print $2 }' "$dir/memory.events"
}

# Why a process ran out of memory, or nothing when it did not: its heap
# reached the -M cap, or the kernel's OOM killer ended it, which shows as
# SIGKILL with an oom_kill counted in this cgroup since the process began.
#   memory_exhaustion <status> <oom-kills-before-it-began>
memory_exhaustion() {
  local status="$1" before="$2" after
  if [[ $status -eq $EXIT_HEAP_OVERFLOW ]]; then
    echo "the heap reached its $HEAP_CAP cap"
  elif [[ $status -eq $EXIT_KILLED && -n "$before" ]]; then
    after=$(cgroup_oom_kills)
    if [[ ${after:-0} -gt $before ]]; then
      echo "the kernel's OOM killer ended it"
    fi
  fi
}

# Walk the package set with one evaluator until every unit has settled,
# starting a new process whenever one spends its budget or dies.  A process
# that dies after a unit's begin report settles that unit as an error,
# unless it ran out of memory (memory_exhaustion): a long process can run
# out on a unit that fits in a fresh one, so the unit is retried once in a
# fresh process and listed in retried.txt, and settles as <exhausted-kind>
# only when the fresh process runs out on it too.  A process that runs out
# between units is resumed when it settled any.
#   walk_package_set <name> <out-dir> <budget> <exhausted-kind> <evaluator command...>
# The walk file follows the command's own arguments.  Returns 1 when the
# walk stopped before settling every unit.
walk_package_set() {
  local name="$1" out_dir="$2" budget="$3" exhausted_kind="$4"
  shift 4
  local settled="$out_dir/settled.txt"
  local run=0 started=$SECONDS complete=false retried=""
  local base status before last culprit reply outcome stop kills_before exhausted message
  mkdir -p "$out_dir/runs"
  : > "$settled"
  : > "$out_dir/errors.txt"
  : > "$out_dir/retried.txt"
  : > "$out_dir/results.txt"
  echo 0 > "$out_dir/processes.txt"
  echo 0 > "$out_dir/seconds.txt"
  while [[ "$complete" != true ]]; do
    run=$((run + 1))
    base="$out_dir/runs/$run"
    settled_json "$settled" > "$out_dir/settled.json"
    kills_before=$(cgroup_oom_kills)
    status=0
    SCOREBOARD_SETTLED="$out_dir/settled.json" SCOREBOARD_BUDGET="$budget" \
      "$@" "$SCOREBOARD_WALK" < /dev/null > "$base.out" 2> "$base.err" || status=$?
    exhausted=$(memory_exhaustion "$status" "$kills_before")
    scoreboard_reports "$base.err" > "$base.reports"
    before=$(($(wc -l < "$settled")))
    awk '$1 != "begin"' "$base.reports" >> "$settled"
    last=$(tail -n 1 "$base.reports")
    outcome=""
    stop=""
    if [[ $status -eq 0 ]]; then
      reply=$(cat "$base.out")
      if [[ "$reply" == true ]]; then
        complete=true
      elif [[ "$reply" != false ]]; then
        stop="printed '$reply', which is neither true nor false"
      elif [[ $(($(wc -l < "$settled"))) -eq $before ]]; then
        stop="spent its budget without settling anything"
      fi
    elif [[ "$last" != "begin "* ]]; then
      if [[ -n "$exhausted" && $(($(wc -l < "$settled"))) -gt $before ]]; then
        outcome=", $exhausted between attributes"
      else
        stop="failed outside any attribute: ${exhausted:-$(last_error_line "$base.err")}"
      fi
    else
      culprit="${last#begin }"
      if awk -v path="$culprit" '$2 == path { found = 1 } END { exit !found }' "$settled"; then
        stop="began $culprit, which an earlier process settled"
      elif [[ -n "$exhausted" && "$culprit" != "$retried" ]]; then
        retried="$culprit"
        echo "$culprit $exhausted" >> "$out_dir/retried.txt"
        outcome=", $exhausted at $culprit, which the next process retries"
      elif [[ -n "$exhausted" ]]; then
        echo "$exhausted_kind $culprit" >> "$settled"
        if [[ "$exhausted_kind" == error ]]; then
          echo "$culprit $status $exhausted, in a fresh process too" >> "$out_dir/errors.txt"
        fi
        outcome=", $exhausted at $culprit in a fresh process too, so it settles as $exhausted_kind"
      else
        echo "error $culprit" >> "$settled"
        message=$(last_error_line "$base.err")
        # A process a signal ended (bash's 128 + N) may have said nothing.
        if [[ -z "$message" && $status -gt 128 ]]; then
          message="ended by signal $((status - 128))"
        fi
        echo "$culprit $status $message" >> "$out_dir/errors.txt"
        outcome=", error at $culprit"
      fi
    fi
    echo "$name: process $run exited $status, $(($(wc -l < "$settled") - before)) settled," \
      "$(($(wc -l < "$settled"))) in all, $((SECONDS - started)) s$outcome"
    if [[ -n "$stop" ]]; then
      echo "process $run $stop" > "$out_dir/stopped.txt"
      echo "$name: the walk stopped: process $run $stop" >&2
      break
    fi
  done
  LC_ALL=C sort -k2,2 "$settled" > "$out_dir/results.txt"
  echo "$run" > "$out_dir/processes.txt"
  echo $((SECONDS - started)) > "$out_dir/seconds.txt"
  [[ "$complete" == true ]]
}

# Force every attribute the nova-nix walk evaluated, in one process under
# +RTS -s and the heap cap: the walk again, with every other unit settled.
# Inside a cgroup it also records the cgroup's memory.max and the OOM kills
# during the pass, which it can count only because a kill ends the
# evaluator and leaves this script running (systemd's OOMPolicy=continue).
#   resource_pass <walk-dir> <out-dir> <nova-nix command...>
resource_pass() {
  local walk_dir="$1" out_dir="$2"
  shift 2
  local started=$SECONDS status=0 cgroup kills_before
  local timer=()
  mkdir -p "$out_dir"
  awk '$1 != "drv"' "$walk_dir/results.txt" | settled_json > "$out_dir/settled.json"
  case "$(uname -s)" in
    Linux) timer=(/usr/bin/time -f %M -o "$out_dir/peak-rss.txt") ;;
    Darwin) timer=(/usr/bin/time -l -o "$out_dir/peak-rss.txt") ;;
  esac
  cgroup=$(own_cgroup)
  if [[ -n "$cgroup" && -r "$cgroup/memory.max" ]]; then
    cat "$cgroup/memory.max" > "$out_dir/memory-max.txt"
  fi
  kills_before=$(cgroup_oom_kills)
  SCOREBOARD_SETTLED="$out_dir/settled.json" SCOREBOARD_BUDGET="" \
    ${timer[@]+"${timer[@]}"} "$@" +RTS -s"$out_dir/rts-stats.txt" "$HEAP_CAP" -RTS "$SCOREBOARD_WALK" \
    < /dev/null > "$out_dir/out" 2> "$out_dir/err" || status=$?
  if [[ -n "$kills_before" ]]; then
    echo $(($(cgroup_oom_kills) - kills_before)) > "$out_dir/oom-kills.txt"
  fi
  scoreboard_reports "$out_dir/err" | awk '$1 == "drv"' > "$out_dir/evaluated.txt"
  echo "$status" > "$out_dir/exit.txt"
  echo $((SECONDS - started)) > "$out_dir/seconds.txt"
  echo "resources: one process forced $(($(wc -l < "$out_dir/evaluated.txt"))) attributes, exit $status, $((SECONDS - started)) s"
}

# The OS peak RSS of the resource pass in MiB, from GNU time's %M (KiB) on
# Linux or BSD time's rusage listing (bytes) on macOS; nothing elsewhere.
peak_rss_mib() {
  [[ -s "$1" ]] || return 0
  case "$(uname -s)" in
    Linux) awk 'END { if ($1 ~ /^[0-9]+$/) printf "%.1f\n", $1 / 1024 }' "$1" ;;
    Darwin) awk '/maximum resident set size/ { printf "%.1f\n", $1 / 1048576 }' "$1" ;;
  esac
}

# A figure from a +RTS -s report: residency in MiB, or productivity.
#   rts_figure <stats-file> residency|productivity
rts_figure() {
  [[ -s "$1" ]] || return 0
  case "$2" in
    residency) awk '/bytes maximum residency/ { gsub(",", "", $1); printf "%.1f\n", $1 / 1048576 }' "$1" ;;
    productivity) awk '$1 == "Productivity" { print $2 }' "$1" ;;
  esac
}

# The units of one kind in a walk's results.
#   count_kind <results-file> <kind>
count_kind() {
  awk -v kind="$2" '$1 == kind { n++ } END { print n + 0 }' "$1"
}

# The attributes nova-nix evaluates to the drvPath upstream evaluates them to.
#   count_matches <upstream-results> <nova-nix-results>
count_matches() {
  awk 'NR == FNR { if ($1 == "drv") upstream[$2] = $3; next }
    $1 == "drv" && ($2 in upstream) && upstream[$2] == $3 { n++ }
    END { print n + 0 }' "$1" "$2"
}

# Each attribute the two walks settle differently, as
# "PATH nova-nix=KIND[:DRVPATH] upstream=KIND[:DRVPATH]", with "-" for an
# attribute one walk did not reach.
#   write_differences <upstream-results> <nova-nix-results>
write_differences() {
  awk 'function unit(kind, detail) { return detail == "" ? kind : kind ":" detail }
    NR == FNR { upstream[$2] = unit($1, $3); next }
    { nova[$2] = unit($1, $3) }
    END {
      for (path in nova) {
        theirs = (path in upstream) ? upstream[path] : "-"
        if (nova[path] != theirs) print path, "nova-nix=" nova[path], "upstream=" theirs
      }
      for (path in upstream) if (!(path in nova)) print path, "nova-nix=-", "upstream=" upstream[path]
    }' "$1" "$2" | LC_ALL=C sort
}

# One row of the scoreboard's table: a label and a figure per walk.
#   scoreboard_row <label> <nova-nix> [<upstream>]
scoreboard_row() {
  if [[ $# -eq 3 ]]; then
    printf '| %s | %s | %s |\n' "$1" "$2" "$3"
  else
    printf '| %s | %s |\n' "$1" "$2"
  fi
}

# A figure with its unit, or the words that say why there is none.
#   figure_or <figure> <unit> <absent>
figure_or() {
  if [[ -n "$1" ]]; then
    echo "$1 $2"
  else
    echo "$3"
  fi
}

# The resource pass's figures as Markdown.  A pass the OOM killer ended
# has no +RTS -s report, which the RTS writes only on its way out.
#   write_resources <resources-dir> <attributes-the-walk-evaluated>
write_resources() {
  local resources="$1" evaluated="$2" figure pass_status bound="" kills=0
  [[ -s "$resources/memory-max.txt" ]] && bound=$(cat "$resources/memory-max.txt")
  [[ -s "$resources/oom-kills.txt" ]] && kills=$(cat "$resources/oom-kills.txt")
  echo "## nova-nix resource pass: $LABEL"
  echo
  echo "One nova-nix process forcing the $evaluated attributes that evaluate, under +RTS $HEAP_CAP:"
  echo
  scoreboard_row "Resources" ""
  scoreboard_row ":--" "--:"
  scoreboard_row "attributes forced" "$(($(wc -l < "$resources/evaluated.txt"))) of $evaluated"
  if [[ "$bound" =~ ^[0-9]+$ ]]; then
    scoreboard_row "memory bound" "$(awk -v bytes="$bound" 'BEGIN { printf "%.1f", bytes / 1048576 }') MiB, the memory.max of its cgroup"
  fi
  pass_status=$(cat "$resources/exit.txt")
  if [[ $kills -gt 0 ]]; then
    scoreboard_row "exit status" "$pass_status, ended by the kernel's OOM killer (memory.events counted $kills)"
  elif [[ $pass_status -eq $EXIT_HEAP_OVERFLOW ]]; then
    scoreboard_row "exit status" "$pass_status, the heap reached its cap"
  elif [[ $pass_status -ne 0 ]]; then
    scoreboard_row "exit status" "$pass_status, $(last_error_line "$resources/err" | sed 's/|/\\|/g')"
  else
    scoreboard_row "exit status" "0"
  fi
  figure=$(rts_figure "$resources/rts-stats.txt" residency)
  scoreboard_row "max residency" "$(figure_or "$figure" MiB "not reported")"
  figure=$(rts_figure "$resources/rts-stats.txt" productivity)
  scoreboard_row "GC productivity" "${figure:-not reported}"
  figure=$(peak_rss_mib "$resources/peak-rss.txt")
  scoreboard_row "OS peak RSS" "$(figure_or "$figure" MiB "not measured on this platform")"
  scoreboard_row "wall time" "$(cat "$resources/seconds.txt") s"
}

# The scoreboard as Markdown, which a job summary renders as it stands.
#   write_scoreboard <upstream-version-or-empty>
write_scoreboard() {
  local upstream_version="$1"
  local dir="$RESULTS_DIR" walk
  local nova="$dir/nova-nix/results.txt" upstream="$dir/upstream/results.txt"
  local evaluated
  local both=()
  evaluated=$(count_kind "$nova" drv)
  echo "## nova-nix scoreboard: $LABEL"
  echo
  echo "nixpkgs \`$NIXPKGS_PATH\`: the attributes \`nix-env -qa\` visits in its top-level set for x86_64-linux."
  if [[ -n "$upstream_version" ]]; then
    echo "Reference: $upstream_version, walking the same tree."
    both=("upstream")
  else
    echo "No nix-instantiate on PATH, so nothing is compared with upstream."
  fi
  for walk in nova-nix upstream; do
    if [[ -s "$dir/$walk/stopped.txt" ]]; then
      echo
      echo "**The $walk walk stopped before it settled every attribute, so its counts cover what it settled:" \
        "$(sed 's/|/\\|/g' "$dir/$walk/stopped.txt")**"
    fi
  done
  echo
  scoreboard_row "Attributes" "nova-nix" ${both[@]+"${both[@]}"}
  if [[ -n "$upstream_version" ]]; then
    scoreboard_row ":--" "--:" "--:"
    scoreboard_row "visited" "$(($(wc -l < "$nova")))" "$(($(wc -l < "$upstream")))"
    scoreboard_row "**evaluate** (produce a drvPath)" "**$evaluated**" "$(count_kind "$upstream" drv)"
    scoreboard_row "**match** upstream's drvPath" "**$(count_matches "$upstream" "$nova")**" ""
    scoreboard_row "throw or assert at drvPath" "$(count_kind "$nova" fail)" "$(count_kind "$upstream" fail)"
    scoreboard_row "fail where tryEval cannot catch" "$(count_kind "$nova" error)" "$(count_kind "$upstream" error)"
    scoreboard_row "retried after running out of memory" "$(($(wc -l < "$dir/nova-nix/retried.txt")))" \
      "$(($(wc -l < "$dir/upstream/retried.txt")))"
    if [[ $(count_kind "$upstream" unmeasured) -gt 0 ]]; then
      scoreboard_row "out of memory in a fresh process too, so not measured" "" "$(count_kind "$upstream" unmeasured)"
    fi
    scoreboard_row "evaluator processes" "$(cat "$dir/nova-nix/processes.txt")" "$(cat "$dir/upstream/processes.txt")"
    scoreboard_row "wall time" "$(cat "$dir/nova-nix/seconds.txt") s" "$(cat "$dir/upstream/seconds.txt") s"
  else
    scoreboard_row ":--" "--:"
    scoreboard_row "visited" "$(($(wc -l < "$nova")))"
    scoreboard_row "**evaluate** (produce a drvPath)" "**$evaluated**"
    scoreboard_row "**match** upstream's drvPath" "not measured"
    scoreboard_row "throw or assert at drvPath" "$(count_kind "$nova" fail)"
    scoreboard_row "fail where tryEval cannot catch" "$(count_kind "$nova" error)"
    scoreboard_row "retried after running out of memory" "$(($(wc -l < "$dir/nova-nix/retried.txt")))"
    scoreboard_row "evaluator processes" "$(cat "$dir/nova-nix/processes.txt")"
    scoreboard_row "wall time" "$(cat "$dir/nova-nix/seconds.txt") s"
  fi
  if [[ -s "$dir/nova-nix/errors.txt" ]]; then
    echo
    echo "nova-nix's failures tryEval cannot catch, by the error that names them, most frequent first:"
    echo
    scoreboard_row "Attributes" "Error"
    scoreboard_row "--:" ":--"
    cut -d' ' -f3- "$dir/nova-nix/errors.txt" | LC_ALL=C sort | uniq -c | sort -rn \
      | awk -v top="$SCOREBOARD_TOP_FAILURES" 'NR <= top' \
      | sed -e 's/|/\\|/g' -e 's/^ *\([0-9]*\) \(.*\)$/| \1 | \2 |/'
  fi
}

# End the scoreboard on an interrupt.  The evaluator in the foreground takes
# a Ctrl-C itself and exits normally (nova-nix reports it and exits 1), after
# which bash would start the walk's next process, and the background walk
# ignores SIGINT as every asynchronous command of a script does.  The
# background walk's TERM trap waits for its evaluator, which pkill ends.
interrupt_scoreboard() {
  trap - INT TERM
  if [[ -n "$SCOREBOARD_UPSTREAM_PID" ]]; then
    kill -TERM "$SCOREBOARD_UPSTREAM_PID" 2> /dev/null || true
    pkill -TERM -P "$SCOREBOARD_UPSTREAM_PID" 2> /dev/null || true
  fi
  echo "the scoreboard was interrupted" >&2
  exit 130
}

# Remove the scoreboard's scratch directory.  The stores in it keep each
# path in the read-only modes a store gives every path it adds, as
# upstream's does, so the tree is made writable before it is removed.
remove_scoreboard_scratch() {
  chmod -R u+w "${SCOREBOARD_SCRATCH:?}"
  rm -rf "${SCOREBOARD_SCRATCH:?}"
}

# Check the label and the budget, find the nova-nix build, and set up what
# both scoreboard modes share: a scratch directory removed on exit, the
# interrupt handling, a fresh cache, and SCOREBOARD_NOVA, the nova-nix
# command every evaluation starts from.
scoreboard_prepare() {
  local nova_bin
  if [[ ! "$LABEL" =~ ^[A-Za-z0-9][A-Za-z0-9._-]*$ ]]; then
    echo "a scoreboard label is letters, digits, dots, dashes and underscores" >&2
    return 2
  fi
  if [[ ! "$UPSTREAM_BUDGET" =~ ^[1-9][0-9]*$ || ! "$NOVA_BUDGET" =~ ^[1-9][0-9]*$ ]]; then
    echo "SCOREBOARD_BUDGET must be a positive number of attributes, not '${SCOREBOARD_BUDGET:-}'" >&2
    return 2
  fi
  nova_bin=$(cd "$REPO_ROOT" && cabal list-bin -v0 exe:nova-nix)
  if [[ ! -x "$nova_bin" ]]; then
    echo "no nova-nix build at $nova_bin; build it, or pass --build" >&2
    return 2
  fi
  SCOREBOARD_SCRATCH=$(mktemp -d)
  trap remove_scoreboard_scratch EXIT
  trap interrupt_scoreboard INT TERM
  # A fresh cache for each evaluator, so that a fetch during evaluation is
  # a fetch and never a hit an earlier run left.
  export XDG_CACHE_HOME="$SCOREBOARD_SCRATCH/cache"
  SCOREBOARD_NOVA=("$nova_bin" eval --nix-path "nixpkgs=$NIXPKGS_PATH" --nix-path "nix=$REPO_ROOT/data/nix")
}

# The scoreboard: nova-nix's walk and, with nix-instantiate on PATH,
# upstream's beside it.  Returns 1 when either walk stopped early.
run_scoreboard() {
  local upstream_version="" walks_complete=true
  local upstream=()
  scoreboard_prepare || return
  rm -rf "${RESULTS_DIR:?}/nova-nix" "${RESULTS_DIR:?}/upstream" "${RESULTS_DIR:?}/resources"
  mkdir -p "$RESULTS_DIR"

  # Upstream walks beside nova-nix: the two are separate processes reading
  # the same tree.  --read-write-mode because --eval alone implies read-only
  # mode, in which a drvPath with deep context cannot be computed (the
  # parity job's drvpath-cases.sh says why); import from derivation is off,
  # as nixpkgs' own CI evaluates the set, so that neither evaluator builds
  # during the walk.  An attribute on which a fresh upstream process runs
  # out of memory too is unmeasured, not failed: the reference running out
  # says nothing about the attribute.
  if command -v nix-instantiate > /dev/null 2>&1; then
    upstream_version=$(nix-instantiate --version)
    upstream=(nix-instantiate --eval --read-write-mode
      --option allow-import-from-derivation false -I "nixpkgs=$NIXPKGS_PATH")
    {
      trap 'exit 143' TERM
      walk_package_set upstream "$RESULTS_DIR/upstream" "$UPSTREAM_BUDGET" unmeasured "${upstream[@]}"
    } &
    SCOREBOARD_UPSTREAM_PID=$!
  fi
  walk_package_set nova-nix "$RESULTS_DIR/nova-nix" "$NOVA_BUDGET" error "${SCOREBOARD_NOVA[@]}" \
    --store "$SCOREBOARD_SCRATCH/walk-store" +RTS "$HEAP_CAP" -RTS || walks_complete=false
  if [[ -n "$SCOREBOARD_UPSTREAM_PID" ]]; then
    wait "$SCOREBOARD_UPSTREAM_PID" || walks_complete=false
    SCOREBOARD_UPSTREAM_PID=""
    write_differences "$RESULTS_DIR/upstream/results.txt" "$RESULTS_DIR/nova-nix/results.txt" \
      > "$RESULTS_DIR/differences.txt"
  fi
  write_scoreboard "$upstream_version" > "$RESULTS_DIR/scoreboard.md"
  echo
  cat "$RESULTS_DIR/scoreboard.md"
  [[ "$walks_complete" == true ]]
}

# The resource pass over the label's nova-nix walk, its figures written to
# resources/resources.md.  The pass settles every unit the walk did not
# evaluate, so it needs a walk that settled them all: one that stopped
# would leave the units it never reached for the pass to begin.
run_resources() {
  local walk="$RESULTS_DIR/nova-nix" resources="$RESULTS_DIR/resources"
  scoreboard_prepare || return
  if [[ ! -s "$walk/processes.txt" ]]; then
    echo "no nova-nix walk under $RESULTS_DIR; run --scoreboard with this label first" >&2
    return 2
  fi
  if [[ -e "$walk/stopped.txt" ]]; then
    echo "the nova-nix walk under $RESULTS_DIR stopped, so the set that evaluates is not known" >&2
    return 1
  fi
  rm -rf "${resources:?}"
  # A store of its own, so that the pass writes what a fresh evaluation
  # writes (builtins.toFile's files, say) rather than finding the walk's.
  resource_pass "$walk" "$resources" "${SCOREBOARD_NOVA[@]}" --store "$SCOREBOARD_SCRATCH/pass-store"
  write_resources "$resources" "$(count_kind "$walk/results.txt" drv)" > "$resources/resources.md"
  echo
  cat "$resources/resources.md"
}

# ----------------------------------------------------------------
# Argument parsing
# ----------------------------------------------------------------

if [[ $# -lt 1 ]]; then
  echo "Usage: $0 <label> [rts-flags...] [--build] [--full]"
  echo "       $0 <label> --scoreboard [-M<cap>] [--build]"
  echo "       $0 <label> --resources [-M<cap>] [--build]"
  echo ""
  echo "  label:         name for this run (e.g. 'baseline', 'iter6-trim')"
  echo "  rts-flags:     additional +RTS flags (e.g. -hT, -hc, -hr, -M8G)"
  echo "  --build:       auto-build before running (standard or profiling as needed)"
  echo "  --full:        complete diagnostic suite (-hT, -hc -p, -hr) with report"
  echo "  --scoreboard:  walk the package set and count what evaluates and matches upstream"
  echo "  --resources:   force what the label's walk evaluated in one process, for its figures"
  exit 1
fi

LABEL="$1"
shift

FLAG_BUILD=false
FLAG_FULL=false
FLAG_SCOREBOARD=false
FLAG_RESOURCES=false
RTS_EXTRA=()

for arg in "$@"; do
  case "$arg" in
    --build)      FLAG_BUILD=true ;;
    --full)       FLAG_FULL=true ;;
    --scoreboard) FLAG_SCOREBOARD=true ;;
    --resources)  FLAG_RESOURCES=true ;;
    -M*)          HEAP_CAP="$arg" ;;
    *)            RTS_EXTRA+=("$arg") ;;
  esac
done

# ----------------------------------------------------------------
# Main
# ----------------------------------------------------------------

RESULTS_DIR="$REPO_ROOT/profiling/results/$LABEL"

if [[ "$FLAG_SCOREBOARD" == true || "$FLAG_RESOURCES" == true ]]; then
  if [[ "$FLAG_SCOREBOARD" == "$FLAG_RESOURCES" || "$FLAG_FULL" == true || ${#RTS_EXTRA[@]} -gt 0 ]]; then
    echo "--scoreboard and --resources each take a heap cap and --build, and no other flag" >&2
    exit 2
  fi
  [[ "$FLAG_BUILD" == true ]] && do_build standard
  if [[ "$FLAG_SCOREBOARD" == true ]]; then
    run_scoreboard
  else
    run_resources
  fi

elif [[ "$FLAG_FULL" == true ]]; then
  # --full mode: run 3 passes (hT, hc+p, hr) with auto-build
  echo "=== Full Diagnostic Suite: $LABEL ==="
  echo "Heap cap: $HEAP_CAP"
  mkdir -p "$RESULTS_DIR"

  # Pass 1: -hT (standard build)
  echo ""
  echo "--- Pass 1/3: Type Breakdown (-hT) ---"
  do_build standard
  run_one_profile "$RESULTS_DIR/hT" -hT

  # Pass 2: -hc -p (profiling build - cost centres + time/alloc .prof)
  echo ""
  echo "--- Pass 2/3: Cost Centres + Time Profile (-hc -p) ---"
  do_build profiling
  run_one_profile "$RESULTS_DIR/hc" -hc -p

  # Pass 3: -hr (profiling build - retainers)
  echo ""
  echo "--- Pass 3/3: Retainers (-hr) ---"
  # No rebuild needed - already have profiling build
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
