#!/usr/bin/env bash
# profiling/bench.sh - nova-nix's measurements over a nixpkgs tree
#
# Usage:
#   ./profiling/bench.sh <label> --scoreboard [-M<cap>] [--build]
#   ./profiling/bench.sh <label> --resources [-M<cap>] [--build]
#   ./profiling/bench.sh <label> --memory [-M<cap>] [--build]
#
# Requires NIXPKGS_PATH in the environment: the nixpkgs checkout that
# <nixpkgs> in the eval expression resolves to, in the path spelling the
# platform's executable expects.
#
# The scoreboard walks the attributes `nix-env -qa` visits in the package
# set (profiling/walk.nix says which) and counts the attributes whose
# drvPath nova-nix evaluates and, of those, the ones whose drvPath is the
# one upstream Nix computes from the same tree.  The second count needs
# nix-instantiate on PATH, which walks the set as the reference; without it
# that count is not measured.  A failure tryEval cannot catch costs only the
# attribute it occurs in; an evaluation that never ends is not a failure,
# and holds the walk until it is killed.  SCOREBOARD_BUDGET in the
# environment overrides how many attributes one evaluator process visits
# during a walk before the next process resumes it.
#
# SCOREBOARD_MEMORY_MAX in the environment bounds the memory of what the
# script measures, the evaluators and the memory suite's benchmark, as a
# cgroup's memory.max takes it (12G, say), in every mode.  Run in a cgroup
# v2 that systemd delegated to it (Delegate=yes), the script keeps itself
# in a leaf of its own and runs each of them in a sibling whose
# memory.max is the bound, so that the kernel's OOM killer, once they
# reach it, ends one of them and never the script measuring them.
# Without it, a pass run inside a cgroup v2 reports that cgroup's.
#
# --resources then takes a completed walk of the same label and forces
# every attribute that evaluated in one nova-nix process, under +RTS -s,
# for its max residency and GC productivity, with /usr/bin/time for its OS
# peak RSS.  It runs apart from the walk so that a pass that exhausts the
# machine costs only its own figures.  Under a memory bound it reports that
# bound and the processes the kernel's OOM killer ended during the pass.
#
# --memory measures where one evaluation's memory goes, over a slice of the
# walk: its first MEMORY_SLICE attributes (2000 unless the environment says
# otherwise).  Both evaluators walk the slice first, as the scoreboard does,
# to find the attributes that end a process or run out of memory in one;
# then one process of each visits the slice with those passed over,
# through profiling/memory.nix,
# which keeps the package set held when evaluation returns.  nova-nix runs
# twice: once measured (+RTS -s, /usr/bin/time), and once under a heap
# profile by closure type (+RTS -hT) with NOVA_NIX_C_STATS set, for a
# census of GHC's heap and of the C data layer when the slice ends: what
# the C side holds by kind, what the result, the root environment, the
# import cache and the search path reach, and what the mark cost, and the
# environments and attribute sets each construct or builtin allocated.
# A third nova-nix run takes a census of regions under
# NOVA_NIX_REGION_STATS: at the end of each force, what the force allocated
# and what still reaches it, and what collectors freeing there would hold
# at each threshold of bytes and each block size and line length
# (cbits/nn_region.h).
# Upstream's pass runs under NIX_SHOW_STATS.  The attrset-memory benchmark
# then builds the attribute sets the census reached, as C sets and as sets
# held in GHC's heap, under the copying and the non-moving collector.
# Upstream's figures need nix-instantiate and jq on PATH.
#
# Examples:
#   ./profiling/bench.sh nixpkgs-24.11 --scoreboard      # Scoreboard
#   ./profiling/bench.sh nixpkgs-24.11 --resources       # Its resource pass
#   ./profiling/bench.sh nixpkgs-24.11 --memory          # The memory suite
#
# Flags:
#   --build       Build what the mode runs first.
#   --scoreboard  The scoreboard.
#   --resources   The resource pass over the scoreboard's walk.
#   --memory      The memory suite over a slice of the walk.
#
# Output:
#   profiling/results/<label>/
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
#     memory/          - the memory suite (--memory): slice/, both walks of
#                        the slice; settled.txt, the attributes every pass
#                        passes over; nova-nix/, census/, regions/ and
#                        upstream/, the four passes, each with its output,
#                        peak-rss.txt and visited.txt, and
#                        census/census.txt, the C census, census/heap.hp,
#                        the heap profile, regions/regions.txt, the census
#                        of regions, and upstream/nix-stats.json; attrset/,
#                        the benchmark's runs; memory.md, every figure as
#                        printed
#
# Every nova-nix run gets the heap cap (-M4G default).

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
NIXPKGS_PATH="${NIXPKGS_PATH:?set NIXPKGS_PATH to a nixpkgs checkout}"
DEFAULT_HEAP_CAP="-M4G"
HEAP_CAP="$DEFAULT_HEAP_CAP"

SCOREBOARD_WALK="$REPO_ROOT/profiling/scoreboard.nix"
MEMORY_WALK="$REPO_ROOT/profiling/memory.nix"
# What builtins.trace makes of the walk's reports, in both evaluators.
SCOREBOARD_MARKER="trace: nova-nix-scoreboard "
# Upstream's walk chunk: the attributes nixpkgs' own CI gives one evaluator
# process, to bound its memory (chunkSize in nixpkgs' ci/eval/default.nix).
DEFAULT_UPSTREAM_BUDGET=5000
# nova-nix's walk chunk.  With the region collector off, so that the C
# data layer frees nothing until the process exits, the resource pass
# reached a 12 GiB bound after 5,973 attributes of nixpkgs 24.11 and 8,867
# of 26.05, about 2.1 and 1.4 MiB each, so a chunk of 2000 stays near
# 4 GiB beside upstream's whether the collector runs or not.
DEFAULT_NOVA_BUDGET=2000
UPSTREAM_BUDGET="${SCOREBOARD_BUDGET:-$DEFAULT_UPSTREAM_BUDGET}"
NOVA_BUDGET="${SCOREBOARD_BUDGET:-$DEFAULT_NOVA_BUDGET}"
# The memory suite's slice: the attributes one process visits, the number
# a nova-nix walk chunk holds.
DEFAULT_MEMORY_SLICE=2000
MEMORY_SLICE="${MEMORY_SLICE:-$DEFAULT_MEMORY_SLICE}"
# Seconds between the heap profile's own samples.  Each census traverses
# the whole heap, gigabytes over a slice of nixpkgs, so a short interval
# would make the profiled pass mostly censuses.  The census the slice's end
# requests comes in addition.
HEAP_SAMPLE_SECONDS=10
# The closure types the memory report lists, largest first.
MEMORY_TOP_CLOSURE_TYPES=10
# GHC's RTS exits with this status when the heap reaches its -M cap.
EXIT_HEAP_OVERFLOW=251
# The status bash gives a process SIGKILL ended (128 + 9), which is how the
# kernel's OOM killer ends one.
EXIT_KILLED=137
# The uncatchable failures the scoreboard lists, most frequent first.
SCOREBOARD_TOP_FAILURES=10
SCOREBOARD_SCRATCH=""
SCOREBOARD_UPSTREAM_PID=""
SCOREBOARD_UPSTREAM_VERSION=""
SCOREBOARD_NOVA=()
SCOREBOARD_UPSTREAM=()
# The cgroup bound_evaluators bounds the evaluators in, and the command
# words that start an evaluator there.
SCOREBOARD_BOUND_CGROUP=""
SCOREBOARD_BOUNDED=()

# ----------------------------------------------------------------
# Build
# ----------------------------------------------------------------

# Build the executable and, for the memory suite, the attrset benchmark.
#   do_build <cabal target...>
do_build() {
  echo "=== Building $* ==="
  (cd "$REPO_ROOT" && cabal build "$@")
}

# ----------------------------------------------------------------
# Walks
# ----------------------------------------------------------------

# The walk's reports in an evaluator's stderr, without the trace prefix.
#   scoreboard_reports <stderr-file>
scoreboard_reports() {
  awk -v prefix="$SCOREBOARD_MARKER" \
    'index($0, prefix) == 1 { print substr($0, length(prefix) + 1) }' "$1"
}

# Settled units, "KIND PATH ..." lines from the files named or stdin, as the
# JSON object walk.nix reads.  Neither a path nor a kind holds a character
# JSON would escape.
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

# The cgroup whose memory.max bounds the evaluators and whose memory.events
# counts their OOM kills: the one bound_evaluators made, or else this
# process's own.
bound_cgroup() {
  if [[ -n "$SCOREBOARD_BOUND_CGROUP" ]]; then
    echo "$SCOREBOARD_BOUND_CGROUP"
  else
    own_cgroup
  fi
}

# With SCOREBOARD_MEMORY_MAX set, bound the evaluators alone: move this
# process into a leaf, harness, of the cgroup systemd delegated to it, and
# give a sibling, bounded, that memory.max and no swap, which each
# evaluator moves itself into as it starts (SCOREBOARD_BOUNDED).  A cgroup
# v2 that enables a controller for its children holds no process of its
# own, so this process moves first, while it has no child.  Writing 0 to
# cgroup.procs moves the process that writes it.  Returns 2 when the bound
# cannot be set.
bound_evaluators() {
  local cg
  [[ -n "${SCOREBOARD_MEMORY_MAX:-}" ]] || return 0
  cg=$(own_cgroup)
  if [[ -z "$cg" ]] || ! mkdir -p "$cg/harness" "$cg/bounded" \
    || ! echo 0 > "$cg/harness/cgroup.procs" \
    || ! echo +memory > "$cg/cgroup.subtree_control" \
    || ! echo "$SCOREBOARD_MEMORY_MAX" > "$cg/bounded/memory.max"; then
    echo "SCOREBOARD_MEMORY_MAX needs a cgroup v2 delegated to this process (systemd's Delegate=yes)" >&2
    return 2
  fi
  if [[ -e "$cg/bounded/memory.swap.max" ]] && ! echo 0 > "$cg/bounded/memory.swap.max"; then
    echo "SCOREBOARD_MEMORY_MAX cannot keep the evaluators out of swap" >&2
    return 2
  fi
  SCOREBOARD_BOUND_CGROUP="$cg/bounded"
  # shellcheck disable=SC2016 # $0 and $@ are the inner shell's.
  SCOREBOARD_BOUNDED=(sh -c 'echo 0 > "$0/cgroup.procs" && exec "$@"' "$SCOREBOARD_BOUND_CGROUP")
}

# The processes the kernel's OOM killer has ended under the bound, from
# the oom_kill line of its cgroup's memory.events; nothing without one.
cgroup_oom_kills() {
  local dir
  dir=$(bound_cgroup)
  [[ -n "$dir" && -r "$dir/memory.events" ]] || return 0
  awk '$1 == "oom_kill" { print $2 }' "$dir/memory.events"
}

# Why a process ran out of memory, or nothing when it did not: its heap
# reached the -M cap, or the kernel's OOM killer ended it, which shows as
# SIGKILL with an oom_kill counted under the bound since the process began.
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

# Walk the package set with one evaluator until every unit has settled, or
# with a limit until that many have, starting a new process whenever one
# spends its budget or dies.  A process that dies after a unit's begin
# report settles that unit as an error, unless it ran out of memory
# (memory_exhaustion): a long process can run out on a unit that fits in a
# fresh one, so the unit is retried once in a fresh process and listed in
# retried.txt, and settles as <exhausted-kind> only when the fresh process
# runs out on it too.  A process that runs out between units is resumed
# when it settled any.  Each process resumes where the last stopped, so the
# units settled are always the first ones of the walk, and a process under
# a limit gets no more budget than the limit leaves.
#   walk_package_set <name> <out-dir> <budget> <limit-or-empty> <exhausted-kind> <evaluator command...>
# The walk file follows the command's own arguments.  Returns 1 when the
# walk stopped before settling every unit, or the limit's worth.
walk_package_set() {
  local name="$1" out_dir="$2" budget="$3" limit="$4" exhausted_kind="$5"
  shift 5
  local settled="$out_dir/settled.txt"
  local run=0 started=$SECONDS complete=false retried=""
  local base status before last culprit reply outcome stop kills_before exhausted message process_budget
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
    process_budget=$budget
    if [[ -n "$limit" && $((limit - $(wc -l < "$settled"))) -lt $process_budget ]]; then
      process_budget=$((limit - $(wc -l < "$settled")))
    fi
    kills_before=$(cgroup_oom_kills)
    status=0
    SCOREBOARD_SETTLED="$out_dir/settled.json" SCOREBOARD_BUDGET="$process_budget" \
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
    if [[ -z "$stop" && -n "$limit" && $(($(wc -l < "$settled"))) -ge $limit ]]; then
      complete=true
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

# Walk with nova-nix and, with nix-instantiate on PATH, with upstream in
# the background beside it, into <out-dir>/nova-nix and <out-dir>/upstream:
# the two are separate processes reading the same tree.
#   walk_both <out-dir> <limit-or-empty>
# Returns 1 when either walk stopped early.
walk_both() {
  local out="$1" limit="$2" complete=true
  if [[ ${#SCOREBOARD_UPSTREAM[@]} -gt 0 ]]; then
    {
      trap 'exit 143' TERM
      walk_package_set upstream "$out/upstream" "$UPSTREAM_BUDGET" "$limit" unmeasured "${SCOREBOARD_UPSTREAM[@]}"
    } &
    SCOREBOARD_UPSTREAM_PID=$!
  fi
  walk_package_set nova-nix "$out/nova-nix" "$NOVA_BUDGET" "$limit" error "${SCOREBOARD_NOVA[@]}" \
    --store "$SCOREBOARD_SCRATCH/walk-store" +RTS "$HEAP_CAP" -RTS || complete=false
  if [[ -n "$SCOREBOARD_UPSTREAM_PID" ]]; then
    wait "$SCOREBOARD_UPSTREAM_PID" || complete=false
    SCOREBOARD_UPSTREAM_PID=""
  fi
  [[ "$complete" == true ]]
}

# One evaluator process over a walk, measured: the units it visited and
# the drvPaths it evaluated, its exit status, wall time and OS peak RSS,
# and under a bound (bound_cgroup) its memory.max and the OOM kills during
# the pass.
#   measured_pass <out-dir> <settled-json> <budget-or-empty> <walk-file> <evaluator command...>
measured_pass() {
  local out_dir="$1" settled="$2" budget="$3" walk="$4"
  shift 4
  local started=$SECONDS status=0 cgroup kills_before
  local timer=()
  mkdir -p "$out_dir"
  case "$(uname -s)" in
    Linux) timer=(/usr/bin/time -f '%M %e' -o "$out_dir/peak-rss.txt") ;;
    Darwin) timer=(/usr/bin/time -l -o "$out_dir/peak-rss.txt") ;;
  esac
  cgroup=$(bound_cgroup)
  if [[ -n "$cgroup" && -r "$cgroup/memory.max" ]]; then
    cat "$cgroup/memory.max" > "$out_dir/memory-max.txt"
  fi
  kills_before=$(cgroup_oom_kills)
  SCOREBOARD_SETTLED="$settled" SCOREBOARD_BUDGET="$budget" \
    ${timer[@]+"${timer[@]}"} "$@" "$walk" < /dev/null > "$out_dir/out" 2> "$out_dir/err" || status=$?
  if [[ -n "$kills_before" ]]; then
    echo $(($(cgroup_oom_kills) - kills_before)) > "$out_dir/oom-kills.txt"
  fi
  scoreboard_reports "$out_dir/err" > "$out_dir/reports.txt"
  awk '$1 == "begin" { print $2 }' "$out_dir/reports.txt" > "$out_dir/visited.txt"
  awk '$1 == "drv"' "$out_dir/reports.txt" > "$out_dir/evaluated.txt"
  echo "$status" > "$out_dir/exit.txt"
  echo $((SECONDS - started)) > "$out_dir/seconds.txt"
}

# Force every attribute the nova-nix walk evaluated, in one process under
# +RTS -s and the heap cap: the walk again, with every other unit settled.
#   resource_pass <walk-dir> <out-dir> <nova-nix command...>
resource_pass() {
  local walk_dir="$1" out_dir="$2"
  shift 2
  mkdir -p "$out_dir"
  awk '$1 != "drv"' "$walk_dir/results.txt" | settled_json > "$out_dir/settled.json"
  measured_pass "$out_dir" "$out_dir/settled.json" "" "$SCOREBOARD_WALK" \
    "$@" +RTS -s"$out_dir/rts-stats.txt" "$HEAP_CAP" -RTS
  echo "resources: one process forced $(($(wc -l < "$out_dir/evaluated.txt"))) attributes," \
    "exit $(cat "$out_dir/exit.txt"), $(cat "$out_dir/seconds.txt") s"
}

# ----------------------------------------------------------------
# Figures
# ----------------------------------------------------------------

# The OS peak RSS of a measured pass in MiB, from GNU time's %M (KiB) on
# Linux or BSD time's rusage listing (bytes) on macOS; nothing elsewhere.
peak_rss_mib() {
  [[ -s "$1" ]] || return 0
  case "$(uname -s)" in
    Linux) awk 'END { if ($1 ~ /^[0-9]+$/) printf "%.1f\n", $1 / 1024 }' "$1" ;;
    Darwin) awk '/maximum resident set size/ { printf "%.1f\n", $1 / 1048576 }' "$1" ;;
  esac
}

# The wall time of a measured pass in seconds, from GNU time's %e or BSD
# time's "real"; nothing elsewhere.
pass_seconds() {
  [[ -s "$1" ]] || return 0
  case "$(uname -s)" in
    Linux) awk 'END { if ($2 ~ /^[0-9.]+$/) print $2 }' "$1" ;;
    Darwin) awk '$2 == "real" { print $1 }' "$1" ;;
  esac
}

# A figure from a +RTS -s report: residency in MiB, productivity, or the
# GC's or the whole run's CPU and elapsed seconds ("CPU ELAPSED").
#   rts_figure <stats-file> residency|productivity|gc|total
rts_figure() {
  [[ -s "$1" ]] || return 0
  case "$2" in
    residency) awk '/bytes maximum residency/ { gsub(",", "", $1); printf "%.1f\n", $1 / 1048576 }' "$1" ;;
    productivity) awk '$1 == "Productivity" { print $2 }' "$1" ;;
    gc | total)
      awk -v phase="$([[ "$2" == gc ]] && echo GC || echo Total)" \
        '$1 == phase && $2 == "time" {
          cpu = $3; elapsed = ($4 == "(") ? $5 : $4
          gsub(/[(s]/, "", cpu); gsub(/[(s]/, "", elapsed); print cpu, elapsed }' "$1"
      ;;
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

# One row of a table: a label and a figure per column.
#   scoreboard_row <label> <figure...>
scoreboard_row() {
  local row="| $1 |"
  shift
  local figure
  for figure in "$@"; do
    row+=" $figure |"
  done
  echo "$row"
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

# A process's exit status as a row's figure: the status and why it ended
# when it did not end well.
#   exit_figure <pass-dir> <stderr-file>
exit_figure() {
  local pass="$1" status kills=0
  [[ -s "$pass/exit.txt" ]] || { echo "did not run"; return 0; }
  status=$(cat "$pass/exit.txt")
  [[ -s "$pass/oom-kills.txt" ]] && kills=$(cat "$pass/oom-kills.txt")
  if [[ $kills -gt 0 ]]; then
    echo "$status, ended by the kernel's OOM killer (memory.events counted $kills)"
  elif [[ $status -eq $EXIT_HEAP_OVERFLOW ]]; then
    echo "$status, the heap reached its cap"
  elif [[ $status -ne 0 ]]; then
    echo "$status, $(last_error_line "$2" | sed 's/|/\\|/g')"
  else
    echo "0"
  fi
}

# The resource pass's figures as Markdown.  A pass the OOM killer ended
# has no +RTS -s report, which the RTS writes only on its way out.
#   write_resources <resources-dir> <attributes-the-walk-evaluated>
write_resources() {
  local resources="$1" evaluated="$2" figure bound=""
  [[ -s "$resources/memory-max.txt" ]] && bound=$(cat "$resources/memory-max.txt")
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
  scoreboard_row "exit status" "$(exit_figure "$resources" "$resources/err")"
  figure=$(rts_figure "$resources/rts-stats.txt" residency)
  scoreboard_row "max residency" "$(figure_or "$figure" MiB "not reported")"
  figure=$(rts_figure "$resources/rts-stats.txt" productivity)
  scoreboard_row "GC productivity" "${figure:-not reported}"
  figure=$(peak_rss_mib "$resources/peak-rss.txt")
  scoreboard_row "OS peak RSS" "$(figure_or "$figure" MiB "not measured on this platform")"
  scoreboard_row "wall time" "$(cat "$resources/seconds.txt") s"
}

# The scoreboard as Markdown, which a job summary renders as it stands.
write_scoreboard() {
  local dir="$RESULTS_DIR" walk
  local nova="$dir/nova-nix/results.txt" upstream="$dir/upstream/results.txt"
  local evaluated
  local both=()
  evaluated=$(count_kind "$nova" drv)
  echo "## nova-nix scoreboard: $LABEL"
  echo
  echo "nixpkgs \`$NIXPKGS_PATH\`: the attributes \`nix-env -qa\` visits in its top-level set for x86_64-linux."
  if [[ -n "$SCOREBOARD_UPSTREAM_VERSION" ]]; then
    echo "Reference: $SCOREBOARD_UPSTREAM_VERSION, walking the same tree."
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
  if [[ -n "$SCOREBOARD_UPSTREAM_VERSION" ]]; then
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

# ----------------------------------------------------------------
# Memory figures
# ----------------------------------------------------------------

# The units of a slice a pass over it begins: every unit the slice's walk
# settled, but for each unit the pass passes over (walk.nix's settled
# input), that unit and every unit under it.  The first file is told from
# the second by name, which holds when it is empty too.
#   pass_budget <passed-over-units> <slice-units>
pass_budget() {
  awk 'FILENAME == ARGV[1] { over[$2] = 1; next }
    {
      parts = split($2, part, ".")
      path = ""
      begun = 1
      for (i = 1; i <= parts; i++) {
        path = (i == 1) ? part[1] : path "." part[i]
        if (path in over) { begun = 0; break }
      }
      n += begun
    }
    END { print n + 0 }' "$1" "$2"
}

# A value of a census or a benchmark run: the rest of its "NAME VALUE"
# line, or nothing.
#   named_value <file> <name>
named_value() {
  [[ -s "$1" ]] || return 0
  awk -v name="$2" '$1 == name { $1 = ""; sub(/^ /, ""); print; exit }' "$1"
}

# A field of upstream's NIX_SHOW_STATS report, or nothing.
#   upstream_stat <nix-stats-json> <jq-path>
upstream_stat() {
  [[ -s "$1" ]] && command -v jq > /dev/null 2>&1 || return 0
  jq -r "($2) // empty" "$1"
}

# Bytes as MiB, to one decimal, or nothing for no bytes.
mib() {
  [[ -n "$1" ]] || return 0
  awk -v bytes="$1" 'BEGIN { printf "%.1f MiB", bytes / 1048576 }'
}

# Seconds as milliseconds, to one decimal, or nothing.
ms() {
  [[ -n "$1" ]] || return 0
  awk -v s="$1" 'BEGIN { printf "%.1f ms", s * 1000 }'
}

# "N objects, X MiB", or "X MiB" without a count; nothing without bytes.
#   objects_figure <count-or-empty> <bytes-or-empty> <noun>
objects_figure() {
  [[ -n "$2" ]] || return 0
  if [[ -n "$1" ]]; then
    echo "$1 $3, $(mib "$2")"
  else
    mib "$2"
  fi
}

# How much of an allocated figure is not reached, in percent of the bytes.
#   unreached_figure <allocated-bytes> <reached-bytes> <reached-count> <noun>
unreached_figure() {
  [[ -n "$1" && -n "$2" ]] || return 0
  awk -v allocated="$1" -v reached="$2" -v count="$3" -v noun="$4" 'BEGIN {
    share = allocated > 0 ? 100 * (allocated - reached) / allocated : 0
    printf "%.1f%% of the bytes; reached: %s %s, %.1f MiB", share, count, noun, reached / 1048576 }'
}

# A heap profile's last sample, which the census at the slice's end
# requested, beside the sample with the most bytes: their totals as
# "END<TAB>PEAK", or each closure type of the last sample as
# "TYPE<TAB>END<TAB>PEAK", largest at the end first.
#   heap_figures <hp-file> totals|types
heap_figures() {
  [[ -s "$1" ]] || return 0
  awk -F '\t' -v what="$2" '
    /^BEGIN_SAMPLE/ { split("", current); total = 0; next }
    /^END_SAMPLE/ {
      if (total > 0) { split("", last); for (t in current) last[t] = current[t]; last_total = total }
      if (total > peak_total) { split("", peak); for (t in current) peak[t] = current[t]; peak_total = total }
      next
    }
    NF == 2 { current[$1] = $2; total += $2 }
    END {
      if (what == "totals") printf "%d\t%d\n", last_total, peak_total
      else for (t in last) printf "%s\t%d\t%d\n", t, last[t], (t in peak) ? peak[t] : 0
    }' "$1" | LC_ALL=C sort -t "$(printf '\t')" -k2,2nr
}

# The memory suite's figures as Markdown: one table, nova-nix beside
# upstream where upstream reports the same thing.
write_memory() {
  local dir="$RESULTS_DIR/memory"
  local census="$dir/census/census.txt" stats="$dir/upstream/nix-stats.json"
  local figure peak theirs kind noun bound="" shared="" upstream_column="upstream"
  local allocated_count allocated_bytes reached_count reached_bytes total_allocated=0 total_reached=0
  local gc_figure objects mark_seconds index_seconds
  [[ -s "$dir/nova-nix/memory-max.txt" ]] && bound=$(cat "$dir/nova-nix/memory-max.txt")
  if [[ -s "$dir/upstream/visited.txt" ]]; then
    upstream_column="upstream: $SCOREBOARD_UPSTREAM_VERSION"
    shared=$(LC_ALL=C comm -12 <(LC_ALL=C sort "$dir/nova-nix/visited.txt") <(LC_ALL=C sort "$dir/upstream/visited.txt") | wc -l)
  fi
  echo "## nova-nix memory: $LABEL"
  echo
  echo "One process of each evaluator over the first $MEMORY_SLICE attributes of the scoreboard's walk of" \
    "\`$NIXPKGS_PATH\`, passing over the $(($(wc -l < "$dir/settled.txt"))) that end a process in either" \
    "evaluator or ran out of memory in one.  nova-nix runs under +RTS $HEAP_CAP; its census is taken when the slice ends, with the package" \
    "set still held, from the result, the root environment, the import cache and the search path."
  if [[ ! -s "$dir/upstream/visited.txt" ]]; then
    echo "No upstream pass ran, so its column is empty."
  fi
  echo
  scoreboard_row "Figure" "nova-nix" "$upstream_column"
  scoreboard_row ":--" "--:" "--:"
  scoreboard_row "attributes the pass visited" "$(($(wc -l < "$dir/nova-nix/visited.txt")))" \
    "$( [[ -s "$dir/upstream/visited.txt" ]] && echo "$(($(wc -l < "$dir/upstream/visited.txt"))), $shared of them both" )"
  scoreboard_row "of them evaluating to a drvPath" "$(($(wc -l < "$dir/nova-nix/evaluated.txt")))" \
    "$( [[ -s "$dir/upstream/evaluated.txt" ]] && echo "$(($(wc -l < "$dir/upstream/evaluated.txt")))" )"
  if [[ "$bound" =~ ^[0-9]+$ ]]; then
    scoreboard_row "memory bound" "$(mib "$bound"), the memory.max of the cgroup" ""
  fi
  scoreboard_row "exit status" "$(exit_figure "$dir/nova-nix" "$dir/nova-nix/err")" \
    "$( [[ -d "$dir/upstream" ]] && exit_figure "$dir/upstream" "$dir/upstream/err" )"
  scoreboard_row "wall time" "$(figure_or "$(pass_seconds "$dir/nova-nix/peak-rss.txt")" s "not measured")" \
    "$(figure_or "$(pass_seconds "$dir/upstream/peak-rss.txt")" s "")"
  scoreboard_row "OS peak RSS" "$(figure_or "$(peak_rss_mib "$dir/nova-nix/peak-rss.txt")" MiB "not measured")" \
    "$(figure_or "$(peak_rss_mib "$dir/upstream/peak-rss.txt")" MiB "")"
  figure=$(rts_figure "$dir/nova-nix/rts-stats.txt" total)
  scoreboard_row "CPU time" "$(figure_or "${figure% *}" s "not reported")" \
    "$(figure_or "$(upstream_stat "$stats" .time.cpu)" s "")"
  figure=$(rts_figure "$dir/nova-nix/rts-stats.txt" gc)
  gc_figure=""
  [[ -n "$figure" ]] && gc_figure="${figure#* } s elapsed, ${figure% *} s CPU"
  theirs=$(upstream_stat "$stats" '.time.gc // .time.gcNonIncremental')
  scoreboard_row "GC time" "${gc_figure:-not reported}" "$(figure_or "$theirs" "s in full collections" "")"
  scoreboard_row "GHC productivity" "$(rts_figure "$dir/nova-nix/rts-stats.txt" productivity)" ""
  scoreboard_row "GHC max residency" "$(figure_or "$(rts_figure "$dir/nova-nix/rts-stats.txt" residency)" MiB "not reported")" ""
  theirs=$(upstream_stat "$stats" .gc.heapSize)
  if [[ -s "$dir/census/heap.hp" ]]; then
    heap_figures "$dir/census/heap.hp" types > "$dir/census/heap-types.tsv"
    IFS=$'\t' read -r figure peak < <(heap_figures "$dir/census/heap.hp" totals)
    scoreboard_row "GHC heap when the slice ends, by -hT (peak sample)" "$(mib "$figure") ($(mib "$peak"))" \
      "$(figure_or "$(mib "$theirs")" "Boehm heap" "")"
    head -n "$MEMORY_TOP_CLOSURE_TYPES" "$dir/census/heap-types.tsv" \
      | while IFS=$'\t' read -r kind figure peak; do
        scoreboard_row "GHC heap: \`${kind//|/\\|}\`" "$(mib "$figure") ($(mib "$peak"))" ""
      done
  else
    scoreboard_row "GHC heap when the slice ends, by -hT" "not measured" "$(figure_or "$(mib "$theirs")" "Boehm heap" "")"
  fi
  if [[ -s "$census" ]]; then
    for kind in thunks envs attrsets lists lambdas primopapps apps ctxstrs strings symbols; do
      allocated_count=$(named_value "$census" "$kind.allocated.count")
      allocated_bytes=$(named_value "$census" "$kind.allocated.bytes")
      reached_count=$(named_value "$census" "$kind.reached.count")
      reached_bytes=$(named_value "$census" "$kind.reached.bytes")
      total_allocated=$((total_allocated + allocated_bytes))
      total_reached=$((total_reached + reached_bytes))
      case "$kind" in
        thunks) noun="thunk cells"; theirs=$(objects_figure "$(upstream_stat "$stats" .values.number)" "$(upstream_stat "$stats" .values.bytes)" values) ;;
        envs) noun="environments"; theirs=$(objects_figure "$(upstream_stat "$stats" .envs.number)" "$(upstream_stat "$stats" .envs.bytes)" envs) ;;
        attrsets) noun="attribute sets"; theirs=$(objects_figure "$(upstream_stat "$stats" .sets.number)" "$(upstream_stat "$stats" .sets.bytes)" sets) ;;
        lists) noun="list headers"; theirs=$(objects_figure "$(upstream_stat "$stats" .list.elements)" "$(upstream_stat "$stats" .list.bytes)" elements) ;;
        lambdas) noun="lambdas"; theirs="" ;;
        primopapps) noun="partially applied builtins"; theirs="" ;;
        apps) noun="deferred applications"; theirs="" ;;
        ctxstrs) noun="strings with context"; theirs="" ;;
        strings) noun="strings"; theirs="" ;;
        symbols) noun="symbols"; theirs=$(objects_figure "$(upstream_stat "$stats" .symbols.number)" "$(upstream_stat "$stats" .symbols.bytes)" symbols) ;;
      esac
      scoreboard_row "C $noun: allocated" "$(objects_figure "$allocated_count" "$allocated_bytes" "$noun")" "$theirs"
      scoreboard_row "C $noun: no root reaches" "$(unreached_figure "$allocated_bytes" "$reached_bytes" "$reached_count" "$noun")" ""
    done
    scoreboard_row "C allocator overhead (headers, unused tails, tracking arrays, symbol tables)" \
      "$(mib "$(named_value "$census" overhead.bytes)")" ""
    scoreboard_row "C bytecode" "$(mib "$(named_value "$census" code.bytes)")" \
      "$(figure_or "$(upstream_stat "$stats" .nrExprs)" expressions "")"
    scoreboard_row "C objects, all kinds: allocated" "$(mib "$total_allocated")" \
      "$(figure_or "$(mib "$(upstream_stat "$stats" .gc.totalBytes)")" "allocated by the GC in all" "")"
    scoreboard_row "**C objects, all kinds: no root reaches (M1)**" \
      "**$(unreached_figure "$total_allocated" "$total_reached" "" "" | sed 's/; reached:.*//')**" ""
    objects=$(named_value "$census" mark.objects)
    mark_seconds=$(named_value "$census" mark.seconds)
    index_seconds=$(named_value "$census" mark.index-seconds)
    scoreboard_row "census mark: objects it entered" "$objects objects, $(mib "$(named_value "$census" mark.bytes)")" ""
    scoreboard_row "census mark: wall time" "$mark_seconds s, after $index_seconds s counting and indexing" ""
    scoreboard_row "census mark: seconds per million objects" \
      "$(awk -v s="$mark_seconds" -v n="$objects" 'BEGIN { if (n > 0) printf "%.3f s", s / (n / 1000000) }')" ""
    scoreboard_row "census mark: pointers it could not place" "$(named_value "$census" mark.unplaced)" ""
    write_site_rows "$census" "$stats"
  else
    scoreboard_row "C census" "not taken: $(exit_figure "$dir/census" "$dir/census/err")" ""
  fi
  write_attrset_rows "$dir/attrset" "$census"
}

# The census of regions as Markdown: its totals, the closes by how much
# their regions allocated, and the simulated collectors by threshold, each
# with the most bytes held at once freeing single objects, freeing whole
# blocks of each size the census tried, and freeing whole lines of each
# length it tried.
write_regions() {
  local dir="$RESULTS_DIR/memory"
  local regions="$dir/regions/regions.txt"
  local allocated outside resurrected grains lines label
  local peaks=("Peak, single objects") aligns=("--:")
  echo "## Regions: $LABEL"
  echo
  if [[ ! -s "$regions" ]]; then
    echo "No census of regions: $(exit_figure "$dir/regions" "$dir/regions/err")"
    return 0
  fi
  allocated=$(named_value "$regions" regions.allocated.bytes)
  outside=$(named_value "$regions" regions.outside.bytes)
  resurrected=$(named_value "$regions" regions.resurrected.count)
  echo "A region is the extent of one force.  At each close the census marks what of the force's" \
    "allocations its writes into older cells and with-scopes, and the roots it registered, still reach" \
    "(cbits/nn_region.h)."
  echo
  scoreboard_row "Figure" "nova-nix"
  scoreboard_row ":--" "--:"
  scoreboard_row "wall time, beside the measured pass" \
    "$(figure_or "$(pass_seconds "$dir/regions/peak-rss.txt")" s "not measured"), $(figure_or "$(pass_seconds "$dir/nova-nix/peak-rss.txt")" s "not measured")"
  scoreboard_row "OS peak RSS" "$(figure_or "$(peak_rss_mib "$dir/regions/peak-rss.txt")" MiB "not measured")"
  scoreboard_row "regions closed" "$(named_value "$regions" regions.closes)"
  scoreboard_row "most regions open at once" "$(named_value "$regions" regions.depth)"
  scoreboard_row "most records held at once" "$(named_value "$regions" regions.records)"
  scoreboard_row "C bytes allocated" "$(mib "$allocated")"
  scoreboard_row "of them outside every region, which no close frees" \
    "$(awk -v all="$allocated" -v out="$outside" 'BEGIN { printf "%.1f MiB, %.1f%%", out / 1048576, (all > 0 ? 100 * out / all : 0) }')"
  scoreboard_row "pointers the census could not place" "$(named_value "$regions" regions.unplaced)"
  scoreboard_row "**reached after a close found them unreached**" \
    "**$resurrected, $(mib "$(named_value "$regions" regions.resurrected.bytes)")**"
  echo
  echo "### Closes by bytes their regions allocated"
  echo
  scoreboard_row "Allocated" "Closes" "Allocated" "Reached at close" "Objects entered" "Pointers examined"
  scoreboard_row "--:" "--:" "--:" "--:" "--:" "--:"
  awk '$1 == "regions.class" {
      floor = $2
      label = floor == 0 ? "nothing" : (floor < 1024 ? floor " B" : (floor < 1048576 ? floor / 1024 " KiB" : floor / 1048576 " MiB"))
      printf "| from %s | %d | %.1f MiB | %.1f%% | %d | %d |\n", label, $3, $4 / 1048576, ($4 > 0 ? 100 * $5 / $4 : 0), $6, $7
    }' "$regions"
  echo
  echo "### Collectors that mark at a close from a threshold of bytes allocated"
  echo
  grains=$(awk '$1 == "regions.grain" {
      printf "%s%s KiB thunks, %s KiB envs, %s KiB strings", sep, $3 / 1024, $4 / 1024, $5 / 1024; sep = "; " }' "$regions")
  lines=$(awk '$1 == "regions.line" { printf "%s%s B", sep, $3; sep = ", " }' "$regions")
  while read -r label; do
    peaks+=("$label")
    aligns+=("--:")
  done < <(awk '$1 == "regions.grain" { print (blocks++ ? $2 : "Peak, blocks " $2) }
      $1 == "regions.line" { print (lines++ ? $3 " B" : "Peak, lines " $3 " B") }' "$regions")
  echo "Peaks are the most C bytes held at once.  Blocks, largest first: $grains.${lines:+  Lines, longest first: $lines.}"
  echo
  scoreboard_row "Threshold" "Closes that mark" "Objects entered" "Pointers examined" "${peaks[@]}"
  scoreboard_row "--:" "--:" "--:" "--:" "${aligns[@]}"
  awk '$1 == "regions.threshold" {
      t = $2
      label = t == 0 ? "every close" : (t < 1048576 ? t / 1024 " KiB" : t / 1048576 " MiB")
      printf "| %s | %d | %d | %d", label, $3, $4, $5
      for (i = 6; i <= NF; i++) printf " | %.1f MiB", $i / 1048576
      printf " |\n"
    }' "$regions"
  echo
  echo "### Collectors that tenure what their closes mark"
  echo
  echo "Each marks at the closes of regions past its threshold, each only what no such close of its inside" \
    "tenured, and then tenures the whole region.  A close marks its region whole when it is outermost, or" \
    "when the closes inside have tenured at least the collector's budget since a mark of the whole.  The" \
    "collector above at the same threshold is beside them."
  echo
  scoreboard_row "Collector" "Closes that mark" "of them marking the whole region" "Objects entered" "Pointers examined" \
    "Freed by marks of the whole" "${peaks[@]}"
  scoreboard_row ":--" "--:" "--:" "--:" "--:" "--:" "${aligns[@]}"
  awk 'function budget(b) { return b == 0 ? "no budget" : "budget " b / 1048576 " MiB" }
    $1 == "regions.threshold" { row[$2] = $0 }
    $1 == "regions.tenure" { bytes = $3; tenure[++n] = $0 }
    END {
      if (n == 0 || !(bytes in row)) exit
      label = bytes < 1048576 ? bytes / 1024 " KiB" : bytes / 1048576 " MiB"
      m = split(row[bytes], r, " ")
      printf "| marking again at each close, %s | %d | | %d | %d | ", label, r[3], r[4], r[5]
      for (i = 6; i <= m; i++) printf " | %.1f MiB", r[i] / 1048576
      printf " |\n"
      for (k = 1; k <= n; k++) {
        m = split(tenure[k], t, " ")
        printf "| **tenuring, %s, %s** | %d | %d | %d | %d | %.1f MiB", budget(t[2]), label, t[4], t[5], t[6], t[7], t[8] / 1048576
        for (i = 9; i <= m; i++) printf " | %.1f MiB", t[i] / 1048576
        printf " |\n"
      }
    }' "$regions"
  echo
  scoreboard_row "Collector" "Most records held at once" "Pointers remembered for a region around" \
    "Most remembered at once" "**Units freed that the census's mark at the close reached**" \
    "**Reached after it freed them**"
  scoreboard_row ":--" "--:" "--:" "--:" "--:" "--:"
  awk 'function budget(b) { return b == 0 ? "no budget" : "budget " b / 1048576 " MiB" }
    $1 == "regions.tenure.records" { records[$2] = $3; order[++n] = $2 }
    $1 == "regions.tenure.remembered" { made[$2] = $3; held[$2] = $4 }
    $1 == "regions.tenure.reached" { reached[$2] = $3 }
    $1 == "regions.tenure.resurrected" { count[$2] = $3; bytes[$2] = $4 }
    END {
      for (k = 1; k <= n; k++) {
        v = order[k]
        printf "| tenuring, %s | %d | %d | %d | **%d** | **%d, %.1f MiB** |\n", budget(v), records[v], made[v], held[v], reached[v], count[v], bytes[v] / 1048576
      }
    }' "$regions"
}

# The census's environments and attribute sets by the site that allocated
# them, as rows of the memory table: the environments' slots beside
# upstream's values in environments, their sizes, then each site that
# allocated anything, most bytes first.  A set is shareable when it was
# made equal to one that already existed, which upstream would not make
# (nn_stats_count_set in cbits/nn_stats.h); the evaluator makes none, so
# the figure is 0 unless that regresses.
#   write_site_rows <census-file> <nix-stats-json>
write_site_rows() {
  local census="$1" stats="$2" name count figure bytes
  if [[ -z "$(named_value "$census" envs.allocated.slots)" ]]; then
    scoreboard_row "C allocations by site" "not counted" ""
    return 0
  fi
  scoreboard_row "C environments: slots" "$(named_value "$census" envs.allocated.slots)" \
    "$(figure_or "$(upstream_stat "$stats" .envs.elements)" "values in envs" "")"
  scoreboard_row "C environments by slots" \
    "$(awk '$1 == "envs.allocated.size" { printf "%s%s: %s", sep, $2, $3; sep = ", " }' "$census")" ""
  while read -r name count figure bytes; do
    scoreboard_row "C environments at \`$name\`" "$count envs, $figure slots, $(mib "$bytes")" ""
  done < <(site_figures "$census" envs slots)
  scoreboard_row "C attribute sets: shareable" \
    "$(awk '{ n = split($1, part, ".") } n == 4 && part[1] == "attrsets" && part[2] == "site" && part[4] == "shareable" { total += $2 }
      END { print total + 0 }' "$census")" ""
  while read -r name count figure bytes; do
    scoreboard_row "C attribute sets at \`$name\`" "$count sets, $figure shareable, $(mib "$bytes")" ""
  done < <(site_figures "$census" attrsets shareable)
}

# Each site of one kind that allocated anything, as "NAME COUNT FIGURE
# BYTES" with the figure named, most bytes first.
#   site_figures <census-file> <kind> <figure>
site_figures() {
  awk -v kind="$2" -v figure="$3" '
    { n = split($1, part, ".") }
    n == 4 && part[1] == kind && part[2] == "site" {
      if (!(part[3] in seen)) { seen[part[3]] = 1; order[++sites] = part[3] }
      value[part[3], part[4]] = $2
    }
    END {
      for (i = 1; i <= sites; i++) {
        s = order[i]
        if (value[s, "count"] > 0) print s, value[s, "count"], value[s, figure], value[s, "bytes"]
      }
    }' "$1" | LC_ALL=C sort -k4,4nr
}

# The attrset benchmark's rows of the memory table.  Bytes per entry are
# the copying run's: under the non-moving collector GHC reports an estimate
# for its heap.  A non-moving collection here marks inside its pause, where
# a running program would mark beside it.  The census's figure for the
# reached sets is the bytes they asked the allocator for, spare room
# included; the benchmark's is what the allocator holds for sets with none,
# its own overhead included.
#   write_attrset_rows <attrset-dir> <census-file>
write_attrset_rows() {
  local dir="$1" census="$2" representation collector run name figure bytes
  if [[ ! -s "$dir/c-copying.txt" ]]; then
    scoreboard_row "attribute set benchmark" "not run" ""
    return 0
  fi
  scoreboard_row "attribute set benchmark: sets built" \
    "$(named_value "$dir/c-copying.txt" sets) sets, $(named_value "$dir/c-copying.txt" entries) entries: the census's reached sets" ""
  scoreboard_row "C sets the census reached: bytes asked for per entry, spare room included" \
    "$(awk -v bytes="$(named_value "$census" attrsets.reached.bytes)" '{ entries += $1 * $2 }
      END { if (entries > 0) printf "%.1f", bytes / entries }' "$dir/sizes.txt")" ""
  for representation in c flat; do
    case "$representation" in
      c)
        name="C sets"
        bytes="bytes per entry the allocator holds, no spare room"
        ;;
      flat)
        name="GHC-heap sets (SmallArray values, PrimArray keys)"
        bytes="bytes per entry live in GHC's heap"
        ;;
    esac
    figure=$(named_value "$dir/$representation-copying.txt" bytes-per-entry)
    scoreboard_row "$name: $bytes" "$(awk -v b="$figure" 'BEGIN { if (b ~ /^[0-9.]+$/) printf "%.1f", b; else print b }')" ""
    for collector in copying nonmoving; do
      run="$dir/$representation-$collector.txt"
      figure="$(ms "$(named_value "$run" gc-pause-seconds)"), $(ms "$(named_value "$run" gc-cpu-seconds)") CPU"
      if [[ "$collector" == nonmoving ]]; then
        figure+=", $(ms "$(named_value "$run" mark-seconds)") of it marking"
      fi
      scoreboard_row "$name: a major GC under the $(named_value "$run" collector) collector" "$figure" ""
    done
  done
}

# ----------------------------------------------------------------
# Setup
# ----------------------------------------------------------------

# End a run on an interrupt.  The evaluator in the foreground takes a
# Ctrl-C itself and exits normally (nova-nix reports it and exits 1), after
# which bash would start the walk's next process, and the background walk
# ignores SIGINT as every asynchronous command of a script does.  The
# background walk's TERM trap waits for its evaluator, which pkill ends.
interrupt_scoreboard() {
  trap - INT TERM
  if [[ -n "$SCOREBOARD_UPSTREAM_PID" ]]; then
    kill -TERM "$SCOREBOARD_UPSTREAM_PID" 2> /dev/null || true
    pkill -TERM -P "$SCOREBOARD_UPSTREAM_PID" 2> /dev/null || true
  fi
  echo "the run was interrupted" >&2
  exit 130
}

# Remove the scratch directory.  The stores in it keep each path in the
# read-only modes a store gives every path it adds, as upstream's does, so
# the tree is made writable before it is removed.
remove_scoreboard_scratch() {
  chmod -R u+w "${SCOREBOARD_SCRATCH:?}"
  rm -rf "${SCOREBOARD_SCRATCH:?}"
}

# Check the label and the budget, find the nova-nix build, and set up what
# every mode shares: a scratch directory removed on exit, the interrupt
# handling, a fresh cache, SCOREBOARD_NOVA, the nova-nix command every
# evaluation starts from, and, with nix-instantiate on PATH,
# SCOREBOARD_UPSTREAM and SCOREBOARD_UPSTREAM_VERSION.
#
# Upstream evaluates with --read-write-mode because --eval alone implies
# read-only mode, in which a drvPath with deep context cannot be computed
# (the parity job's drvpath-cases.sh says why); import from derivation is
# off, as nixpkgs' own CI evaluates the set, so that neither evaluator
# builds during a walk.  An attribute on which a fresh upstream process
# runs out of memory too is unmeasured, not failed: the reference running
# out says nothing about the attribute.
scoreboard_prepare() {
  local nova_bin
  if [[ ! "$LABEL" =~ ^[A-Za-z0-9][A-Za-z0-9._-]*$ ]]; then
    echo "a label is letters, digits, dots, dashes and underscores" >&2
    return 2
  fi
  if [[ ! "$UPSTREAM_BUDGET" =~ ^[1-9][0-9]*$ || ! "$NOVA_BUDGET" =~ ^[1-9][0-9]*$ ]]; then
    echo "SCOREBOARD_BUDGET must be a positive number of attributes, not '${SCOREBOARD_BUDGET:-}'" >&2
    return 2
  fi
  bound_evaluators || return
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
  SCOREBOARD_NOVA=(${SCOREBOARD_BOUNDED[@]+"${SCOREBOARD_BOUNDED[@]}"} "$nova_bin" eval
    --nix-path "nixpkgs=$NIXPKGS_PATH" --nix-path "nix=$REPO_ROOT/data/nix")
  if command -v nix-instantiate > /dev/null 2>&1; then
    SCOREBOARD_UPSTREAM_VERSION=$(nix-instantiate --version)
    SCOREBOARD_UPSTREAM=(${SCOREBOARD_BOUNDED[@]+"${SCOREBOARD_BOUNDED[@]}"} nix-instantiate --eval --read-write-mode
      --option allow-import-from-derivation false -I "nixpkgs=$NIXPKGS_PATH")
  fi
}

# ----------------------------------------------------------------
# Modes
# ----------------------------------------------------------------

# The scoreboard: nova-nix's walk and, with nix-instantiate on PATH,
# upstream's beside it.  Returns 1 when either walk stopped early.
run_scoreboard() {
  local walks_complete=true
  scoreboard_prepare || return
  rm -rf "${RESULTS_DIR:?}/nova-nix" "${RESULTS_DIR:?}/upstream" "${RESULTS_DIR:?}/resources"
  mkdir -p "$RESULTS_DIR"
  walk_both "$RESULTS_DIR" "" || walks_complete=false
  if [[ -n "$SCOREBOARD_UPSTREAM_VERSION" ]]; then
    write_differences "$RESULTS_DIR/upstream/results.txt" "$RESULTS_DIR/nova-nix/results.txt" \
      > "$RESULTS_DIR/differences.txt"
  fi
  write_scoreboard > "$RESULTS_DIR/scoreboard.md"
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

# The attrset benchmark over the census's reached sets, under each
# collector, into <attrset-dir>/<representation>-<collector>.txt, under the
# bound as the evaluators are.
#   attrset_benchmark <bench-binary> <census-file> <attrset-dir>
attrset_benchmark() {
  local bench="$1" census="$2" dir="$3" representation collector
  local rts=()
  mkdir -p "$dir"
  awk '$1 == "attrsets.reached.size" { print $2, $3 }' "$census" > "$dir/sizes.txt"
  for representation in c flat; do
    for collector in copying nonmoving; do
      rts=()
      [[ "$collector" == nonmoving ]] && rts=(+RTS -xn -RTS)
      echo "attrset-memory: $representation sets under the $collector collector"
      ${SCOREBOARD_BOUNDED[@]+"${SCOREBOARD_BOUNDED[@]}"} "$bench" "$representation" "$dir/sizes.txt" ${rts[@]+"${rts[@]}"} \
        > "$dir/$representation-$collector.txt" 2> "$dir/$representation-$collector.err" \
        || echo "attrset-memory: $representation under $collector exited $?" >&2
    done
  done
}

# The memory suite, its figures written to memory/memory.md.  Returns 1
# when a walk of the slice stopped, since the passes would then cover
# attributes the slice does not.
run_memory() {
  local dir="$RESULTS_DIR/memory" bench_bin evaluator
  scoreboard_prepare || return
  if [[ ! "$MEMORY_SLICE" =~ ^[1-9][0-9]*$ ]]; then
    echo "MEMORY_SLICE must be a positive number of attributes, not '$MEMORY_SLICE'" >&2
    return 2
  fi
  bench_bin=$(cd "$REPO_ROOT" && cabal list-bin -v0 bench:attrset-memory) || bench_bin=""
  if [[ ! -x "$bench_bin" ]]; then
    echo "no attrset-memory build at $bench_bin; build it, or pass --build" >&2
    return 2
  fi
  rm -rf "${dir:?}"
  mkdir -p "$dir"
  if ! walk_both "$dir/slice" "$MEMORY_SLICE"; then
    echo "a walk of the slice stopped before it settled $MEMORY_SLICE attributes" >&2
    return 1
  fi
  # Every attribute that ends a process in either evaluator, so that both
  # passes visit the same attributes wherever the walks agree, and every
  # one a process ran out of memory at before a fresh one settled it: the
  # pass carries the whole slice's state to it, more than that process did.
  for evaluator in nova-nix upstream; do
    if [[ -s "$dir/slice/$evaluator/settled.txt" ]]; then
      awk '$1 == "error" || $1 == "unmeasured"' "$dir/slice/$evaluator/settled.txt"
      awk '{ print "retried", $1 }' "$dir/slice/$evaluator/retried.txt"
    fi
  done | LC_ALL=C sort -u -k2,2 > "$dir/settled.txt"
  settled_json "$dir/settled.txt" > "$dir/settled.json"

  # Stores of their own, as the resource pass's.
  measured_pass "$dir/nova-nix" "$dir/settled.json" "$(pass_budget "$dir/settled.txt" "$dir/slice/nova-nix/settled.txt")" \
    "$MEMORY_WALK" "${SCOREBOARD_NOVA[@]}" --store "$SCOREBOARD_SCRATCH/memory-store" \
    +RTS -s"$dir/nova-nix/rts-stats.txt" "$HEAP_CAP" -RTS
  echo "memory: nova-nix's pass visited $(($(wc -l < "$dir/nova-nix/visited.txt"))) attributes, exit $(cat "$dir/nova-nix/exit.txt")"
  measured_pass "$dir/census" "$dir/settled.json" "$(pass_budget "$dir/settled.txt" "$dir/slice/nova-nix/settled.txt")" \
    "$MEMORY_WALK" env NOVA_NIX_C_STATS="$dir/census/census.txt" "${SCOREBOARD_NOVA[@]}" \
    --store "$SCOREBOARD_SCRATCH/census-store" \
    +RTS -hT -i"$HEAP_SAMPLE_SECONDS" -po"$dir/census/heap" "$HEAP_CAP" -RTS
  echo "memory: nova-nix's census pass exited $(cat "$dir/census/exit.txt")"
  measured_pass "$dir/regions" "$dir/settled.json" "$(pass_budget "$dir/settled.txt" "$dir/slice/nova-nix/settled.txt")" \
    "$MEMORY_WALK" env NOVA_NIX_REGION_STATS="$dir/regions/regions.txt" "${SCOREBOARD_NOVA[@]}" \
    --store "$SCOREBOARD_SCRATCH/regions-store" \
    +RTS "$HEAP_CAP" -RTS
  echo "memory: nova-nix's region census pass exited $(cat "$dir/regions/exit.txt")"
  if [[ ${#SCOREBOARD_UPSTREAM[@]} -gt 0 ]]; then
    measured_pass "$dir/upstream" "$dir/settled.json" "$(pass_budget "$dir/settled.txt" "$dir/slice/upstream/settled.txt")" \
      "$MEMORY_WALK" env NIX_SHOW_STATS=1 NIX_SHOW_STATS_PATH="$dir/upstream/nix-stats.json" "${SCOREBOARD_UPSTREAM[@]}"
    echo "memory: upstream's pass visited $(($(wc -l < "$dir/upstream/visited.txt"))) attributes, exit $(cat "$dir/upstream/exit.txt")"
  fi
  if [[ -s "$dir/census/census.txt" ]]; then
    attrset_benchmark "$bench_bin" "$dir/census/census.txt" "$dir/attrset"
  fi
  { write_memory && echo && write_regions; } > "$dir/memory.md"
  echo
  cat "$dir/memory.md"
}

# ----------------------------------------------------------------
# Argument parsing
# ----------------------------------------------------------------

usage() {
  echo "Usage: $0 <label> --scoreboard|--resources|--memory [-M<cap>] [--build]"
  echo ""
  echo "  label:         name for this run (e.g. 'nixpkgs-24.11')"
  echo "  --scoreboard:  walk the package set and count what evaluates and matches upstream"
  echo "  --resources:   force what the label's walk evaluated in one process, for its figures"
  echo "  --memory:      measure where one process's memory goes over a slice of the walk"
  echo "  -M<cap>:       nova-nix's heap cap (default $DEFAULT_HEAP_CAP)"
  echo "  --build:       build what the mode runs first"
}

if [[ $# -lt 1 ]]; then
  usage
  exit 1
fi

LABEL="$1"
shift

FLAG_BUILD=false
MODE=""

for arg in "$@"; do
  case "$arg" in
    --build) FLAG_BUILD=true ;;
    --scoreboard | --resources | --memory)
      if [[ -n "$MODE" ]]; then
        echo "one of --scoreboard, --resources and --memory" >&2
        exit 2
      fi
      MODE="${arg#--}"
      ;;
    -M*) HEAP_CAP="$arg" ;;
    *)
      usage >&2
      exit 2
      ;;
  esac
done

if [[ -z "$MODE" ]]; then
  usage >&2
  exit 2
fi

# ----------------------------------------------------------------
# Main
# ----------------------------------------------------------------

RESULTS_DIR="$REPO_ROOT/profiling/results/$LABEL"

case "$MODE" in
  scoreboard)
    [[ "$FLAG_BUILD" == true ]] && do_build exe:nova-nix
    run_scoreboard
    ;;
  resources)
    [[ "$FLAG_BUILD" == true ]] && do_build exe:nova-nix
    run_resources
    ;;
  memory)
    [[ "$FLAG_BUILD" == true ]] && do_build exe:nova-nix bench:attrset-memory
    run_memory
    ;;
esac
