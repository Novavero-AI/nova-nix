#!/usr/bin/env bash
# Check that a profile nova-nix keeps goes through the generations a
# profile upstream Nix keeps does, step for step, and that both build a
# builtin:buildenv derivation into the same tree.
#
# Fixture packages are directories made in the scratch directory: files in
# bin that two of them share, a share that is a link to a directory beside
# one that is a directory, a dot file, a nix-support to skip, and a
# propagated-user-env-packages naming another fixture.  Each side adds them
# as the same source paths: `nix-store --add` upstream, an evaluation of
# builtins.path with --read-write-mode here.  Both sides then run one
# sequence on a profile of their own (add, add again, add with --priority,
# remove, rollback, add after the rollback, wipe-history), and after every
# step the two must agree on the command's status, the profile's current
# generation, each generation and the store path it names, manifest.json's
# bytes, and `profile list --json`.  Every generation's tree, and the
# environment buildenv.nix builds, must then dump to the same NAR on both
# sides.  Messages are not compared: the rows' upstreams quote paths
# differently.
#
# A profile's tree links into the store it was made in, so the two trees
# hold the same bytes only when both stores are /nix/store.  Upstream's
# store is the runner's; nova-nix's side runs in a private mount namespace
# with a scratch directory bound over /nix, so its store is /nix/store too
# while upstream's /nix is untouched outside it.  That side runs as this
# user, through passwordless sudo, which makes the namespace, and setpriv,
# which leaves root before nova-nix runs.  Upstream's builtin:buildenv reads
# packages through their /nix/store paths in the client, so its side cannot
# use a store of its own under the scratch directory.
#
# NOVA_NIX_BIN names the executable under test; nix, nix-store, nix-build
# and nix-instantiate come from PATH.  Upstream runs with nix-command
# enabled, which its newest releases still leave off by default, and the
# oldest row's upstream calls `add` `install`.
#
# No set -e: each step's failure is reported with what it printed.
set -uo pipefail

# Report a failed step with the log it left, and stop.
fail() {
  echo "FAIL  $1" >&2
  if [ -n "${2:-}" ] && [ -r "$2" ]; then sed 's/^/      /' "$2" >&2; fi
  exit 1
}

# The sequence, one step per line: a word for the command, then what it
# takes.  A package is named by its fixture's directory.
steps='add alpha-1.0
add delta-1.0
add-priority 4 alt-2.0
remove alpha
rollback
add beta-2.0
wipe-history'

# What a profile is after a step, under OUT: the step's status, the
# generation the profile's link names, each generation's link in order of
# number with what it names, and when the profile has a manifest, its bytes
# and what `profile list --json` prints.  The links are read as they are
# spelled, never followed.  The arguments after the status are the side's
# command, which lists the profile given `list --json`.
snapshot() {
  local out=$1 profile=$2 status=$3 link number
  shift 3
  mkdir -p "$out"
  echo "$status" >"$out/status"
  readlink "$profile" >"$out/current" 2>/dev/null || echo none >"$out/current"
  : >"$out/generations"
  for link in "$profile"-*-link; do
    [ -L "$link" ] || continue
    number=${link##"$profile"-}
    number=${number%-link}
    printf '%s %s\n' "$number" "$(readlink "$link")" >>"$out/generations"
  done
  sort -n -o "$out/generations" "$out/generations"
  if [ -e "$profile/manifest.json" ]; then
    cp "$profile/manifest.json" "$out/manifest.json"
    "$@" </dev/null >"$out/list.json" 2>/dev/null || echo failed >"$out/list.json"
  fi
}

# Run the sequence on PROFILE, a snapshot after each step under OUT, with
# the side's command as PROFILE_COMMAND, which takes a step's words and the
# profile, and the fixtures' store paths in the files under PATHS.
runSequence() {
  local out=$1 profile=$2 paths=$3 profileCommand=$4 n=0 word first second
  while read -r word first second; do
    n=$((n + 1))
    case "$word" in
    add) set -- add "$(cat "$paths/$first")" ;;
    add-priority) set -- add --priority "$first" "$(cat "$paths/$second")" ;;
    remove) set -- remove "$first" ;;
    rollback) set -- rollback ;;
    wipe-history) set -- wipe-history ;;
    *) fail "no such step: $word" ;;
    esac
    "$profileCommand" "$profile" "$@" </dev/null >"$out/$n.log" 2>&1
    snapshot "$out/$n" "$profile" "$?" "$profileCommand" "$profile" list --json
  done <<<"$steps"
}

# nova-nix's side, run in the namespace by the part below it, as this user:
# add the fixtures as upstream did, check each lands at upstream's path,
# run the sequence, and evaluate and build the buildenv derivation.
if [ "${1:-}" = "--nova-side" ]; then
  scratch=$2
  novaBin=$3
  repoRoot=$4
  # Called by name, through runSequence.
  # shellcheck disable=SC2329
  novaProfile() {
    local profile=$1
    shift
    "$novaBin" profile "$@" --profile "$profile"
  }
  for fixture in "$scratch"/fixtures/*; do
    name=${fixture##*/}
    added=$("$novaBin" eval --read-write-mode --expr "builtins.path { path = /. + \"$fixture\"; }" 2>"$scratch/nova/add.log") ||
      fail "nova-nix adding $name" "$scratch/nova/add.log"
    added=${added#\"}
    added=${added%\"}
    [ "$added" = "$(cat "$scratch/paths/$name")" ] ||
      fail "nova-nix added $name as $added, where upstream added it as $(cat "$scratch/paths/$name")"
  done
  runSequence "$scratch/nova" "$scratch/nova-profiles/profile" "$scratch/paths" novaProfile
  "$novaBin" eval --expr "(import $repoRoot/.github/parity/buildenv.nix { fixtures = \"$scratch/fixtures\"; }).drvPath" \
    >"$scratch/nova/buildenv.drv" 2>"$scratch/nova/buildenv.log" ||
    fail "nova-nix evaluating buildenv.nix" "$scratch/nova/buildenv.log"
  "$novaBin" build "$repoRoot/.github/parity/buildenv.nix" --argstr fixtures "$scratch/fixtures" --no-out-link \
    >"$scratch/nova/buildenv.out" 2>"$scratch/nova/buildenv.log" ||
    fail "nova-nix building buildenv.nix" "$scratch/nova/buildenv.log"
  exit 0
fi

novaBin=${NOVA_NIX_BIN:?set NOVA_NIX_BIN to the nova-nix executable}
repoRoot=$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)
for tool in nix nix-store nix-build nix-instantiate sudo unshare setpriv; do
  command -v "$tool" >/dev/null 2>&1 || {
    echo "$tool is not on PATH; this script needs it" >&2
    exit 1
  }
done
sudo -n true 2>/dev/null || {
  echo "this script needs passwordless sudo to make a mount namespace" >&2
  exit 1
}

# A registered store path is read-only, so the cleanup makes the scratch
# tree writable before removing it.
scratch=$(mktemp -d) || exit 1
trap 'chmod -R u+w "$scratch" 2>/dev/null; rm -rf "$scratch"' EXIT
mkdir "$scratch/fixtures" "$scratch/paths" "$scratch/upstream" "$scratch/nova" \
  "$scratch/upstream-profiles" "$scratch/nova-profiles" "$scratch/nova-root" \
  "$scratch/upstream-home" "$scratch/nova-home" "$scratch/nova-conf"

echo "upstream: $(nix --version 2>&1 | head -n1)"
echo "nova-nix: $("$novaBin" --version 2>&1 | head -n1)"
echo

upstreamNix() {
  HOME=$scratch/upstream-home NIX_STATE_HOME=$scratch/upstream-home/state XDG_STATE_HOME=$scratch/upstream-home/state \
    nix --extra-experimental-features nix-command "$@"
}
# `add --help` succeeds only where `add` is a command; where it is not,
# `install` is.
if upstreamNix profile add --help </dev/null >/dev/null 2>&1; then
  addWord=add
else
  addWord=install
fi
upstreamProfile() {
  local profile=$1
  shift
  if [ "$1" = add ]; then
    shift
    set -- "$addWord" "$@"
  fi
  upstreamNix profile "$@" --profile "$profile"
}

# Make a fixture package from lines of `kind path [content]`: a file, an
# executable, a link to content, each under the package's directory.
fixture() {
  local name=$1 dir=$scratch/fixtures/$1 kind path content
  mkdir -p "$dir"
  while read -r kind path content; do
    [ -n "$kind" ] || continue
    mkdir -p "$(dirname "$dir/$path")"
    case "$kind" in
    file) printf '%s\n' "$content" >"$dir/$path" ;;
    program)
      printf '#!/bin/sh\necho %s\n' "$content" >"$dir/$path"
      chmod +x "$dir/$path"
      ;;
    link) ln -s "$content" "$dir/$path" ;;
    esac
  done
  nix-store --add "$dir" >"$scratch/paths/$name" 2>"$scratch/add.log" ||
    fail "upstream adding $name" "$scratch/add.log"
}

fixture gamma-1.0 <<'EOF'
program bin/gamma gamma
EOF
gamma=$(cat "$scratch/paths/gamma-1.0")
fixture alpha-1.0 <<'EOF'
program bin/alpha alpha
file data/alpha.txt alpha's data
link share data
file .hidden not linked
EOF
fixture delta-1.0 <<EOF
program bin/delta delta
file share/delta/doc.txt delta's documentation
file nix-support/propagated-user-env-packages $gamma
EOF
fixture alt-2.0 <<'EOF'
program bin/alpha another alpha
EOF
fixture beta-2.0 <<'EOF'
program bin/beta beta
file share/beta.txt beta's share
EOF
for path in "$scratch"/paths/*; do echo "fixture:  $(cat "$path")"; done
echo

runSequence "$scratch/upstream" "$scratch/upstream-profiles/profile" "$scratch/paths" upstreamProfile
upstreamDrv=$(nix-instantiate "$repoRoot/.github/parity/buildenv.nix" --argstr fixtures "$scratch/fixtures" 2>"$scratch/upstream/buildenv.log") ||
  fail "upstream instantiating buildenv.nix" "$scratch/upstream/buildenv.log"
upstreamOut=$(nix-build "$upstreamDrv" --no-out-link 2>"$scratch/upstream/buildenv.log") ||
  fail "upstream building buildenv.nix" "$scratch/upstream/buildenv.log"

# nova-nix reads no nix.conf of the runner's and no substituter.
sudo -n unshare --mount --propagation private -- \
  sh -c 'root=$1; uid=$2; gid=$3; shift 3; mount --bind "$root" /nix && exec setpriv --reuid="$uid" --regid="$gid" --init-groups -- "$@"' \
  sh "$scratch/nova-root" "$(id -u)" "$(id -g)" \
  env -i PATH=/usr/local/bin:/usr/bin:/bin HOME="$scratch/nova-home" NIX_STATE_HOME="$scratch/nova-home/state" \
  NIX_CONF_DIR="$scratch/nova-conf" XDG_CONFIG_HOME="$scratch/nova-home/config" XDG_CONFIG_DIRS="$scratch/nova-conf" \
  NIX_CONFIG="substituters =
trusted-public-keys =" \
  "$BASH" "$repoRoot/.github/parity/profile-cases.sh" --nova-side "$scratch" "$novaBin" "$repoRoot" ||
  fail "nova-nix's side in its mount namespace"

# After every step, each of which succeeds upstream, the two agree on the
# profile.  A difference is shown with what each side's command printed.
n=0
while read -r step; do
  n=$((n + 1))
  [ "$(cat "$scratch/upstream/$n/status")" = 0 ] ||
    fail "upstream failed '$step'" "$scratch/upstream/$n.log"
  if ! diff -r "$scratch/upstream/$n" "$scratch/nova/$n" >"$scratch/diff.log" 2>&1; then
    {
      echo "upstream's output:"
      cat "$scratch/upstream/$n.log"
      echo "nova-nix's output:"
      cat "$scratch/nova/$n.log"
    } >>"$scratch/diff.log"
    fail "after '$step', the profiles differ (upstream's first)" "$scratch/diff.log"
  fi
  printf '  ok    %s\n' "$step"
done <<<"$steps"

# Each tree either side made dumps to the same NAR as the other's.
dumpHash() {
  nix-store --dump "$1" 2>/dev/null | sha256sum | cut -d' ' -f1
}
cut -d' ' -f2 "$scratch"/upstream/*/generations | sort -u >"$scratch/trees"
while read -r tree; do
  base=${tree#/nix/store/}
  [ "$(dumpHash "/nix/store/$base")" = "$(dumpHash "$scratch/nova-root/store/$base")" ] ||
    fail "generation $base dumps differently on the two sides"
done <"$scratch/trees"
printf '  ok    %s generations dump to the same NAR on both sides\n' "$(wc -l <"$scratch/trees" | tr -d ' ')"

novaDrv=$(tr -d '"' <"$scratch/nova/buildenv.drv")
[ "$novaDrv" = "$upstreamDrv" ] ||
  fail "buildenv.nix's drvPath is $novaDrv under nova-nix and $upstreamDrv upstream"
novaOut=$(tail -n1 "$scratch/nova/buildenv.out")
[ "$novaOut" = "$upstreamOut" ] ||
  fail "buildenv.nix builds $novaOut under nova-nix and $upstreamOut upstream"
base=${upstreamOut#/nix/store/}
[ "$(dumpHash "$upstreamOut")" = "$(dumpHash "$scratch/nova-root/store/$base")" ] ||
  fail "buildenv.nix's output $base dumps differently on the two sides"
printf '  ok    buildenv.nix: %s, built to %s on both sides\n' "${upstreamDrv##*/}" "$base"
