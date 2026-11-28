#!/usr/bin/env bash
# Compare `registry list` and `registry resolve` with upstream Nix's.
#
# Each side reads the same registries: a user registry under
# XDG_CONFIG_HOME, a machine registry in NIX_CONF_DIR, and a global one
# that flake-registry names, a fixture file or upstream's default, which is
# downloaded.  A row compares what both print on stdout and their exit
# status; a refusal compares the final error line, and a registry that
# cannot be read the warning that says so.
#
# NOVA_NIX_BIN names the executable under test; nix comes from PATH.
# Registries are a flakes feature, so the rows run against 2.36.0 and
# later, the release the flakes support follows.
#
# No set -e: a mismatch is the output, not a reason to stop.
set -uo pipefail

novaBin=${NOVA_NIX_BIN:?NOVA_NIX_BIN names the nova-nix executable}

upstreamVersion=$(nix --version | head -n1 | sed 's/.* //')
upstreamAtLeast() {
  [ "$(printf '%s\n%s\n' "$1" "$upstreamVersion" | sort -V | head -n1)" = "$1" ]
}
if ! upstreamAtLeast 2.36.0; then
  echo "skipped: registries follow 2.36.0, and this row's upstream is $upstreamVersion"
  exit 0
fi

fixture=$(mktemp -d)
trap 'rm -rf "$fixture"' EXIT
mkdir -p "$fixture/config/nix" "$fixture/conf" "$fixture/empty-config" "$fixture/empty-conf"

flakes='nix-command flakes'
checked=0
failures=0

# Write a version 2 registry of FROM-ID TO-ATTRIBUTES pairs to FILE, the
# attributes a JSON object's members.
registry() {
  local file=$1 first=1 entries=''
  shift
  while [ "$#" -gt 1 ]; do
    [ "$first" -eq 1 ] || entries="$entries,"
    entries="$entries{\"from\": {\"type\": \"indirect\", \"id\": \"$1\"}, \"to\": {$2}}"
    first=0
    shift 2
  done
  printf '{"version": 2, "flakes": [%s]}\n' "$entries" >"$file"
}

registry "$fixture/config/nix/registry.json" \
  mine '"type": "github", "owner": "me", "repo": "mine"' \
  nixpkgs '"type": "github", "owner": "me", "repo": "nixpkgs", "dir": "pkgs"'
registry "$fixture/conf/registry.json" \
  machine '"type": "git", "url": "https://example.org/machine.git", "ref": "main"'
registry "$fixture/global.json" \
  nixpkgs '"type": "github", "owner": "NixOS", "repo": "nixpkgs", "ref": "nixpkgs-unstable"' \
  pkgs '"type": "indirect", "id": "nixpkgs"' \
  loop '"type": "indirect", "id": "loop"' \
  tarball '"type": "tarball", "url": "https://example.org/a.tar.gz"'
printf '{"version": 1, "flakes": []}\n' >"$fixture/old.json"
printf '{"version": 2, "flakes": [{"from": {"type": "indirect", "id": "a"}, "to": {"type": "github", "owner": ["o"], "repo": "a"}}]}\n' >"$fixture/bad.json"

# Run one side with the registries of a row: CONFIG the config home,
# CONF the machine's directory, GLOBAL the flake-registry setting (unset
# for upstream's default), FEATURES the features, then the command.
side() {
  local config=$1 conf=$2 global=$3 features=$4 setting=''
  shift 4
  [ "$global" = default ] || setting="flake-registry = $global"
  XDG_CONFIG_HOME="$config" NIX_CONF_DIR="$conf" NIX_CONFIG="$setting" "$@" --extra-experimental-features "$features"
}

# A row both sides must print the same for, on stdout, with the same
# status.  What each printed on stderr is shown when the row differs.
row() {
  local label=$1 config=$2 conf=$3 global=$4 features=$5 upstream nova upstreamStatus novaStatus
  shift 5
  checked=$((checked + 1))
  upstream=$(cd "$fixture" && side "$config" "$conf" "$global" "$features" nix registry "$@" 2>"$fixture/upstream.err")
  upstreamStatus=$?
  nova=$(cd "$fixture" && side "$config" "$conf" "$global" "$features" "$novaBin" registry "$@" 2>"$fixture/nova.err")
  novaStatus=$?
  if [ "$upstreamStatus" -eq "$novaStatus" ] && [ "$upstream" = "$nova" ]; then
    printf '  ok    %s\n' "$label"
  else
    failures=$((failures + 1))
    printf '  DIFF  %s\n' "$label"
    printf '          upstream    (exit %s) %s\n' "$upstreamStatus" "$upstream"
    printf '          nova-nix    (exit %s) %s\n' "$novaStatus" "$nova"
    printf '          upstream stderr  %s\n' "$(cat "$fixture/upstream.err")"
    printf '          nova-nix stderr  %s\n' "$(cat "$fixture/nova.err")"
  fi
}

# A row both sides must refuse on the same final error line.
refused() {
  local label=$1 config=$2 conf=$3 global=$4 features=$5 upstream nova
  shift 5
  checked=$((checked + 1))
  upstream=$(cd "$fixture" && side "$config" "$conf" "$global" "$features" nix registry "$@" 2>&1 >/dev/null | sed 's/^ *//' | grep '^error: ' | tail -n1)
  nova=$(cd "$fixture" && side "$config" "$conf" "$global" "$features" "$novaBin" registry "$@" 2>&1 >/dev/null | sed 's/^ *//' | grep '^error: ' | tail -n1)
  if [ -n "$upstream" ] && [ "$upstream" = "$nova" ]; then
    printf '  ok    %s\n' "$label"
  else
    failures=$((failures + 1))
    printf '  DIFF  %s\n' "$label"
    printf '          upstream    %s\n' "$upstream"
    printf '          nova-nix    %s\n' "$nova"
  fi
}

# A row both sides must warn of in the same words, on the lines that say a
# registry cannot be read, and print the same for on stdout.
warned() {
  local label=$1 config=$2 conf=$3 global=$4 features=$5 upstream nova upstreamOut novaOut
  shift 5
  checked=$((checked + 1))
  upstreamOut=$(cd "$fixture" && side "$config" "$conf" "$global" "$features" nix registry "$@" 2>"$fixture/upstream.err")
  novaOut=$(cd "$fixture" && side "$config" "$conf" "$global" "$features" "$novaBin" registry "$@" 2>"$fixture/nova.err")
  upstream=$(grep -E 'cannot (read|parse) flake registry' "$fixture/upstream.err")
  nova=$(grep -E 'cannot (read|parse) flake registry' "$fixture/nova.err")
  if [ -n "$upstream" ] && [ "$upstream" = "$nova" ] && [ "$upstreamOut" = "$novaOut" ]; then
    printf '  ok    %s\n' "$label"
  else
    failures=$((failures + 1))
    printf '  DIFF  %s\n' "$label"
    printf '          upstream    %s / %s\n' "$upstream" "$upstreamOut"
    printf '          nova-nix    %s / %s\n' "$nova" "$novaOut"
  fi
}

user="$fixture/config"
machine="$fixture/conf"
noUser="$fixture/empty-config"
noMachine="$fixture/empty-conf"
global="$fixture/global.json"

echo "== registry list =="
row "every registry's entries, in order" "$user" "$machine" "$global" "$flakes" list
row "no global registry when the setting is empty" "$user" "$machine" "" "$flakes" list
row "upstream's default global registry" "$noUser" "$noMachine" default "$flakes" list
warned "a registry of another version" "$noUser" "$noMachine" "$fixture/old.json" "$flakes" list
warned "an entry with an attribute of the wrong type" "$noUser" "$noMachine" "$fixture/bad.json" "$flakes" list
warned "a registry entry needing flakes with the feature off" "$user" "$noMachine" "" nix-command list
echo

echo "== registry resolve =="
row "nixpkgs, from the user's registry with its dir" "$user" "$machine" "$global" "$flakes" resolve nixpkgs
row "nixpkgs, from the global registry" "$noUser" "$noMachine" "$global" "$flakes" resolve nixpkgs
row "a ref carried over" "$noUser" "$noMachine" "$global" "$flakes" resolve nixpkgs/nixos-24.05
row "a revision carried over" "$noUser" "$noMachine" "$global" "$flakes" resolve nixpkgs/2aae6c35c94fcfb415dbe95f408b9ce91ee846ed
row "an entry naming another" "$noUser" "$noMachine" "$global" "$flakes" resolve pkgs
row "a machine entry" "$noUser" "$machine" "$global" "$flakes" resolve machine
row "a direct reference" "$noUser" "$noMachine" "$global" "$flakes" resolve github:NixOS/nix
row "several references" "$user" "$noMachine" "$global" "$flakes" resolve mine nixpkgs github:a/b
row "nixpkgs, from upstream's default global registry" "$noUser" "$noMachine" default "$flakes" resolve nixpkgs
refused "a reference no entry maps" "$noUser" "$noMachine" "$global" "$flakes" resolve absent
refused "an entry that maps a reference to itself" "$noUser" "$noMachine" "$global" "$flakes" resolve loop
refused "a ref for a tarball" "$noUser" "$noMachine" "$global" "$flakes" resolve tarball/main
refused "a forge given a ref and a revision" "$noUser" "$noMachine" "$global" "$flakes" resolve nixpkgs/main/2aae6c35c94fcfb415dbe95f408b9ce91ee846ed
refused "a registry reference with flakes off" "$noUser" "$noMachine" "$global" nix-command resolve nixpkgs
echo

echo "$checked checked, $failures differing"
[ "$failures" -eq 0 ]
