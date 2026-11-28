# What pkgs/windows' bash runs, as test/data/bash.nix's checks run it: the
# builder is bash itself, with this script, and writes one line for each
# thing checked to $out, which CI compares with
# test/data/bash-checks-answer.txt.  It stops at the first command that
# fails unexpectedly, under set -e with inherit_errexit, and so writes
# fewer lines.
#
# PATH is two store directories spelled as the builder spells them, with
# drive letters, joined with `:': probe's, a program, and scripts', a #!
# script whose interpreter is this bash and one whose interpreter is
# probe.

# The builder's environment sets out, probe and scripts.
# shellcheck disable=SC2154

set -eu
shopt -s inherit_errexit

exec > "$out"

PATH="$probe\\bin:$scripts\\bin"

# Functions, positional parameters and a nameref.
set_through () {
  local -n target=$1
  # shellcheck disable=SC2034 # target is a nameref
  target=$2
}
value=before
set_through value after
echo "function and local -n: $value"

# Indexed and associative arrays.
words=(alpha beta gamma)
echo "array: ${#words[@]} elements, the second ${words[1]}"
declare -A colours=([sky]=blue [grass]=green)
echo "associative array: sky ${colours[sky]}, grass ${colours[grass]}, ${#colours[@]} keys"

if shopt -q inherit_errexit; then
  echo "shopt: inherit_errexit is on"
fi

# A program found on PATH: its arguments as it reads them back, its
# output to a file, and its exit status.
status=0
# shellcheck disable=SC1003 # the backslashes are the argument
probe args 3 plain "two words" 'a "quote"' 'back\slash\' '' > probe.out || status=$?
echo "probe exited $status"
while IFS= read -r line; do
  echo "probe read $line"
done < probe.out

# Writing, appending, and reading a file as a program's standard input.
probe args 0 first > lines.txt
probe args 0 second >> lines.txt
probe cat < lines.txt > copy.txt
while IFS= read -r line; do
  echo "probe cat copied $line"
done < copy.txt

# /dev/null takes a program's output.
probe args 0 discarded > /dev/null
echo "/dev/null took probe's output"

# A #! script found on PATH, and its exit status.
status=0
greet > greet.out || status=$?
read -r line < greet.out
echo "greet printed: $line"
echo "greet exited $status"

# A #! script whose interpreter is another program, which reads the
# script's standard input: `probe cat SCRIPT'.
relay < lines.txt > relay.out
while IFS= read -r line; do
  echo "relay copied $line"
done < relay.out

# A command PATH does not hold.
status=0
no-such-command 2> /dev/null || status=$?
echo "no-such-command exited $status"
