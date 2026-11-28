#!/usr/bin/env bash
# What process-audit.sh decides for the process creations of a stage, one
# case per rule.  Each stage's job runs these before it turns the audit
# on, since a check that cannot fail is a run that tests nothing and stays
# green.
#
# Portable to bash 3.2, which is what macOS ships.
#
# No set -e: a mismatch is the output, not a reason to stop before the rest
# of the cases have run.
set -uo pipefail

script=$(dirname "${BASH_SOURCE[0]}")/process-audit.sh
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
checked=0
failures=0

readonly KAEM='C:\nix\store\0123456789abcdfghijklmnpqrsvwxyz-kaem\bin\kaem.exe'
readonly CP='C:\nix\store\0123456789abcdfghijklmnpqrsvwxyz-mescc-tools-extra\bin\cp.exe'
readonly NOVA='C:\a\_temp\bin\nova-nix.exe'
readonly CMD='C:\Windows\System32\cmd.exe'
readonly CONHOST='C:\Windows\System32\conhost.exe'
readonly CONHOST_COMMAND='\??\C:\Windows\system32\conhost.exe 0xffffffff -ForceV1'

# event PARENT CHILD COMMAND: a process creation as the Security log
# renders it.
event() {
  printf '<Event xmlns="http://schemas.microsoft.com/win/2004/08/events/event"><System><EventID>4688</EventID><EventRecordID>7</EventRecordID></System>'
  printf "<EventData><Data Name='NewProcessId'>0x1</Data><Data Name='NewProcessName'>%s</Data><Data Name='CommandLine'>%s</Data><Data Name='ParentProcessName'>%s</Data></EventData></Event>" "$2" "$3" "$1"
}

# expect STATUS TEXT LABEL: process-audit.sh's scan of the events written
# to $work/events exits STATUS and prints a line holding TEXT.
expect() {
  local out got
  checked=$((checked + 1))
  out=$(bash "$script" scan "$work/events" 2>&1)
  got=$?
  if [ "$got" = "$1" ] && printf '%s\n' "$out" | grep -qF -- "$2"; then
    printf 'ok    %s\n' "$3"
  else
    printf 'FAIL  %s: exited %s, wanted %s and a line holding %s, printed:\n' "$3" "$got" "$1" "$2"
    printf '%s\n' "$out" | sed 's/^/      /'
    failures=$((failures + 1))
  fi
}

{ event "$NOVA" "$KAEM" "kaem.exe --file build.kaem"; event "$KAEM" "$CP" "cp.exe a b"; } > "$work/events"
expect 0 "2 processes started, 1 of them by a program in the store, 0 of those console hosts, 0 from outside the store" "a store program that starts store programs passes"

{ event "$KAEM" "$CP" "cp.exe a b"; event "$KAEM" "$CMD" "cmd.exe /c copy a b"; } > "$work/events"
expect 1 "::error::$KAEM started $CMD, which is not in the store: cmd.exe /c copy a b" "a store program that starts cmd.exe fails, naming both and the command line"

{ event "$NOVA" "$KAEM" "kaem.exe"; event "$NOVA" "$CMD" "cmd.exe /c ver"; event "$KAEM" "$CP" "cp.exe"; } > "$work/events"
expect 0 "3 processes started, 1 of them by a program in the store" "a program from outside the store may start any program"

{ event "$KAEM" "$CP" "cp.exe"; event 'c:\NIX\STORE\0123456789abcdfghijklmnpqrsvwxyz-kaem\bin\kaem.exe' 'C:/nix/store/0123456789abcdfghijklmnpqrsvwxyz-x/bin/x.exe' "x.exe"; } > "$work/events"
expect 0 "2 of them by a program in the store, 0 of those" "the store is matched without regard to case or separator"

event "$KAEM" 'C:\nix\storefront\x.exe' "x.exe" > "$work/events"
expect 1 "which is not in the store" "a directory that only begins like the store is outside it"

event "$KAEM" "$CMD" 'cmd.exe /c &quot;a &amp; b&quot; &lt;in &gt;out &apos;q&apos;' > "$work/events"
expect 1 "cmd.exe /c \"a & b\" <in >out 'q'" "the command line is shown with the log's escapes undone"

{ event "$KAEM" "$CP" "cp.exe"; event "$KAEM" "$CONHOST" "$CONHOST_COMMAND"; event "$CP" 'c:\windows\system32\CONHOST.EXE' "$CONHOST_COMMAND"; } > "$work/events"
expect 0 "3 processes started, 3 of them by a program in the store, 2 of those console hosts, 0 from outside the store" "the console host Windows starts for a console program passes"

event "$KAEM" "$CONHOST" 'conhost.exe --headless --width 80' > "$work/events"
expect 1 "::error::$KAEM started $CONHOST, which is not in the store: conhost.exe --headless --width 80" "conhost.exe with another command line fails"

event "$KAEM" 'C:\Windows\System32\cmd.exe' "$CONHOST_COMMAND" > "$work/events"
expect 1 "which is not in the store" "another program with the console host's command line fails"

event "$NOVA" "$KAEM" "kaem.exe" > "$work/events"
expect 1 "the audit recorded none of the stage's builds" "an audit that holds no process a store program started fails"

: > "$work/events"
expect 1 "0 processes started" "an empty log fails"

echo "$checked cases, $failures failed"
[ "$failures" -eq 0 ]
