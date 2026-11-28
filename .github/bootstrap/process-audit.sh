#!/usr/bin/env bash
# Fail if a program from the store started a program from outside it in a
# seed ladder stage's job, as a build that runs the host's cmd.exe does,
# which no hash pins.  Windows' audit of process creation records every
# process that starts, with the image of the process that started it, so
# what it shows holds for every build and check the job ran, whatever their
# logs say and whatever PATH held: a program can start another by a path,
# and Windows looks a bare name up in its own directories before PATH.
#
#   start  turns the audit of process creation on, by its subcategory's
#          GUID, which no language of the runner renames, with command
#          lines; makes the Security log large enough to hold the stage's
#          events; turns Windows Error Reporting off, whose WerFault.exe a
#          program that crashes would start; and appends the log's newest
#          record to GITHUB_ENV as AUDIT_FIRST_RECORD, the stage's events
#          being the ones after it.
#   check  reads the process creations since AUDIT_FIRST_RECORD and fails
#          on each whose creator's image is in the store and whose own is
#          not, naming both and its command line.  It fails as well if the
#          log no longer holds every event since then, since a log that
#          overwrote some cannot say what they were, and if no process a
#          store program started is among them, since every stage's job
#          builds a program that starts another, so the audit recorded
#          nothing.
#   scan FILE  reads FILE, events as check writes them from the log, and
#          decides as check does; process-audit-cases.sh runs it.
#
# Git for Windows' bash rewrites an argument that begins with '/' into a
# Windows path before it starts a program that is not its own, and
# auditpol, wevtutil and reg take their options so, so the rewriting is
# turned off for every program this starts (MSYS2_ARG_CONV_EXCL).
#
# Usage: process-audit.sh start | check | scan FILE
# Required environment:
#   GITHUB_ENV           for start, the file a step appends to, to set the
#                        environment of the job's later steps
#   AUDIT_FIRST_RECORD   for check, the record start appended
#   RUNNER_TEMP          for check, where the events are written
set -euo pipefail

export MSYS2_ARG_CONV_EXCL='*'

# The audit subcategory Process Creation, whose events are 4688.
readonly PROCESS_CREATION='{0CCE922B-69AE-11D9-BED3-505054503030}'
readonly PROCESS_CREATION_EVENT=4688
readonly AUDIT_KEY='HKLM\Software\Microsoft\Windows\CurrentVersion\Policies\System\Audit'
readonly WER_KEY='HKLM\SOFTWARE\Microsoft\Windows\Windows Error Reporting'
# The largest size the Security log takes, in bytes, a multiple of 64 KB:
# a GCC stage can start some hundreds of thousands of processes, each event
# a kilobyte or two.
readonly LOG_BYTES=4294901760
# The store's directory, with forward slashes: awk reads a backslash in
# an assigned value as an escape, and the images are compared with their
# backslashes turned to slashes, and without regard to case, as Windows
# compares paths.
readonly STORE_PREFIX='C:/nix/store/'

fail() {
  echo "::error::$*" >&2
  exit 1
}

# The EventRecordID of the Security log's newest or oldest event, or 0 for
# an empty log: DIRECTION is true for the newest.
record_at() {
  wevtutil qe Security /c:1 "/rd:$1" /f:xml | sed -n 's|.*<EventRecordID>\([0-9]*\)</EventRecordID>.*|\1|p'
}

# The verdict on a file of events: each process creation a store program
# started whose image is outside the store, as an error line, then the
# counts.  Exits 1 on any, or when no store program started one.
scan() {
  awk -v store="$STORE_PREFIX" '
    BEGIN { RS = "</Event>"; prefix = tolower(store) }
    function field(name,    pattern, text) {
      pattern = "<Data Name=.?" name ".?>[^<]*</Data>"
      if (!match($0, pattern)) return ""
      text = substr($0, RSTART, RLENGTH)
      sub(/^<Data Name=[^>]*>/, "", text)
      sub(/<\/Data>$/, "", text)
      gsub(/&lt;/, "<", text); gsub(/&gt;/, ">", text); gsub(/&quot;/, "\"", text)
      gsub(/&apos;/, "\047", text); gsub(/&amp;/, "\\&", text)
      return text
    }
    function inStore(image,    folded) {
      folded = tolower(image)
      gsub(/\\/, "/", folded)
      return index(folded, prefix) == 1
    }
    /<EventID[ >]/ {
      events++
      parent = field("ParentProcessName")
      child = field("NewProcessName")
      if (!inStore(parent)) next
      fromStore++
      if (inStore(child)) next
      outside++
      print "::error::" parent " started " child ", which is not in the store: " field("CommandLine")
    }
    END {
      print events + 0 " processes started, " fromStore + 0 " of them by a program in the store, " outside + 0 " of those from outside it"
      if (fromStore == 0) {
        print "::error::no process a program in the store started is among them, so the audit recorded none of the stage'"'"'s builds"
        exit 1
      }
      exit (outside > 0 ? 1 : 0)
    }
  ' "$1"
}

case ${1:-} in
  start)
    : "${GITHUB_ENV:?}"
    auditpol /set "/subcategory:$PROCESS_CREATION" /success:enable > /dev/null
    reg add "$AUDIT_KEY" /v ProcessCreationIncludeCmdLine_Enabled /t REG_DWORD /d 1 /f > /dev/null
    reg add "$WER_KEY" /v Disabled /t REG_DWORD /d 1 /f > /dev/null
    wevtutil sl Security "/ms:$LOG_BYTES"
    first=$(record_at true)
    echo "AUDIT_FIRST_RECORD=${first:-0}" >> "$GITHUB_ENV"
    echo "auditing process creation after record ${first:-0}"
    ;;
  check)
    : "${AUDIT_FIRST_RECORD:?}" "${RUNNER_TEMP:?}"
    oldest=$(record_at false)
    # Every event after the first record is in the log only if the oldest
    # it holds is no later than the one after it.
    [ "${oldest:-0}" -le $((AUDIT_FIRST_RECORD + 1)) ] ||
      fail "the Security log's oldest event is record $oldest, after record $AUDIT_FIRST_RECORD, so it no longer holds every process the stage started"
    events="$RUNNER_TEMP/process-creation.xml"
    wevtutil qe Security "/q:*[System[(EventID=$PROCESS_CREATION_EVENT) and (EventRecordID>$AUDIT_FIRST_RECORD)]]" /f:xml > "$events"
    scan "$events"
    ;;
  scan)
    scan "${2:?the file of events}"
    ;;
  *)
    echo "usage: process-audit.sh start | check | scan FILE" >&2
    exit 1
    ;;
esac
