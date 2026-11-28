# context-to-unified.awk -- a context diff, as GNU publishes bash's
# official patches, rewritten as the unified diff busybox's patch applies.
#
# Each file's header becomes `--- a/NAME' and `+++ b/NAME', NAME being the
# file the `---' line names, so the result applies with -p1 from the top
# of the source tree.  Each hunk's old and new sections are merged in
# order: lines both keep, then for a change the old lines removed before
# the new lines added.  An empty section means the other's context lines
# are all it holds.  Text outside the file headers and hunks, such as the
# patch's description, is dropped.  A line that is empty, where the file
# has an empty line, counts as one while its section's range holds more,
# as GNU patch reads it.

function flush_hunk(    i, j, nold, nnew, o, n, out, k) {
  if (!in_hunk)
    return
  nold = old_count
  nnew = new_count
  if (nold == 0)
    for (j = 1; j <= nnew; j++)
      if (substr(new_line[j], 1, 1) == " ")
        old_line[++nold] = new_line[j]
  if (nnew == 0)
    for (i = 1; i <= nold; i++)
      if (substr(old_line[i], 1, 1) == " ")
        new_line[++nnew] = old_line[i]
  k = 0
  i = 1
  j = 1
  while (i <= nold || j <= nnew) {
    o = (i <= nold) ? substr(old_line[i], 1, 1) : ""
    n = (j <= nnew) ? substr(new_line[j], 1, 1) : ""
    if (o == "-") {
      out[++k] = "-" substr(old_line[i++], 3)
    } else if (n == "+") {
      out[++k] = "+" substr(new_line[j++], 3)
    } else if (o == "!" || n == "!") {
      while (i <= nold && substr(old_line[i], 1, 1) == "!")
        out[++k] = "-" substr(old_line[i++], 3)
      while (j <= nnew && substr(new_line[j], 1, 1) == "!")
        out[++k] = "+" substr(new_line[j++], 3)
    } else {
      out[++k] = " " substr(old_line[i], 3)
      i++
      j++
    }
  }
  printf "@@ -%s +%s @@\n", range(old_start, old_end), range(new_start, new_end)
  for (i = 1; i <= k; i++)
    print out[i]
  in_hunk = 0
}

# A context range as a unified one: START,COUNT, where a section of no
# lines starts at the line before it.
function range(start, end) {
  if (end < start)
    return start "," 0
  return start "," (end - start + 1)
}

function split_range(text, which,    parts, n, start, end) {
  # A single number N is the line N alone, or no lines when N is 0.
  n = split(text, parts, ",")
  start = parts[1] + 0
  end = (n > 1) ? parts[2] + 0 : (start == 0 ? -1 : start)
  if (which == "old") {
    old_start = start
    old_end = end
  } else {
    new_start = start
    new_end = end
  }
}

/^\*\*\* [^0-9*]/ {
  # The old file's name; the new file's line follows.
  flush_hunk()
  pending_old = 1
  next
}

/^--- / && pending_old {
  name = $2
  sub(/\t.*/, "", name)
  print "--- a/" name
  print "+++ b/" name
  pending_old = 0
  next
}

/^\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*/ {
  flush_hunk()
  in_hunk = 1
  section = ""
  old_count = 0
  new_count = 0
  next
}

in_hunk && /^\*\*\* [0-9]/ {
  r = $2
  split_range(r, "old")
  section = "old"
  next
}

in_hunk && /^--- [0-9]/ {
  r = $2
  split_range(r, "new")
  section = "new"
  next
}

in_hunk && section == "old" && /^[ !+-] / {
  old_line[++old_count] = $0
  next
}

in_hunk && section == "new" && /^[ !+-] / {
  new_line[++new_count] = $0
  next
}

# An empty line of the file whose trailing space the patch lost: a bare
# marker, or, while its section's range holds more lines, nothing at all.
in_hunk && section == "old" && old_count < old_end - old_start + 1 && /^[ !+-]?$/ {
  old_line[++old_count] = ($0 == "" ? " " : $0) " "
  next
}

in_hunk && section == "new" && new_count < new_end - new_start + 1 && /^[ !+-]?$/ {
  new_line[++new_count] = ($0 == "" ? " " : $0) " "
  next
}

{
  flush_hunk()
}

END {
  flush_hunk()
}
