# pkgs/windows' busybox-w32, as the Bootstrap (Windows) CI jobs build it:
# version, a script that runs busybox --version; and configure-tools, in
# which busybox's sh runs configure-tools.sh through the applets configure
# runs (sh, sed, grep, awk, expr, tr, cat, mkdir, rm, cp, mv, test and
# printf) and writes a line of what each did to its output, which CI
# compares with the lines it expects.  A line whose test fails is left out.
let
  inherit (import ../../pkgs/windows) stage0-pe32 busybox-w32;
  inherit (stage0-pe32) writeText;
  kaem = import ./check.nix stage0-pe32.kaem;

  script = writeText "configure-tools.sh" ''
    set -e
    out=$1
    record() {
      printf '%s\n' "$*" >> "$out"
    }
    : > "$out"

    n=$((6 * 7))
    record "sh arithmetic $n"
    v=configure.ac
    record "sh expansion ''${v%.ac} ''${v#conf} ''${#v}"
    case $v in
      *.ac) record "sh case ac" ;;
      *) record "sh case other" ;;
    esac
    greet() {
      echo "hello $1"
    }
    record "sh function $(greet world)"
    for word in a b c; do
      printf '%s' "$word"
    done > loop.txt
    record "sh loop $(cat loop.txt)"
    cat > child.sh <<'EOF'
    echo "child $1"
    exit 3
    EOF
    status=0
    sh child.sh here > child.txt || status=$?
    record "sh child $(cat child.txt) status $status"

    mkdir -p dir/sub
    test -d dir/sub && record "mkdir -p"
    printf 'one\ntwo\nthree\n' > dir/sub/lines.txt
    record "cat $(cat dir/sub/lines.txt | tr '\n' ,)"
    cp dir/sub/lines.txt copy.txt
    mv copy.txt moved.txt
    test ! -e copy.txt && test -f moved.txt && record "cp mv"
    rm -r dir moved.txt
    test ! -e dir && test ! -e moved.txt && record "rm -r"
    record "$(printf 'printf %s %03d %x' done 7 255)"
    [ 3 -lt 10 ] && [ abc != abd ] && [ -n "$v" ] && [ -z "" ] && record "test"

    printf 'one\ntwo\nthree\n' > lines.txt
    record "sed $(echo 'CC=gcc -O2' | sed 's/^\([^=]*\)=\(.*\)$/\1 is \2/')"
    record "sed -n $(sed -n '2p' lines.txt)"
    record "grep -c $(grep -c o lines.txt)"
    grep -q '^th' lines.txt && record "grep -q"
    record "grep -E $(grep -E 'w|h' lines.txt | tr '\n' ,)"
    record "grep -v $(grep -v o lines.txt)"
    record "awk $(printf '1 2\n3 4\n' | awk '{ s += $1 * $2 } END { print s }')"
    record "awk -F $(echo a:b:c | awk -F: '{ print $NF, NF }')"
    record "expr $(expr 2 + 3) $(expr conftest.c : '\(.*\)\.c')"
    record "tr $(echo abc-def | tr a-z A-Z | tr -d -)"
  '';
in
{
  version = kaem.runCommand "busybox-w32-get-version" { } ''
    ${busybox-w32}/bin/busybox.exe --version
    mkdir ''${out}
  '';

  configure-tools = kaem.runCommand "busybox-w32-configure-tools" { } ''
    ${busybox-w32}/bin/busybox.exe sh ${script} ''${out}
  '';
}
