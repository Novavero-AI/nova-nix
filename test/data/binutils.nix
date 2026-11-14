# pkgs/windows' binutils, as the Bootstrap (Windows) CI jobs build it:
# version, a script that runs ld --version, as nixpkgs' get-version test
# does; and answer, in which busybox's sh has as assemble a function that
# returns 42, ar archive the object, ld link it as a program's entry point,
# and the program run; and ar archive and extract a file holding a 0x1A
# byte and a CR LF, which a file msvcrt opens as text would cut short or
# change.  It writes what nm, ar and objdump read back of each file, the
# program's exit status and whether the extracted file is the archived one
# to its output, without the carriage returns msvcrt's text-mode stdout ends
# a line with, and CI compares that with the lines it expects.  A line whose
# step fails is left out.
let
  inherit (import ../../pkgs/windows) stage0-pe32 busybox-w32 binutils;
  inherit (stage0-pe32) writeText;
  kaem = import ./check.nix stage0-pe32.kaem;

  script = writeText "answer.sh" ''
    out=$1
    bin=$2/bin
    cat > answer.s <<'EOF'
        .text
        .globl _answer
    _answer:
        movl $42, %eax
        ret
    EOF
    "$bin/i686-w64-mingw32-as.exe" -o answer.o answer.s
    "$bin/i686-w64-mingw32-ar.exe" rc libanswer.a answer.o
    "$bin/i686-w64-mingw32-ld.exe" -e _answer -o answer.exe answer.o
    status=0
    ./answer.exe || status=$?
    printf 'AAAA\r\nBBBB\032CCCC' > ctrlz.bin
    "$bin/i686-w64-mingw32-ar.exe" rc libctrlz.a ctrlz.bin
    mkdir extracted
    (cd extracted && "$bin/i686-w64-mingw32-ar.exe" x ../libctrlz.a)
    {
      "$bin/i686-w64-mingw32-nm.exe" -g answer.o
      "$bin/i686-w64-mingw32-ar.exe" t libanswer.a
      "$bin/i686-w64-mingw32-objdump.exe" -f answer.exe | grep 'file format'
      echo "answer.exe exits $status"
      if cmp -s ctrlz.bin extracted/ctrlz.bin; then
        echo "ctrlz.bin comes out of its archive whole"
      fi
    } | tr -d '\r' > "$out"
  '';
in
{
  version = kaem.runCommand "binutils-get-version" { } ''
    ${binutils}/bin/i686-w64-mingw32-ld.exe --version
    mkdir ''${out}
  '';

  answer = kaem.runCommand "binutils-answer" { } ''
    ${busybox-w32}/bin/busybox.exe sh ${script} ''${out} ${binutils}
  '';
}
