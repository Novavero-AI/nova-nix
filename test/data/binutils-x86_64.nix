# pkgs/windows' binutils-x86_64, as the x86_64 (Windows) CI jobs build it.
# Each check has busybox's sh run a script that writes lines to its
# output, without the carriage returns msvcrt's text-mode files end a line
# with, which CI compares with the file of this directory named for it.  A
# step that fails leaves its lines out.
#
# link has dlltool make an import library for kernel32's ExitProcess
# from a .def file, as assemble a function that calls ExitProcess with
# 42, ar archive the object, and ld link it with the import library as a
# program's entry point, and the program run; then as, given --32, and
# ld, given the i386pe emulation, link a function that returns 42 as an
# i386 program's entry point, as test/data/binutils.nix does for i686,
# and that program run.  dlltool assembles parts of the library with the
# as of its own directory and prefix (binutils/dlltool.c:4189-4260).  The
# x86_64 program exits through ExitProcess because under the x86_64
# emulation of Windows ARM64 the value its entry point returns is not its
# exit status, as it is on x64 and for an i386 program on either.  The
# function takes 40 bytes of stack before the call, as the x64 calling
# convention has it: 32 of shadow space for the callee, and 8 that
# realign the stack to 16 bytes, which the call that entered the
# function left 8 bytes off.  It writes the version line ld prints, what
# nm, ar and objdump read back, each program's exit status, and the
# machine and optional header magic of each program's PE header, which
# od reads from the file and not binutils: 0x8664 and 0x20b, PE32+, for
# x86_64, and 0x14c and 0x10b, PE32, for i386 (the PE format's
# IMAGE_FILE_MACHINE_AMD64, IMAGE_FILE_MACHINE_I386 and the two magic
# numbers).  od reads an integer in the machine's byte order, which on
# Windows is the PE format's little-endian.  The answer,
# test/data/binutils-x86_64-link-answer.txt, is derived: nm prints a
# 64-bit address in 16 digits, or 16 blanks for an undefined symbol
# (binutils/nm.c:1855-1860), and an x86_64 PE symbol with no underscore
# (bfd/config.bfd:719); the import library defines __imp_ExitProcess as
# the slot of the import address table the loader fills with
# ExitProcess's address (binutils/dlltool.c:2420); nm sorts by the
# collation of Windows' own locale (see test/data/gcc46.nix), so it
# lists answer.o's symbols unsorted, in the order COFF writes them, the
# undefined after the defined (bfd/coffgen.c:810-853); objdump names the
# formats pei-x86-64 and pei-i386 (bfd/pei-x86_64.c:28, bfd/pei-i386.c);
# and the x86_64 program's exit status is the value it passes
# ExitProcess, the i386 program's the value its one thread's entry point
# returns.
#
# stamps lists every program and DLL binutils-x86_64 installs with the
# TimeDateStamp of its COFF file header, as test/data/gcc-latest.nix's
# stamps does, and CI compares the lines with
# test/data/binutils-x86_64-stamps-answer.txt, which lists the programs
# the build installs, each with 0.
let
  inherit (import ../../pkgs/windows)
    stage0-pe32
    busybox-w32
    binutils-x86_64
    ;
  inherit (stage0-pe32) writeText;
  kaem = import ./check.nix stage0-pe32.kaem;

  # The machine and the optional header's magic of a PE image, in hex, as
  # od reads them: the PE header's offset is the 32-bit value at 60, the
  # machine the 16 bits after its 4-byte signature, and the magic the 16
  # bits after the 20-byte COFF header.
  peHeader = ''
    pe_header() {
      header=$(( $(od -A n -t u4 -j 60 -N 4 "$1") ))
      machine=$(( $(od -A n -t u2 -j $((header + 4)) -N 2 "$1") ))
      magic=$(( $(od -A n -t u2 -j $((header + 24)) -N 2 "$1") ))
      printf '%s: machine 0x%x, optional header magic 0x%x\n' "$1" "$machine" "$magic"
    }
  '';

  linkScript = writeText "link.sh" ''
    out=$1
    bin=$2/bin
    prefix=$bin/x86_64-w64-mingw32-
    ${peHeader}
    cat > kernel32.def <<'EOF'
    LIBRARY kernel32.dll
    EXPORTS
    ExitProcess
    EOF
    cat > answer.s <<'EOF'
        .text
        .globl answer
    answer:
        subq $40, %rsp
        movl $42, %ecx
        call *__imp_ExitProcess(%rip)
    EOF
    cat > answer32.s <<'EOF'
        .text
        .globl _answer
    _answer:
        movl $42, %eax
        ret
    EOF
    "''${prefix}dlltool.exe" --input-def kernel32.def --output-lib libkernel32.a
    "''${prefix}as.exe" -o answer.o answer.s
    "''${prefix}ar.exe" rc libanswer.a answer.o
    "''${prefix}ld.exe" --no-insert-timestamp -e answer -o answer.exe answer.o libkernel32.a
    status=0
    ./answer.exe || status=$?
    "''${prefix}as.exe" --32 -o answer32.o answer32.s
    "''${prefix}ld.exe" -m i386pe --no-insert-timestamp -e _answer -o answer32.exe answer32.o
    status32=0
    ./answer32.exe || status32=$?
    {
      "''${prefix}ld.exe" --version | head -n 1
      "''${prefix}nm.exe" -g -p answer.o
      "''${prefix}ar.exe" t libanswer.a
      "''${prefix}objdump.exe" -f answer.exe | grep 'file format'
      pe_header answer.exe
      echo "answer.exe exits $status"
      "''${prefix}nm.exe" -g answer32.o
      "''${prefix}objdump.exe" -f answer32.exe | grep 'file format'
      pe_header answer32.exe
      echo "answer32.exe exits $status32"
    } | tr -d '\r' > "$out"
  '';

  stampsScript = writeText "stamps.sh" ''
    out=$1
    cd "$2" || exit
    find . -type f | sort | while read -r file; do
      case $file in
        *.exe | *.dll) ;;
        *) continue ;;
      esac
      header=$(( $(od -A n -t u4 -j 60 -N 4 "$file") ))
      signature=$(od -A n -t x1 -j "$header" -N 4 "$file" | tr -d ' ')
      if [ "$signature" = 50450000 ]; then
        echo "$file $(( $(od -A n -t u4 -j $((header + 8)) -N 4 "$file") ))"
      else
        echo "$file has no PE signature"
      fi
    done > "$out"
  '';
in
{
  link = kaem.runCommand "binutils-x86_64-link" { } ''
    ${busybox-w32}/bin/busybox.exe sh ${linkScript} ''${out} ${binutils-x86_64}
  '';

  stamps = kaem.runCommand "binutils-x86_64-stamps" { } ''
    ${busybox-w32}/bin/busybox.exe sh ${stampsScript} ''${out} ${binutils-x86_64}
  '';
}
