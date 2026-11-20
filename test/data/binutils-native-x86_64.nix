# pkgs/windows' binutils-native-x86_64, as the native x86_64 (Windows) CI
# jobs build it.  Each check has busybox's sh run a script that writes
# lines to its output, without the carriage returns msvcrt's text-mode
# files end a line with, which CI compares with the file of this directory
# named for it.  A step that fails leaves its lines out.
#
# link is test/data/binutils-x86_64.nix's, with the tools by their own
# names: dlltool makes an import library for kernel32's ExitProcess, as
# assembles a function that exits through it with 42, ar archives the
# object and ld links it as a program's entry point, which runs; then as,
# given --32, and ld, given the i386pe emulation, link a function that
# returns 42 as an i386 program's entry point, which runs too.  It writes
# what that check writes, which is the same, since the release and the
# target are the same, and then what makes these tools the native ones,
# for each of as.exe and ld.exe: the machine and optional header magic of
# its PE header, 0x8664 and 0x20b, an x86_64 PE32+ image, where
# binutils-x86_64's are i386 PE32; KERNEL32.dll and msvcrt.dll, which
# every MinGW-w64 program imports, among the DLLs it imports, so that the
# count after them is known to read its import table; and how many of
# those DLLs are GCC's runtime libraries, winpthreads or mcfgthreads,
# which is none, since gcc-x86_64's libgcc and these tools' libraries are
# static.
#
# stamps lists every program and DLL binutils-native-x86_64 installs with
# the TimeDateStamp of its COFF file header, as test/data/gcc-latest.nix's
# stamps does, and CI compares the lines with
# test/data/binutils-native-x86_64-stamps-answer.txt, which lists the
# programs the build installs, each with 0.
let
  inherit (import ../../pkgs/windows)
    stage0-pe32
    busybox-w32
    binutils-native-x86_64
    ;
  inherit (stage0-pe32) writeText;
  kaem = import ./check.nix stage0-pe32.kaem;

  # The machine and the optional header's magic of a PE image, in hex, as
  # od reads them (see test/data/binutils-x86_64.nix).
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
    "$bin/dlltool.exe" --input-def kernel32.def --output-lib libkernel32.a
    "$bin/as.exe" -o answer.o answer.s
    "$bin/ar.exe" rc libanswer.a answer.o
    "$bin/ld.exe" --no-insert-timestamp -e answer -o answer.exe answer.o libkernel32.a
    status=0
    ./answer.exe || status=$?
    "$bin/as.exe" --32 -o answer32.o answer32.s
    "$bin/ld.exe" -m i386pe --no-insert-timestamp -e _answer -o answer32.exe answer32.o
    status32=0
    ./answer32.exe || status32=$?
    {
      "$bin/ld.exe" --version | head -n 1
      "$bin/nm.exe" -g -p answer.o
      "$bin/ar.exe" t libanswer.a
      "$bin/objdump.exe" -f answer.exe | grep 'file format'
      pe_header answer.exe
      echo "answer.exe exits $status"
      "$bin/nm.exe" -g answer32.o
      "$bin/objdump.exe" -f answer32.exe | grep 'file format'
      pe_header answer32.exe
      echo "answer32.exe exits $status32"
      for program in bin/as.exe bin/ld.exe; do
        (cd "$2" && pe_header "$program")
        imports=$(cd "$2" && "$bin/objdump.exe" -p "$program" | tr -d '\r' | grep "DLL Name")
        printf '%s\n' "$imports" | grep -i -e "kernel32.dll$" -e "msvcrt.dll$" | sort -u
        echo "$program imports of GCC's runtime, winpthreads or mcfgthreads: $(printf '%s\n' "$imports" | grep -c -i -e libgcc -e libstdc -e libssp -e libatomic -e libgomp -e libquadmath -e libwinpthread -e libmcfgthread)"
      done
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
  link = kaem.runCommand "binutils-native-x86_64-link" { } ''
    ${busybox-w32}/bin/busybox.exe sh ${linkScript} ''${out} ${binutils-native-x86_64}
  '';

  stamps = kaem.runCommand "binutils-native-x86_64-stamps" { } ''
    ${busybox-w32}/bin/busybox.exe sh ${stampsScript} ''${out} ${binutils-native-x86_64}
  '';
}
