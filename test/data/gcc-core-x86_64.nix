# pkgs/windows' gcc-core-x86_64, as the x86_64 (Windows) CI jobs build it.
# Each check has busybox's sh run a script that writes lines to its
# output, without the carriage returns msvcrt's text-mode files end a line
# with, which CI compares with the file of this directory named for it.  A
# step that fails leaves its lines out.
#
# link has the driver read windows.h, and with it MinGW-w64's central
# headers, and compile answer.c to an object that binutils-x86_64's ld
# links, with libgcc and no runtime, as a program's entry point, and the
# program run.  The function divides two unsigned 128-bit integers built
# from values read at run time, which libgcc's __udivti3 computes: the
# numerator is 42 times the divisor plus 5, so the quotient is 42.  The
# script writes the target and version the driver reports, its thread
# model, which is single, the libgcc and text symbols of the object, the
# machine and optional header magic of the program's PE header, which od
# reads from the file and not binutils, and the program's exit status.
# The answer, test/data/gcc-core-x86_64-link-answer.txt, is derived: the
# lines are the configuration's, x86_64 PE names a C symbol with no
# underscore (gcc/config/i386/cygming.h:60-61), GCC calls
# __udivti3 for an unsigned 128-bit division (gcc/optabs.def:146), and
# 0x8664 and 0x20b are the PE format's IMAGE_FILE_MACHINE_AMD64 and PE32+.
# nm sorts by the collation of Windows' own locale (see
# test/data/gcc46.nix), so it lists the symbols unsorted and busybox's
# sort, which compares bytes, sorts them.
#
# stamps lists every program and DLL gcc-core-x86_64 installs with the
# TimeDateStamp of its COFF file header, as test/data/gcc-latest.nix's
# stamps does, and CI compares the lines with
# test/data/gcc-core-x86_64-stamps-answer.txt, which lists the programs the
# build installs, each with 0.
let
  inherit (import ../../pkgs/windows)
    stage0-pe32
    busybox-w32
    binutils-x86_64
    gcc-core-x86_64
    ;
  inherit (stage0-pe32) writeText;
  kaem = import ./check.nix stage0-pe32.kaem;

  linkScript = writeText "link.sh" ''
    out=$1
    gcc=$2/bin/x86_64-w64-mingw32-gcc.exe
    prefix=$3/bin/x86_64-w64-mingw32-
    pe_header() {
      header=$(( $(od -A n -t u4 -j 60 -N 4 "$1") ))
      machine=$(( $(od -A n -t u2 -j $((header + 4)) -N 2 "$1") ))
      magic=$(( $(od -A n -t u2 -j $((header + 24)) -N 2 "$1") ))
      printf '%s: machine 0x%x, optional header magic 0x%x\n' "$1" "$machine" "$magic"
    }
    cat > read.c <<'EOF'
    #include <windows.h>

    DWORD
    last_error (void)
    {
      return GetLastError ();
    }
    EOF
    cat > answer.c <<'EOF'
    volatile unsigned long long numerator_high = 126, numerator_low = 299;
    volatile unsigned long long divisor_high = 3, divisor_low = 7;

    int
    answer (void)
    {
      unsigned __int128 numerator = (unsigned __int128) numerator_high << 64 | numerator_low;
      unsigned __int128 divisor = (unsigned __int128) divisor_high << 64 | divisor_low;

      return (int) (numerator / divisor);
    }
    EOF
    "$gcc" -O2 -c -o answer.o answer.c
    "''${prefix}ld.exe" --no-insert-timestamp -e answer -o answer.exe answer.o "$("$gcc" -print-libgcc-file-name)"
    status=0
    ./answer.exe || status=$?
    {
      "$gcc" -dumpmachine
      "$gcc" -dumpversion
      "$gcc" -v 2>&1 | tr -d '\r' | grep "^Thread model:"
      "$gcc" -fsyntax-only read.c && echo "read.c: windows.h read"
      "''${prefix}nm.exe" -g -p answer.o | tr -d '\r' | awk '$(NF - 1) == "T" || $(NF - 1) == "U" { print $(NF - 1), $NF }' | sort
      pe_header answer.exe
      echo "answer.exe exits $status"
    } | tr -d '\r' > "$out"
  '';

  # od reads an integer in the machine's byte order, which on Windows is
  # the PE format's little-endian; the signature is the bytes P, E, 0
  # and 0.
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
  link = kaem.runCommand "gcc-core-x86_64-link" { } ''
    ${busybox-w32}/bin/busybox.exe sh ${linkScript} ''${out} ${gcc-core-x86_64} ${binutils-x86_64}
  '';

  stamps = kaem.runCommand "gcc-core-x86_64-stamps" { } ''
    ${busybox-w32}/bin/busybox.exe sh ${stampsScript} ''${out} ${gcc-core-x86_64}
  '';
}
