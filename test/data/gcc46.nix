# pkgs/windows' mingw-w64-crt and gcc46, as the Bootstrap (Windows) CI jobs
# build them.  In each, busybox's sh runs a script that writes lines to its
# output, without the carriage returns msvcrt's text-mode files end a line
# with, which CI compares with a file of this directory written from the
# same sources built by GCC 4.6.4 as another compiler built it.  A step
# that fails leaves its lines out.
#
# crt pins what the runtime installs: every file under its output, and the
# global symbols of crt2.o, the start file of every program.  configure
# decides by linking a test whether crt2.o defines __CTOR_LIST__ itself,
# and a test that fails on Windows for another reason changes the symbols,
# not the build's exit status.  nm sorts with the collation of Windows'
# own locale (setlocale (LC_COLLATE, "") and strcoll, binutils/nm.c:1959
# and 888), which msvcrt takes from the system whatever the environment
# says, so the symbols are listed unsorted and sorted by name with
# busybox's sort, which compares bytes.  Compared with
# test/data/gcc46-crt-answer.txt.
#
# hello has gcc46's driver compile hello.c and link it, with no flag naming
# a directory, into a program that runs: a line through msvcrt.dll's puts,
# which GCC makes of a printf with no conversion, a line through its printf
# of a 64-bit division and remainder of values read at run time, which
# libgcc's __divdi3 and __moddi3 compute, and a double printed with %f.
# The script writes what the program prints and its exit status, the DLLs
# it imports, which are msvcrt.dll and kernel32.dll alone, and libgcc's two
# functions in its symbol table.  Compared with
# test/data/gcc46-hello-answer.txt.
let
  inherit (import ../../pkgs/windows)
    stage0-pe32
    busybox-w32
    binutils
    mingw-w64-crt
    gcc46
    ;
  inherit (stage0-pe32) writeText;
  kaem = import ./check.nix stage0-pe32.kaem;

  crtScript = writeText "crt.sh" ''
    out=$1
    crt=$2
    bin=$3/bin
    {
      (cd "$crt" && find . -type f | sort)
      "$bin/i686-w64-mingw32-nm.exe" -g -p "$crt/lib/crt2.o" |
        awk '{ print $NF "\t" $0 }' | sort | cut -f 2-
    } | tr -d '\r' > "$out"
  '';

  helloScript = writeText "hello.sh" ''
    out=$1
    gcc=$2/bin/i686-w64-mingw32-gcc.exe
    bin=$3/bin
    cat > hello.c <<'EOF'
    #include <stdio.h>

    volatile long long numerator = 1234567890123LL;
    volatile long long denominator = -1000;

    int
    main (int argc, char **argv)
    {
      long long n = numerator, d = denominator;

      printf ("hello, world\n");
      printf ("%I64d / %I64d = %I64d remainder %I64d\n", n, d, n / d, n % d);
      printf ("%f\n", argc * 1.25);
      return 0;
    }
    EOF
    "$gcc" -O2 -o hello.exe hello.c
    status=0
    ./hello.exe > run.txt || status=$?
    {
      cat run.txt
      echo "hello.exe exits $status"
      "$bin/i686-w64-mingw32-objdump.exe" -p hello.exe | grep "DLL Name"
      "$bin/i686-w64-mingw32-nm.exe" hello.exe | tr -d '\r' | grep -e " T ___divdi3$" -e " T ___moddi3$" | cut -d " " -f 2-
    } | tr -d '\r' > "$out"
  '';
in
{
  crt = kaem.runCommand "mingw-w64-crt-contents" { } ''
    ${busybox-w32}/bin/busybox.exe sh ${crtScript} ''${out} ${mingw-w64-crt} ${binutils}
  '';

  hello = kaem.runCommand "gcc46-hello" { } ''
    ${busybox-w32}/bin/busybox.exe sh ${helloScript} ''${out} ${gcc46} ${binutils}
  '';
}
