# pkgs/windows' mingw-w64-crt-x86_64, as the x86_64 (Windows) CI jobs
# build it.  Each check has busybox's sh run a script that writes lines to
# its output, without the carriage returns msvcrt's text-mode files end a
# line with, which CI compares with the file of this directory named for
# it.  A step that fails leaves its lines out.
#
# crt pins what the runtime installs, as test/data/mingw-w64-latest.nix's
# crt does for i686: every file under its output, and the global symbols
# of crt2.o, each with the letter nm gives its kind, sorted by name with
# busybox's sort.  The answer, test/data/mingw-w64-x86_64-crt-answer.txt,
# is derived from another build of the same release: its files are the
# ones Homebrew's mingw-w64 14.0.0 installs for x86_64 from the runtime,
# and its symbols those of the crt2.o in that package, which GCC 16.2.0
# compiled.
#
# hello has gcc-core-x86_64's driver compile hello.c and link it against
# this runtime, given its directories as test/data/mingw-w64-latest.nix's
# hello gives gcc10 the i686 runtime's, and run it.  The program is that
# check's, for x86_64: it prints the release and the msvcrt.dll version the
# headers name, and calls gettimeofday, _recalloc and _byteswap_ushort,
# each compiled into the runtime's libraries.  The script writes what the
# program prints and its exit status, the DLLs it imports, which are
# msvcrt.dll and kernel32.dll alone, the three functions in its symbol
# table, each with no underscore before it, how many debugging sections it
# has, which is none, and the machine and optional header magic of its PE
# header, which od reads from the file and not binutils.  The answer,
# test/data/mingw-w64-x86_64-hello-answer.txt, is derived: the program's
# lines are what MinGW-w64's headers and sources define it to print, the
# DLLs, symbols and sections follow from the configuration, and 0x8664
# and 0x20b are the PE format's IMAGE_FILE_MACHINE_AMD64 and PE32+.
let
  inherit (import ../../pkgs/windows)
    stage0-pe32
    busybox-w32
    binutils-x86_64
    gcc-core-x86_64
    mingw-w64-headers-latest
    mingw-w64-crt-x86_64
    ;
  inherit (stage0-pe32) writeText;
  kaem = import ./check.nix stage0-pe32.kaem;

  crtScript = writeText "crt.sh" ''
    out=$1
    crt=$2
    prefix=$3/bin/x86_64-w64-mingw32-
    {
      (cd "$crt" && find . -type f | sort)
      "''${prefix}nm.exe" -g -p "$crt/lib/crt2.o" |
        awk '{ print $NF "\t" $(NF - 1) " " $NF }' | sort | cut -f 2-
    } | tr -d '\r' > "$out"
  '';

  helloScript = writeText "hello.sh" ''
    out=$1
    gcc=$2/bin/x86_64-w64-mingw32-gcc.exe
    headers=$3/include
    crt=$4/lib
    prefix=$5/bin/x86_64-w64-mingw32-
    pe_header() {
      header=$(( $(od -A n -t u4 -j 60 -N 4 "$1") ))
      machine=$(( $(od -A n -t u2 -j $((header + 4)) -N 2 "$1") ))
      magic=$(( $(od -A n -t u2 -j $((header + 24)) -N 2 "$1") ))
      printf '%s: machine 0x%x, optional header magic 0x%x\n' "$1" "$machine" "$magic"
    }
    cat > hello.c <<'EOF'
    #include <malloc.h>
    #include <stdio.h>
    #include <stdlib.h>
    #include <sys/time.h>
    #include <time.h>

    volatile unsigned short halfword = 0x1234;
    volatile size_t overflowing = (size_t) -1;

    int
    main (void)
    {
      struct timeval now;
      int got = gettimeofday (&now, NULL);
      time_t then = time (NULL);
      int *numbers = calloc (2, sizeof *numbers);
      int *grown;

      printf ("hello from MinGW-w64 %s for msvcrt.dll 0x%x\n", __MINGW64_VERSION_STR, __MSVCRT_VERSION__);
      printf ("gettimeofday returns %d, within a second of time: %d\n", got, then - now.tv_sec <= 1 && now.tv_sec - then <= 1);
      numbers[0] = 1;
      numbers[1] = 2;
      grown = _recalloc (numbers, 4, sizeof *grown);
      printf ("_recalloc keeps %d %d and zeroes %d %d\n", grown[0], grown[1], grown[2], grown[3]);
      printf ("_recalloc of an overflowing size returns NULL: %d\n", _recalloc (grown, overflowing, 2) == NULL);
      printf ("0x%x swapped is 0x%x\n", halfword, _byteswap_ushort (halfword));
      free (grown);
      return 0;
    }
    EOF
    "$gcc" -B "$crt/" -isystem "$headers" -O2 -o hello.exe hello.c
    status=0
    ./hello.exe > run.txt || status=$?
    {
      cat run.txt
      echo "hello.exe exits $status"
      "''${prefix}objdump.exe" -p hello.exe | grep "DLL Name"
      "''${prefix}nm.exe" -p hello.exe | tr -d '\r' | grep -e " T _recalloc$" -e " T gettimeofday$" -e " T _byteswap_ushort$" | cut -d " " -f 2- | sort
      echo "debugging sections: $("''${prefix}objdump.exe" -h hello.exe | grep -c " \.debug_")"
      pe_header hello.exe
    } | tr -d '\r' > "$out"
  '';
in
{
  crt = kaem.runCommand "mingw-w64-x86_64-crt" { } ''
    ${busybox-w32}/bin/busybox.exe sh ${crtScript} ''${out} ${mingw-w64-crt-x86_64} ${binutils-x86_64}
  '';

  hello = kaem.runCommand "mingw-w64-x86_64-hello" { } ''
    ${busybox-w32}/bin/busybox.exe sh ${helloScript} ''${out} ${gcc-core-x86_64} ${mingw-w64-headers-latest} ${mingw-w64-crt-x86_64} ${binutils-x86_64}
  '';
}
