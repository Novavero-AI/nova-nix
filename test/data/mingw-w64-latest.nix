# pkgs/windows' mingw-w64-headers-latest and mingw-w64-crt-latest, as the
# GCC 10 (Windows) CI jobs build them.  Each check has busybox's sh run a
# script that writes lines to its output, without the carriage returns
# msvcrt's text-mode files end a line with, which CI compares with the
# file of this directory named for it.  A step that fails leaves its lines
# out.
#
# crt pins what the runtime installs, as test/data/gcc46.nix's crt does
# for v8.0.3's: every file under its output, and the global symbols of
# crt2.o, the start file of every program, each with the letter nm gives
# its kind.  configure decides by linking a test whether the objects name
# __ImageBase or __image_base__, and a test that fails on Windows for
# another reason changes the symbols, not the build's exit status.  The
# addresses are left out: they are where GCC 10's code puts each
# function, which no other build of the source shows.  nm sorts with the
# collation of Windows' own locale (see test/data/gcc46.nix), so the
# symbols are listed unsorted and sorted by name with busybox's sort,
# which compares bytes.  The answer, test/data/mingw-w64-latest-crt-
# answer.txt, is derived from other builds of the same release: its files
# are what the runtime's Makefile installs for this configuration, read
# from the install lists of a Makefile configured with these flags, every
# one of which Homebrew's mingw-w64 14.0.0 installs for i686 as well; and
# its symbols are those of the crt2.o in that package, which GCC 16.2.0
# compiled.
#
# hello has gcc10's driver compile hello.c and link it against this
# runtime, given its directories as GCC's build gives a compiler it links
# the target libraries' runtime with (configure.ac:3320-3325 in GCC
# 10.4.0): -B before the runtime's library directory, where the driver
# finds crt2.o and the libraries first, and -isystem before the headers'.
# gcc10 holds v8.0.3's in its own prefix, which the driver reads after
# these.  The program prints the release and the msvcrt.dll version the
# headers name, and calls the three functions whose builtins GCC 4.6.4
# lacks (see pkgs/windows/by-name/mi/mingw-w64-crt-latest), each compiled
# into the runtime's libraries and not imported: gettimeofday, which it
# compares with msvcrt.dll's time; _recalloc, which keeps two values and
# zeroes what it adds, and returns NULL for a size whose product
# overflows; and _byteswap_ushort.  The script writes what the program
# prints and its exit status, the DLLs it imports, which are msvcrt.dll
# and kernel32.dll alone, the three functions in its symbol table, each
# with the underscore i386 Windows puts before a C symbol, and how many
# debugging sections it has, which is none.  The answer,
# test/data/mingw-w64-latest-hello-answer.txt, is derived: the program's
# lines are what MinGW-w64's headers and sources define it to print, and
# the DLLs, symbols and sections follow from the configuration.
let
  inherit (import ../../pkgs/windows)
    stage0-pe32
    busybox-w32
    binutils
    gcc10
    mingw-w64-headers-latest
    mingw-w64-crt-latest
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
        awk '{ print $NF "\t" $(NF - 1) " " $NF }' | sort | cut -f 2-
    } | tr -d '\r' > "$out"
  '';

  helloScript = writeText "hello.sh" ''
    out=$1
    gcc=$2/bin/i686-w64-mingw32-gcc.exe
    headers=$3/include
    crt=$4/lib
    bin=$5/bin
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
      "$bin/i686-w64-mingw32-objdump.exe" -p hello.exe | grep "DLL Name"
      "$bin/i686-w64-mingw32-nm.exe" -p hello.exe | tr -d '\r' | grep -e " T __recalloc$" -e " T _gettimeofday$" -e " T __byteswap_ushort$" | cut -d " " -f 2- | sort
      echo "debugging sections: $("$bin/i686-w64-mingw32-objdump.exe" -h hello.exe | grep -c " \.debug_")"
    } | tr -d '\r' > "$out"
  '';
in
{
  crt = kaem.runCommand "mingw-w64-latest-crt" { } ''
    ${busybox-w32}/bin/busybox.exe sh ${crtScript} ''${out} ${mingw-w64-crt-latest} ${binutils}
  '';

  hello = kaem.runCommand "mingw-w64-latest-hello" { } ''
    ${busybox-w32}/bin/busybox.exe sh ${helloScript} ''${out} ${gcc10} ${mingw-w64-headers-latest} ${mingw-w64-crt-latest} ${binutils}
  '';
}
