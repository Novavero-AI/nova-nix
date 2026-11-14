# pkgs/windows' gcc-core, as the Bootstrap (Windows) CI jobs build it:
# version, a script that runs the driver with --version, as nixpkgs'
# get-version test does; and answer, in which busybox's sh has the compiler
# do three things.  It compiles text.c to assembly: constants cc1 has to
# fold, in its 64-bit integer arithmetic, in its floating point and through
# MPFR, one more taken from MinGW-w64's limits.h, and functions with a
# loop, a switch, bit-fields, a 64-bit multiplication and doubles.  It
# reads windows.h, and with it MinGW-w64's central headers, for read.c.
# And it compiles answer.c to an object, through binutils' as, which ld
# links as a program's entry point and which runs: the function returns a
# sum modulo 60, a division cc1 turns into a multiplication by a constant
# it computes, and the sum makes it 42.  ld links the object alone, there
# being no runtime yet, so no source calls a library function or divides
# 64-bit integers, which would call libgcc.
#
# The script writes the assembly, a line for the headers, what nm reads
# back of the object and the program's exit status to its output, without
# the carriage returns msvcrt's text-mode files end a line with, and CI
# compares that with test/data/gcc-core-answer.txt, the same sources
# compiled by GCC 4.6.4 as another compiler built it.  A step that fails
# leaves its lines out.
let
  inherit (import ../../pkgs/windows)
    stage0-pe32
    busybox-w32
    binutils
    gcc-core
    ;
  inherit (stage0-pe32) writeText;
  kaem = import ./check.nix stage0-pe32.kaem;

  script = writeText "answer.sh" ''
    out=$1
    gcc=$2/bin/i686-w64-mingw32-gcc.exe
    bin=$3/bin
    cat > text.c <<'EOF'
    #include <limits.h>
    #include <stddef.h>

    struct flags
    {
      unsigned int kind : 3;
      unsigned int size : 5;
      int bias : 7;
    };

    const unsigned long long folded
      = (0x123456789abcdefULL * 7ULL + (0x123456789abcdefULL >> 13)) % 1000003ULL;
    const long long quotient = -0x7fffffffffffLL / 12345;
    const double third = 1.0 / 3.0;
    const int root = (int) (__builtin_sqrt (2.0) * 1000000.0);
    const int path_max = PATH_MAX;

    unsigned int
    checksum (const unsigned char *bytes, size_t count)
    {
      unsigned int sum = 0;
      size_t i;

      for (i = 0; i < count; i++)
        sum = sum * 31 + bytes[i];
      return sum;
    }

    int
    classify (int n)
    {
      switch (n)
        {
        case 0: return 7;
        case 1: return 11;
        case 2: return 13;
        case 3: return 17;
        case 4: return 19;
        case 5: return 23;
        case 6: return 29;
        default: return -1;
        }
    }

    long long
    scale (long long a, long long b)
    {
      return a * b + (a >> 3);
    }

    int
    pack (struct flags *f, int n)
    {
      f->kind = n;
      f->size = n >> 3;
      f->bias = -n;
      return f->bias + f->size;
    }

    double
    mean (const double *values, int count)
    {
      double sum = 0.0;
      int i;

      for (i = 0; i < count; i++)
        sum += values[i];
      return count > 0 ? sum / count : 0.0;
    }
    EOF
    cat > read.c <<'EOF'
    #include <windows.h>

    DWORD
    last_error (void)
    {
      return GetLastError ();
    }
    EOF
    cat > answer.c <<'EOF'
    static int table[] = { 3, 1, 4, 1, 5, 9, 2, 6 };
    volatile int rounds = 8;

    int
    answer (void)
    {
      int i, sum = 0;

      for (i = 0; i < rounds; i++)
        sum += table[i] * (i + 1);
      return sum % 60;
    }
    EOF
    "$gcc" -O2 -S -o text.s text.c
    "$gcc" -O2 -c -o answer.o answer.c
    "$bin/i686-w64-mingw32-ld.exe" -e _answer -o answer.exe answer.o
    status=0
    ./answer.exe || status=$?
    {
      cat text.s
      "$gcc" -fsyntax-only read.c && echo "read.c: windows.h read"
      "$bin/i686-w64-mingw32-nm.exe" -g answer.o
      echo "answer.exe exits $status"
    } | tr -d '\r' > "$out"
  '';
in
{
  version = kaem.runCommand "gcc-core-get-version" { } ''
    ${gcc-core}/bin/i686-w64-mingw32-gcc.exe --version
    mkdir ''${out}
  '';

  answer = kaem.runCommand "gcc-core-answer" { } ''
    ${busybox-w32}/bin/busybox.exe sh ${script} ''${out} ${gcc-core} ${binutils}
  '';
}
