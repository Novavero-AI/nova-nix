# pkgs/windows' gcc10, as the GCC 10 (Windows) CI jobs build it.  Each
# check has busybox's sh run a script that writes lines to its output,
# without the carriage returns msvcrt's text-mode files end a line with,
# which CI compares with the file of this directory named for it.  A step
# that fails leaves its lines out.
#
# hello-c has the C driver compile hello.c and link it, with no flag
# naming a directory, into a program that runs.  The program prints the
# compiler's version and its default C standard, a 64-bit division and
# remainder of values read at run time, which GCC 10 has libgcc's
# __divmoddi4 compute in one call (ix86_init_libfuncs and
# TARGET_EXPAND_DIVMOD_LIBFUNC in gcc/config/i386/i386.c), and the
# results of the three builtins GCC 4.6.4 lacks that MinGW-w64's
# runtimes after v8.0.3 call: __atomic_load_n, __builtin_mul_overflow
# and __builtin_bswap16 (see pkgs/windows/by-name/mi/mingw-w64-headers).
# _Generic, which 4.6.4's C front end lacks as well, compiles it.
#
# hello-cxx has the C++ driver compile hello.cc and link it the same way.
# The program has the compiler, libstdc++, libsupc++ and libgcc's unwinder
# work together, as test/data/gcc46-cxx.nix's does, through C++14, GCC
# 10's default: the compiler's version and __cplusplus; iostream output;
# a global object constructed before main and a function-local static
# constructed on its first call; classes with virtual functions, held by
# std::unique_ptr from std::make_unique and called through a base pointer;
# a variadic function template, instantiated for int and std::string; a
# lambda that std::sort calls; a constexpr function static_assert
# evaluates; std::map; std::to_string; and two exceptions, a
# std::runtime_error thrown three calls down and caught as std::exception
# after unwinding has destroyed an object in each of the four frames, and
# a class thrown and caught by reference to its base.
#
# Each script writes what its program prints and its exit status, the
# DLLs the program imports, which are msvcrt.dll and kernel32.dll alone
# since libgcc and libstdc++ are static, functions of libgcc's (and for
# C++ libsupc++'s) in its symbol table, each with the underscore i386
# Windows puts before a C symbol, and how many debugging sections it has,
# which is none: nothing it links was compiled with -g but libgcc, whose
# debugging information gcc10's install removes (see
# pkgs/windows/by-name/gc/gcc10).  nm sorts by the collation of Windows'
# own locale (see test/data/gcc46.nix), so it lists them unsorted and
# busybox's sort, which compares bytes, sorts them.  The answer files are
# derived, not recorded: the programs' lines are what the C and C++
# standards and GCC's documented predefined macros define them to print,
# and the DLLs, symbols and sections follow from the configuration.
#
# stamps lists every program and DLL gcc10 installs with the TimeDateStamp
# of its COFF file header, as test/data/gcc46-cxx.nix's stamps does for
# gcc46-cxx, and CI compares the lines with
# test/data/gcc10-stamps-answer.txt, which lists the programs the build
# installs, each with 0.
let
  inherit (import ../../pkgs/windows)
    stage0-pe32
    busybox-w32
    binutils
    gcc10
    ;
  inherit (stage0-pe32) writeText;
  kaem = import ./check.nix stage0-pe32.kaem;

  helloCScript = writeText "hello-c.sh" ''
    out=$1
    gcc=$2/bin/i686-w64-mingw32-gcc.exe
    bin=$3/bin
    cat > hello.c <<'EOF'
    #include <stdio.h>

    _Static_assert (sizeof (long long) == 8, "long long has 64 bits");

    #define KIND(x) _Generic ((x), int: "int", double: "double", default: "other")

    volatile long long numerator = 1234567890123LL;
    volatile long long denominator = -1000;
    volatile unsigned short halfword = 0x1234;
    volatile int factor = 65536;
    int shared = 42;

    int
    main (int argc, char **argv)
    {
      long long n = numerator, d = denominator;
      int product;
      int overflows = __builtin_mul_overflow (factor, factor, &product);

      printf ("hello from gcc %d.%d.%d, C %ld\n", __GNUC__, __GNUC_MINOR__, __GNUC_PATCHLEVEL__, __STDC_VERSION__);
      printf ("%I64d / %I64d = %I64d remainder %I64d\n", n, d, n / d, n % d);
      printf ("0x%x swapped is 0x%x\n", halfword, __builtin_bswap16 (halfword));
      printf ("%d * %d overflows int: %d\n", factor, factor, overflows);
      printf ("loaded %d\n", __atomic_load_n (&shared, __ATOMIC_SEQ_CST));
      printf ("%s %s %f\n", KIND (argc), KIND (argc * 1.25), argc * 1.25);
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
      "$bin/i686-w64-mingw32-nm.exe" -p hello.exe | tr -d '\r' | grep -e " T ___divmoddi4$" | cut -d " " -f 2- | sort
      echo "debugging sections: $("$bin/i686-w64-mingw32-objdump.exe" -h hello.exe | grep -c " \.debug_")"
    } | tr -d '\r' > "$out"
  '';

  helloCxxScript = writeText "hello-cxx.sh" ''
    out=$1
    gxx=$2/bin/i686-w64-mingw32-g++.exe
    bin=$3/bin
    cat > hello.cc <<'EOF'
    #include <algorithm>
    #include <iostream>
    #include <map>
    #include <memory>
    #include <stdexcept>
    #include <string>
    #include <vector>

    class Shape
    {
    public:
      virtual ~Shape () = default;
      virtual const char *name () const = 0;
      virtual int area () const = 0;
    };

    class Rectangle : public Shape
    {
    public:
      Rectangle (int w, int h) : width (w), height (h) {}
      const char *name () const override { return "rectangle"; }
      int area () const override { return width * height; }

    private:
      int width;
      int height;
    };

    class Square final : public Rectangle
    {
    public:
      explicit Square (int side) : Rectangle (side, side) {}
      const char *name () const override { return "square"; }
    };

    template <typename T>
    T
    total (T value)
    {
      return value;
    }

    template <typename T, typename... Rest>
    T
    total (T first, Rest... rest)
    {
      return first + total (rest...);
    }

    constexpr int
    squared (int n)
    {
      return n * n;
    }

    static_assert (squared (12) == 144, "squared is evaluated at compile time");

    struct Frame
    {
      static int live;
      Frame () { ++live; }
      ~Frame () { --live; }
    };

    int Frame::live = 0;

    int
    descend (int depth)
    {
      Frame frame;
      if (depth == 0)
        throw std::runtime_error ("bottom");
      return descend (depth - 1) + 1;
    }

    const std::string &
    unit ()
    {
      static const std::string name ("parts");
      return name;
    }

    static const std::string greeting ("hello from g++");

    int
    main ()
    {
      std::cout << greeting << " " << __GNUC__ << "." << __GNUC_MINOR__ << "." << __GNUC_PATCHLEVEL__
                << ", C++ " << __cplusplus << std::endl;

      std::vector<std::unique_ptr<Shape>> shapes;
      shapes.push_back (std::make_unique<Rectangle> (3, 4));
      shapes.push_back (std::make_unique<Square> (5));
      for (const auto &shape : shapes)
        std::cout << shape->name () << " " << shape->area () << std::endl;

      std::vector<int> numbers {5, 3, 9, 1, 7};
      std::sort (numbers.begin (), numbers.end (), [] (int a, int b) { return a > b; });
      std::string sorted;
      for (int number : numbers)
        sorted += " " + std::to_string (number);
      std::cout << "sorted" << sorted << std::endl;

      std::cout << total (1, 2, 3, 4, 5, 6, 7, 8, 9, 10) << " " << total (std::string ("tool"), std::string ("chain"))
                << " " << unit () << std::endl;

      std::map<std::string, int> counts;
      for (const char *word : {"b", "a", "b", "c", "b"})
        ++counts[word];
      std::string tally;
      for (const auto &entry : counts)
        tally += " " + entry.first + "=" + std::to_string (entry.second);
      std::cout << "counted" << tally << std::endl;

      try
        {
          descend (3);
        }
      catch (const std::exception &error)
        {
          std::cout << "caught " << error.what () << " with " << Frame::live << " frames live" << std::endl;
        }

      try
        {
          throw Square (2);
        }
      catch (const Shape &shape)
        {
          std::cout << "caught a " << shape.name () << " of area " << shape.area () << std::endl;
        }
      return 0;
    }
    EOF
    "$gxx" -O2 -o hello.exe hello.cc
    status=0
    ./hello.exe > run.txt || status=$?
    {
      cat run.txt
      echo "hello.exe exits $status"
      "$bin/i686-w64-mingw32-objdump.exe" -p hello.exe | grep "DLL Name"
      "$bin/i686-w64-mingw32-nm.exe" -p hello.exe | tr -d '\r' | grep -e " T ___cxa_throw$" -e " T ___gxx_personality_sj0$" -e " T __Unwind_SjLj_RaiseException$" | cut -d " " -f 2- | sort
      echo "debugging sections: $("$bin/i686-w64-mingw32-objdump.exe" -h hello.exe | grep -c " \.debug_")"
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
  hello-c = kaem.runCommand "gcc10-hello-c" { } ''
    ${busybox-w32}/bin/busybox.exe sh ${helloCScript} ''${out} ${gcc10} ${binutils}
  '';

  hello-cxx = kaem.runCommand "gcc10-hello-cxx" { } ''
    ${busybox-w32}/bin/busybox.exe sh ${helloCxxScript} ''${out} ${gcc10} ${binutils}
  '';

  stamps = kaem.runCommand "gcc10-stamps" { } ''
    ${busybox-w32}/bin/busybox.exe sh ${stampsScript} ''${out} ${gcc10}
  '';
}
