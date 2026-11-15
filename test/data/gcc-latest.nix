# pkgs/windows' gcc-latest, as the GCC 16 (Windows) CI jobs build it.
# Each check has busybox's sh run a script that writes lines to its
# output, without the carriage returns msvcrt's text-mode files end a line
# with, which CI compares with the file of this directory named for it.  A
# step that fails leaves its lines out.
#
# hello-c has the C driver compile hello.c and link it, with no flag
# naming a directory, into a program that runs, and writes the
# configuration gcc -v prints, in brackets, and its thread model.  The
# program prints the compiler's version and its default C standard, the
# release and msvcrt.dll version of the MinGW-w64 headers the compiler
# reads, C23's constexpr, nullptr, bool and an enumeration with a fixed
# underlying type, which GCC 10 lacks, and its typeof, which GCC 10 has
# as an extension, a 64-bit division and remainder of values read at run
# time, which libgcc's __divmoddi4 computes, and a product of two
# unsigned _BitInt(128) values, which libgcc's __mulbitint3 computes.
#
# hello-cxx has the C++ driver compile hello.cc and link it the same way.
# The program has the compiler, libstdc++, libsupc++ and libgcc's
# unwinder work together through C++20, GCC 16's default: std::format; a
# concept and a function template it constrains; a consteval function; a
# defaulted three-way comparison; classes with virtual functions, held by
# std::unique_ptr and called through a base pointer; a range filtered and
# transformed through views, std::ranges::sort and std::span; four
# std::jthread threads that add to a sum under a std::mutex and signal a
# std::condition_variable, which libstdc++ has for this target only with
# a runtime whose headers name Vista or later; and test/data/gcc10.nix's
# two exceptions, one unwound through four frames.
#
# Each script writes what its program prints and its exit status, the
# DLLs the program imports, which are msvcrt.dll and kernel32.dll alone
# since libgcc and libstdc++ are static, functions of libgcc's (and for
# C++ libsupc++'s and libstdc++'s) in its symbol table, each with the
# underscore i386 Windows puts before a C symbol, how many CodeView
# entries its debug directory holds, which is the one the build ID ld
# writes for --enable-linker-build-id, and how many debugging sections it
# has, which is none.  nm sorts by the collation of Windows' own locale
# (see test/data/gcc46.nix), so it lists them unsorted and busybox's sort,
# which compares bytes, sorts them.  The answer files are derived, not
# recorded: the programs' lines are what the C and C++ standards, GCC's
# documented predefined macros and MinGW-w64's headers define them to
# print, the gcc -v lines are what gcc.cc prints for the configuration
# gcc-latest's build phase leaves empty and the target's thread model,
# and the DLLs, symbols, entries and sections follow from the
# configuration.
#
# stamps lists every program and DLL gcc-latest installs with the
# TimeDateStamp of its COFF file header, as test/data/gcc10.nix's stamps
# does, and CI compares the lines with
# test/data/gcc-latest-stamps-answer.txt, which lists the programs the
# build installs, each with 0.
let
  inherit (import ../../pkgs/windows)
    stage0-pe32
    busybox-w32
    binutils
    gcc-latest
    ;
  inherit (stage0-pe32) writeText;
  kaem = import ./check.nix stage0-pe32.kaem;

  helloCScript = writeText "hello-c.sh" ''
    out=$1
    gcc=$2/bin/i686-w64-mingw32-gcc.exe
    bin=$3/bin
    cat > hello.c <<'EOF'
    #include <inttypes.h>
    #include <stdio.h>

    static_assert (sizeof (long long) == 8);

    constexpr int answer = 42;
    enum width : unsigned char { narrow = 200 };

    volatile long long numerator = 1234567890123LL;
    volatile long long denominator = -1000;
    volatile unsigned _BitInt(128) left = 0xffffffffffffffffuwb;
    volatile unsigned _BitInt(128) right = 0x10000uwb;

    [[nodiscard]] static int
    twice (int n)
    {
      return n + n;
    }

    int
    main (void)
    {
      typeof (answer) copy = answer;
      int *nothing = nullptr;
      bool yes = true;
      long long n = numerator, d = denominator;
      unsigned _BitInt(128) product = left * right;

      printf ("hello from gcc %d.%d.%d, C %ld\n", __GNUC__, __GNUC_MINOR__, __GNUC_PATCHLEVEL__, __STDC_VERSION__);
      printf ("MinGW-w64 %s for msvcrt.dll 0x%x\n", __MINGW64_VERSION_STR, __MSVCRT_VERSION__);
      printf ("constexpr %d, typeof %d, twice %d, nullptr %d, bool %d, enum %d of size %d\n", answer, copy, twice (copy), nothing == NULL, yes, narrow, (int) sizeof (enum width));
      printf ("%" PRId64 " / %" PRId64 " = %" PRId64 " remainder %" PRId64 "\n", (int64_t) n, (int64_t) d, (int64_t) (n / d), (int64_t) (n % d));
      printf ("0x%" PRIx64 "%016" PRIx64 "\n", (uint64_t) (product >> 64), (uint64_t) product);
      return 0;
    }
    EOF
    "$gcc" -O2 -o hello.exe hello.c
    status=0
    ./hello.exe > run.txt || status=$?
    {
      cat run.txt
      echo "hello.exe exits $status"
      echo "configured with: [$("$gcc" -v 2>&1 | tr -d '\r' | sed -n 's/^Configured with: //p')]"
      "$gcc" -v 2>&1 | tr -d '\r' | grep "^Thread model:"
      "$bin/i686-w64-mingw32-objdump.exe" -p hello.exe | grep "DLL Name"
      "$bin/i686-w64-mingw32-nm.exe" -p hello.exe | tr -d '\r' | grep -e " T ___divmoddi4$" -e " T ___mulbitint3$" | cut -d " " -f 2- | sort
      echo "CodeView entries: $("$bin/i686-w64-mingw32-objdump.exe" -p hello.exe | grep -c "(format RSDS")"
      echo "debugging sections: $("$bin/i686-w64-mingw32-objdump.exe" -h hello.exe | grep -c " \.debug_")"
    } | tr -d '\r' > "$out"
  '';

  helloCxxScript = writeText "hello-cxx.sh" ''
    out=$1
    gxx=$2/bin/i686-w64-mingw32-g++.exe
    bin=$3/bin
    cat > hello.cc <<'EOF'
    #include <algorithm>
    #include <compare>
    #include <concepts>
    #include <condition_variable>
    #include <format>
    #include <iostream>
    #include <memory>
    #include <mutex>
    #include <ranges>
    #include <span>
    #include <stdexcept>
    #include <string>
    #include <thread>
    #include <vector>

    template <typename T>
    concept Numeric = std::integral<T> || std::floating_point<T>;

    template <Numeric T>
    constexpr T
    twice (T value)
    {
      return value + value;
    }

    consteval int
    squared (int n)
    {
      return n * n;
    }

    static_assert (squared (12) == 144);

    struct Version
    {
      int major;
      int minor;
      int patch;
      auto operator<=> (const Version &) const = default;
    };

    class Shape
    {
    public:
      virtual ~Shape () = default;
      virtual std::string name () const = 0;
      virtual int area () const = 0;
    };

    class Rectangle : public Shape
    {
    public:
      Rectangle (int w, int h) : width (w), height (h) {}
      std::string name () const override { return "rectangle"; }
      int area () const override { return width * height; }

    private:
      int width;
      int height;
    };

    class Square final : public Rectangle
    {
    public:
      explicit Square (int side) : Rectangle (side, side) {}
      std::string name () const override { return "square"; }
    };

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

    static const std::string greeting ("hello from g++");

    int
    main ()
    {
      std::cout << std::format ("{} {}.{}.{}, C++ {}", greeting, __GNUC__, __GNUC_MINOR__, __GNUC_PATCHLEVEL__, __cplusplus) << std::endl;
      std::cout << std::format ("MinGW-w64 {} for msvcrt.dll {:#x}", __MINGW64_VERSION_STR, __MSVCRT_VERSION__) << std::endl;

      std::vector<std::unique_ptr<Shape>> shapes;
      shapes.push_back (std::make_unique<Rectangle> (3, 4));
      shapes.push_back (std::make_unique<Square> (5));
      for (const auto &shape : shapes)
        std::cout << std::format ("{} {}", shape->name (), shape->area ()) << std::endl;

      std::vector<int> numbers {5, 3, 9, 1, 7, 4, 6};
      std::string squares;
      for (int square : numbers | std::views::filter ([] (int n) { return n % 2 == 0; }) | std::views::transform ([] (int n) { return n * n; }))
        squares += std::format (" {}", square);
      std::ranges::sort (numbers);
      std::string middle;
      for (int number : std::span<const int> (numbers).subspan (2, 3))
        middle += std::format (" {}", number);
      std::cout << "even squares" << squares << ", middle of sorted" << middle << std::endl;

      Version current {16, 2, 0};
      Version previous {10, 4, 0};
      std::cout << std::format ("twice {} and {}, 16.2.0 after 10.4.0: {}", twice (21), twice (1.25), current > previous) << std::endl;

      std::mutex lock;
      std::condition_variable done;
      int finished = 0;
      int sum = 0;
      {
        std::vector<std::jthread> workers;
        for (int i = 1; i <= 4; ++i)
          workers.emplace_back ([&, i] {
            std::lock_guard guard (lock);
            sum += i * 10;
            ++finished;
            done.notify_one ();
          });
        std::unique_lock waiting (lock);
        done.wait (waiting, [&] { return finished == 4; });
      }
      std::cout << std::format ("4 threads added {}", sum) << std::endl;

      try
        {
          descend (3);
        }
      catch (const std::exception &error)
        {
          std::cout << std::format ("caught {} with {} frames live", error.what (), Frame::live) << std::endl;
        }

      try
        {
          throw Square (2);
        }
      catch (const Shape &shape)
        {
          std::cout << std::format ("caught a {} of area {}", shape.name (), shape.area ()) << std::endl;
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
      "$bin/i686-w64-mingw32-nm.exe" -p hello.exe | tr -d '\r' | grep -e " T ___cxa_throw$" -e " T ___gxx_personality_sj0$" -e " T __Unwind_SjLj_RaiseException$" -e " T __ZNSt6thread15_M_start_threadESt10unique_ptrINS_6_StateESt14default_deleteIS1_EEPFvvE$" | cut -d " " -f 2- | sort
      echo "CodeView entries: $("$bin/i686-w64-mingw32-objdump.exe" -p hello.exe | grep -c "(format RSDS")"
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
  hello-c = kaem.runCommand "gcc-latest-hello-c" { } ''
    ${busybox-w32}/bin/busybox.exe sh ${helloCScript} ''${out} ${gcc-latest} ${binutils}
  '';

  hello-cxx = kaem.runCommand "gcc-latest-hello-cxx" { } ''
    ${busybox-w32}/bin/busybox.exe sh ${helloCxxScript} ''${out} ${gcc-latest} ${binutils}
  '';

  stamps = kaem.runCommand "gcc-latest-stamps" { } ''
    ${busybox-w32}/bin/busybox.exe sh ${stampsScript} ''${out} ${gcc-latest}
  '';
}
