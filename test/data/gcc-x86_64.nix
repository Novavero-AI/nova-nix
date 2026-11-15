# pkgs/windows' gcc-x86_64, as the GCC 16 x86_64 (Windows) CI jobs build
# it.  Each check has busybox's sh run a script that writes lines to its
# output, without the carriage returns msvcrt's text-mode files end a line
# with, which CI compares with the file of this directory named for it.  A
# step that fails leaves its lines out.
#
# hello-c has the C driver compile hello.c and link it, with no flag
# naming a directory, into a program that runs, and writes the
# configuration gcc -v prints, in brackets, and its thread model.  The
# program is test/data/gcc-latest.nix's, for x86_64: it prints the
# compiler's version and default C standard, the release and msvcrt.dll
# version of the MinGW-w64 headers the compiler reads, C23's constexpr,
# nullptr, bool, typeof and an enumeration with a fixed underlying type,
# the sizes of a pointer, a long, a long long and a long double, which
# x86_64 Windows makes 8, 4, 8 and 16 bytes, a 128-bit division and
# remainder of values read at run time, which libgcc's __divmodti4
# computes, and a product of two unsigned _BitInt(256) values, which
# libgcc's __mulbitint3 computes.
#
# hello-cxx has the C++ driver compile test/data/gcc-latest.nix's hello.cc
# and link it the same way: the compiler, libstdc++, libsupc++ and libgcc's
# unwinder working together through C++20, GCC 16's default, with four
# std::jthread threads under the win32 thread model and two exceptions,
# one unwound through four frames, which on x86_64 unwind through SEH.
#
# Each script writes what its program prints and its exit status, the
# DLLs the program imports, which are msvcrt.dll and kernel32.dll alone
# since libgcc and libstdc++ are static, functions of libgcc's (and for
# C++ libsupc++'s and libstdc++'s) in its symbol table, each with no
# underscore before it, how many CodeView entries its debug directory
# holds, which is none with no build ID, how many debugging sections it
# has, which is none, and the machine and optional header magic of its PE
# header, which od reads from the file and not binutils.  nm sorts by the
# collation of Windows' own locale (see test/data/gcc46.nix), so it lists
# them unsorted and busybox's sort, which compares bytes, sorts them.  The
# answer files are derived, not recorded: the programs' lines are what the
# C and C++ standards, GCC's documented predefined macros, the x86_64
# Windows ABI and MinGW-w64's headers define them to print, the gcc -v
# lines are what gcc.cc prints for the configuration the build phase
# leaves empty and the target's thread model, the DLLs, symbols, entries
# and sections follow from the configuration, and 0x8664 and 0x20b are the
# PE format's IMAGE_FILE_MACHINE_AMD64 and PE32+.
#
# stamps lists every program and DLL gcc-x86_64 installs with the
# TimeDateStamp of its COFF file header, as test/data/gcc-latest.nix's
# stamps does, and CI compares the lines with
# test/data/gcc-x86_64-stamps-answer.txt, which lists the programs the
# build installs, each with 0.
let
  inherit (import ../../pkgs/windows)
    stage0-pe32
    busybox-w32
    binutils-x86_64
    gcc-x86_64
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

  helloCScript = writeText "hello-c.sh" ''
    out=$1
    gcc=$2/bin/x86_64-w64-mingw32-gcc.exe
    prefix=$3/bin/x86_64-w64-mingw32-
    ${peHeader}
    cat > hello.c <<'EOF'
    #include <inttypes.h>
    #include <stdio.h>

    static_assert (sizeof (long long) == 8);

    constexpr int answer = 42;
    enum width : unsigned char { narrow = 200 };

    volatile __int128 numerator = (__int128) 0x7fff << 64 | 0xffffffffffffffff;
    volatile __int128 denominator = -0x10000;
    volatile unsigned _BitInt(256) left = 0xffffffffffffffffffffffffffffffffuwb;
    volatile unsigned _BitInt(256) right = 0x10000uwb;

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
      __int128 n = numerator, d = denominator;
      unsigned _BitInt(256) product = left * right;

      printf ("hello from gcc %d.%d.%d, C %ld\n", __GNUC__, __GNUC_MINOR__, __GNUC_PATCHLEVEL__, __STDC_VERSION__);
      printf ("MinGW-w64 %s for msvcrt.dll 0x%x\n", __MINGW64_VERSION_STR, __MSVCRT_VERSION__);
      printf ("constexpr %d, typeof %d, twice %d, nullptr %d, bool %d, enum %d of size %d\n", answer, copy, twice (copy), nothing == NULL, yes, narrow, (int) sizeof (enum width));
      printf ("pointer %d, long %d, long long %d, long double %d bytes\n", (int) sizeof (void *), (int) sizeof (long), (int) sizeof (long long), (int) sizeof (long double));
      printf ("%" PRId64 " remainder %" PRId64 "\n", (int64_t) (n / d), (int64_t) (n % d));
      printf ("0x%" PRIx64 "%016" PRIx64 "%016" PRIx64 "\n", (uint64_t) (product >> 128), (uint64_t) (product >> 64), (uint64_t) product);
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
      "''${prefix}objdump.exe" -p hello.exe | grep "DLL Name"
      "''${prefix}nm.exe" -p hello.exe | tr -d '\r' | grep -e " T __divmodti4$" -e " T __mulbitint3$" | cut -d " " -f 2- | sort
      echo "CodeView entries: $("''${prefix}objdump.exe" -p hello.exe | grep -c "(format RSDS")"
      echo "debugging sections: $("''${prefix}objdump.exe" -h hello.exe | grep -c " \.debug_")"
      pe_header hello.exe
    } | tr -d '\r' > "$out"
  '';

  helloCxxScript = writeText "hello-cxx.sh" ''
    out=$1
    gxx=$2/bin/x86_64-w64-mingw32-g++.exe
    prefix=$3/bin/x86_64-w64-mingw32-
    ${peHeader}
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
      "''${prefix}objdump.exe" -p hello.exe | grep "DLL Name"
      "''${prefix}nm.exe" -p hello.exe | tr -d '\r' | grep -e " T __cxa_throw$" -e " T __gxx_personality_seh0$" -e " T _Unwind_RaiseException$" -e " T _ZNSt6thread15_M_start_threadESt10unique_ptrINS_6_StateESt14default_deleteIS1_EEPFvvE$" | cut -d " " -f 2- | sort
      echo "CodeView entries: $("''${prefix}objdump.exe" -p hello.exe | grep -c "(format RSDS")"
      echo "debugging sections: $("''${prefix}objdump.exe" -h hello.exe | grep -c " \.debug_")"
      pe_header hello.exe
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
  hello-c = kaem.runCommand "gcc-x86_64-hello-c" { } ''
    ${busybox-w32}/bin/busybox.exe sh ${helloCScript} ''${out} ${gcc-x86_64} ${binutils-x86_64}
  '';

  hello-cxx = kaem.runCommand "gcc-x86_64-hello-cxx" { } ''
    ${busybox-w32}/bin/busybox.exe sh ${helloCxxScript} ''${out} ${gcc-x86_64} ${binutils-x86_64}
  '';

  stamps = kaem.runCommand "gcc-x86_64-stamps" { } ''
    ${busybox-w32}/bin/busybox.exe sh ${stampsScript} ''${out} ${gcc-x86_64}
  '';
}
