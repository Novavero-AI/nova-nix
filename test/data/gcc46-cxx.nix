# pkgs/windows' gcc46-cxx, as the GCC 4.6.4 C++ (Windows) CI jobs build it.
# hello has the C++ driver compile hello.cc and link it, with no flag
# naming a directory, into a program that runs, and busybox's sh writes
# lines to its output, without the carriage returns msvcrt's text-mode
# files end a line with, which CI compares with
# test/data/gcc46-cxx-hello-answer.txt.  A step that fails leaves its
# lines out.
#
# The program has the compiler, libstdc++, libsupc++ and libgcc's unwinder
# work together: iostream output; a global object constructed before main
# and a function-local static constructed on its first call, through
# libsupc++'s guard functions; classes with virtual functions, called
# through a base pointer; a function template, instantiated for int and
# std::string; std::vector; new and delete; and two exceptions, a
# std::runtime_error thrown three calls down and caught as std::exception
# after unwinding has destroyed an object in each of the four frames, and a
# class thrown and caught by reference to its base.  The script writes what
# the program prints and its exit status, the DLLs it imports, which are
# msvcrt.dll and kernel32.dll alone since libgcc and libstdc++ are static,
# and three functions of SJLJ exception handling in its symbol table:
# libsupc++'s __cxa_throw and __gxx_personality_sj0 and libgcc's
# _Unwind_SjLj_RaiseException, each with the underscore i386 Windows puts
# before a C symbol.  nm sorts by the collation of Windows' own locale
# (see test/data/gcc46.nix), so it lists them unsorted and busybox's sort,
# which compares bytes, sorts them.
#
# The answer file is derived, not recorded: the program's lines are what
# the C++ standard defines this program to print, and the DLLs and
# symbols follow from the configuration.
#
# stamps lists every program and DLL gcc46-cxx installs, by its path in
# the output, with the TimeDateStamp of its COFF file header: the four
# bytes eight past the PE signature, whose offset is the four bytes at
# 0x3C (Microsoft's PE format specification, "MS-DOS Stub (Image Only)",
# "Signature (Image Only)" and "COFF File Header (Object and Image)").
# ld and strip each write the time there unless the build asks them not
# to (see pkgs/windows/by-name/gc/gcc46-cxx), so a program that carries
# it makes two builds of gcc46-cxx differ.  A file with no PE signature
# at that offset says so in place of its stamp.  CI compares the lines
# with test/data/gcc46-cxx-stamps-answer.txt, which lists the programs
# the build installs, each with 0.
let
  inherit (import ../../pkgs/windows)
    stage0-pe32
    busybox-w32
    binutils
    gcc46-cxx
    ;
  inherit (stage0-pe32) writeText;
  kaem = import ./check.nix stage0-pe32.kaem;

  helloScript = writeText "hello.sh" ''
    out=$1
    gxx=$2/bin/i686-w64-mingw32-g++.exe
    bin=$3/bin
    cat > hello.cc <<'EOF'
    #include <iostream>
    #include <stdexcept>
    #include <string>
    #include <vector>

    class Shape
    {
    public:
      virtual ~Shape () {}
      virtual const char *name () const = 0;
      virtual int area () const = 0;
    };

    class Rectangle : public Shape
    {
    public:
      Rectangle (int w, int h) : width (w), height (h) {}
      const char *name () const { return "rectangle"; }
      int area () const { return width * height; }

    private:
      int width;
      int height;
    };

    class Square : public Rectangle
    {
    public:
      explicit Square (int side) : Rectangle (side, side) {}
      const char *name () const { return "square"; }
    };

    template <typename T>
    T
    total (const std::vector<T> &values)
    {
      T sum = T ();
      for (typename std::vector<T>::size_type i = 0; i < values.size (); ++i)
        sum += values[i];
      return sum;
    }

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
      std::cout << greeting << std::endl;

      std::vector<Shape *> shapes;
      shapes.push_back (new Rectangle (3, 4));
      shapes.push_back (new Square (5));
      for (std::vector<Shape *>::size_type i = 0; i < shapes.size (); ++i)
        {
          std::cout << shapes[i]->name () << " " << shapes[i]->area () << std::endl;
          delete shapes[i];
        }

      std::vector<int> numbers;
      for (int i = 1; i <= 10; ++i)
        numbers.push_back (i);
      std::vector<std::string> words;
      words.push_back ("tool");
      words.push_back ("chain");
      std::cout << total (numbers) << " " << total (words) << " " << unit () << std::endl;

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
  hello = kaem.runCommand "gcc46-cxx-hello" { } ''
    ${busybox-w32}/bin/busybox.exe sh ${helloScript} ''${out} ${gcc46-cxx} ${binutils}
  '';

  stamps = kaem.runCommand "gcc46-cxx-stamps" { } ''
    ${busybox-w32}/bin/busybox.exe sh ${stampsScript} ''${out} ${gcc46-cxx}
  '';
}
