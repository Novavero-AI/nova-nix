# What the stage-2 stdenv gives a build's compilers and linker, as a build
# sees it.  test/data/stage2-stdenv.nix's flags runs this in its build
# phase, in the source directory with the stdenv's PATH, and installs the
# lines it writes to flags.txt, which CI compares with
# test/data/stage2-stdenv-flags-answer.txt.  A step that fails leaves its
# line out.
#
# gcc, cc, g++ and c++, run by name as a package's build runs them, each
# compile and link std.c, which prints the standard it was compiled as,
# and gcc does so with a package's own -std=c99 too and from a make
# recipe; ld links answer.c's object; and the line of each program has the
# time stamp in its PE header.  gcc -v, which links nothing, exits 0.
# Then, for each variable of the environment that names directories GCC
# searches (pkgs/windows/stage2-stdenv/cc-wrapper.sh), it writes whether
# gcc and g++, with the variable naming a directory, print that directory
# with -v -E or -print-search-dirs, and whether gcc-native-x86_64's own
# drivers do, which $compilers names, so that the check is known to find
# a directory where one is searched.

# line TEXT: write TEXT to flags.txt.
line() {
  printf '%s\n' "$1" >> flags.txt
}

# stamp PROGRAM: the time stamp in PROGRAM's COFF file header, which
# follows the PE signature, whose offset is at 60, as od reads it in the
# machine's byte order, which on Windows is the PE format's little-endian.
stamp() {
  header=$(( $(od -A n -t u4 -j 60 -N 4 "$1") ))
  echo $(( $(od -A n -t u4 -j $((header + 8)) -N 4 "$1") ))
}

# ran NAME PROGRAM: NAME, what PROGRAM, a build of std.c, prints without
# the carriage return msvcrt's text-mode stdout ends a line with, and
# PROGRAM's time stamp.
ran() {
  printed=$(./"$2" | tr -d '\r') && line "$1: $printed, time stamp $(stamp "$2")"
}

: > flags.txt
gcc -o std-gcc.exe std.c && ran gcc std-gcc.exe
cc -o std-cc.exe std.c && ran cc std-cc.exe
gcc -std=c99 -o std-c99.exe std.c && ran "gcc -std=c99" std-c99.exe
g++ -o std-gxx.exe std.c && ran g++ std-gxx.exe
c++ -o std-cxx.exe std.c && ran c++ std-cxx.exe
make -s -f std.mk && ran "make's gcc" std-make.exe
gcc -c answer.c && ld -e answer -o answer.exe answer.o && line "ld: time stamp $(stamp answer.exe)"
gcc -v 2> /dev/null && line "gcc -v exits 0"

ambient=$PWD/ambient
mkdir -p "$ambient/include"
: > empty.c

# searched VARIABLE CC CXX: whether CC and CXX, with VARIABLE naming
# $ambient, print it with -v -E, CC compiling C and CXX C++, or with
# -print-search-dirs.  COMPILER_PATH's directories are searched for their
# include directory, which is there.
searched() {
  found=$(
    export "$1=$ambient/"
    {
      "$2" -v -E -o empty.i empty.c
      "$3" -v -E -o empty.ii empty.c
      "$2" -print-search-dirs
    } 2>&1 | grep -c ambient
  )
  if [ "$found" = 0 ]; then
    echo "not named"
  else
    echo named
  fi
}

for variable in CPATH C_INCLUDE_PATH CPLUS_INCLUDE_PATH COMPILER_PATH LIBRARY_PATH LPATH GCC_EXEC_PREFIX; do
  line "$variable: $(searched "$variable" gcc g++) by gcc and g++, $(searched "$variable" "$compilers/gcc.exe" "$compilers/g++.exe") by gcc-native-x86_64's gcc.exe and g++.exe"
done
