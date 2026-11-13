# MinGW-w64's opendir, readdir and closedir for a program tinycc-native
# compiles: mingw-w64-crt/misc/dirent.c of the v8.0.3 release, as it is, in
# a library a link names, beside the dirent.h it is compiled against.
#
# msvcrt.dll has no directory stream.  MinGW-w64 defines one in this file,
# over msvcrt's _findfirst and _findnext, and archives it in libmingwex.a,
# which its GCC links every program with; tcc links no such library.  GCC's
# cc1 is the program here: libcpp reads a directory of precompiled headers
# through it (libcpp/files.c).
#
# TinyCC's own dirent.h is not the header for this file.  It declares the
# struct dirent of an older MinGW-w64, whose d_name is a pointer into the
# DIR (win32/include/dirent.h:25-33), and this file fills an array of 260
# characters (dirent.c:121 and 197).  Against TinyCC's header it compiles
# without a warning and readdir copies each name through a pointer nothing
# sets.  So the output carries include/dirent.h, MinGW-w64's declarations
# at v8.0.3, and a package that links the library reads that header in
# place of TinyCC's by naming this output in buildInputs.
#
# The source is fetched alone, as busybox-w32 fetches the mingw-w64-crt
# sources it links, from the GitHub mirror of MinGW-w64's repository at the
# commit its v8.0.3 tag names: the same bytes as the file in the release
# archive mingw-w64-headers unpacks.  It is in the public domain.
{
  fetchurl,
  stage0-pe32,
  tinycc-native,
}:
let
  inherit (stage0-pe32) kaem;

  pname = "mingw-w64-dirent";
  version = "8.0.3";

  # The commit the v8.0.3 tag names.
  rev = "05407e416d4f04dc4a252403160193b735a1c6fd";

  src = fetchurl {
    url = "https://raw.githubusercontent.com/mingw-w64/mingw-w64/${rev}/mingw-w64-crt/misc/dirent.c";
    hash = "sha256-baXryk2yqdTEa1pXRJfPffuhtDfXZEZqn6TIKvw9+wo=";
  };

  cc = "${tinycc-native.compiler}/bin/tcc.exe";
  include = ./include;
in
kaem.runCommand "${pname}-${version}" tinycc-native.env ''
  ${cc} -Wall -I ${include} -c -o dirent.o ${src}
  mkdir -p ''${out}/lib
  mkdir -p ''${out}/include
  ${cc} -ar cr ''${out}/lib/libdirent.a dirent.o
  cp ${include}/dirent.h ''${out}/include/dirent.h
''
// {
  srcs = [ src ];
}
