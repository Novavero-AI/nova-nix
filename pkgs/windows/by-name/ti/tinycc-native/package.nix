# TinyCC as a native Windows compiler, past nixpkgs: tinycc-mes's source
# compiled with upstream's win32/build-tcc.bat's defines, _WIN32 defined,
# against TinyCC's own win32/ headers, and linked against msvcrt.dll and
# kernel32.dll through import tables tcc writes from TinyCC's .def files,
# with no Mes in what it builds.  tinycc-native.prev is compiled by
# tinycc-mes and tinycc-native by tinycc-native.prev, so the compiler is
# one a compiler of its own source and configuration wrote; CI checks the
# two are the same bytes.
#
# Where upstream's tcc.exe finds include/ and lib/ beside itself, these
# take the directories a script names, as the rounds before them do: the
# headers in C_INCLUDE_PATH, and the runtime and import libraries with -B,
# since a store path, spelled with a backslash, cannot be a C string (see
# tinycc's package.nix).  A link searches no other directory, and -l<name>
# finds <name>.def there, as upstream's does (see tinycc-mes's
# package.nix).  tcc predefines __MINGW32__, as MinGW's GCC does, since
# Windows sources test it to choose their Windows code; TinyCC defines it
# nowhere, and CONFIG_TCC_SWITCHES, the options tcc applies before its
# command line, is the hook upstream's configure sets from --tcc-switches.
{
  stage0-pe32,
  mes,
  tinycc-mes,
}:
let
  inherit (stage0-pe32) kaem;
  inherit (tinycc-mes) src version;

  # tcc's own headers, then the C library's and the Windows API's, which
  # build-tcc.bat installs as include/ and include/winapi.
  includes = "${src}/include;${src}/win32/include;${src}/win32/include/winapi";
  env = {
    C_INCLUDE_PATH = includes;
  };

  # The objects build-tcc.bat archives as libtcc1.a, in its order; tcc
  # names each for its source, in the build directory.  build-tcc.bat also
  # compiles runmain.o, bcheck.o and the bt-*.o objects, which -run, -b and
  # -bt link and nothing here does, so those options are not available.
  runtimeSources = [
    "lib/libtcc1.c"
    "win32/lib/crt1.c"
    "win32/lib/crt1w.c"
    "win32/lib/wincrt1.c"
    "win32/lib/wincrt1w.c"
    "win32/lib/dllcrt1.c"
    "win32/lib/dllmain.c"
    "win32/lib/chkstk.S"
    "lib/alloca.S"
    "lib/alloca-bt.S"
    "lib/stdatomic.c"
    "lib/atomic.S"
    "lib/builtin.c"
  ];
  runtimeObjects = map (
    source: builtins.replaceStrings [ ".c" ".S" ] [ ".o" ".o" ] (baseNameOf source)
  ) runtimeSources;

  # The import libraries upstream installs beside libtcc1.a.
  importLibraries = [
    "kernel32.def"
    "msvcrt.def"
    "user32.def"
    "gdi32.def"
    "ws2_32.def"
  ];

  # TinyCC's runtime compiled by a round's tcc, the import libraries, and
  # an empty libm.a: what a tcc that links without -nostdlib takes from the
  # -B directory.  -lm is how Unix builds ask for the math functions, which
  # msvcrt.dll holds, and TinyCC finds a library only by a file
  # (libtcc.c's tcc_add_library), so the name needs one that adds nothing,
  # as MinGW-w64's runtime has (mingw-w64-crt/Makefile.am:72-77).  GCC
  # links its genautomata with -lm (gcc/Makefile.in:3948 at 4.6.4).  tcc
  # -ar given no object writes an archive with no member.
  buildRuntime =
    pname: round:
    let
      cc = "${round.compiler}/bin/tcc.exe";
    in
    kaem.runCommand "${pname}-libs-${version}" round.env ''
      ${builtins.concatStringsSep "\n" (map (source: "${cc} -c ${src}/${source}") runtimeSources)}
      mkdir -p ''${out}/lib
      ${cc} -ar cr ''${out}/lib/libtcc1.a ${builtins.concatStringsSep " " runtimeObjects}
      ${cc} -ar cr ''${out}/lib/libm.a
      ${builtins.concatStringsSep "\n" (
        map (def: "cp ${src}/win32/lib/${def} \${out}/lib/${def}") importLibraries
      )}
    '';

  # build-tcc.bat's defines, -DTCC_TARGET_PE -DTCC_TARGET_I386, with the
  # directories left to the script, and none of the Mes rounds': _WIN32
  # stays defined, so tcc.c includes windows.h and runs on msvcrt.  One
  # executable, as build-tcc.bat builds it when TCC_C names tcc.c, where
  # its default links tcc.exe against a libtcc.dll.  Without -g, as
  # build-tcc.bat builds it, since the debug information names the build
  # directory, which differs between the two rounds.  tcc links its
  # runtime, msvcrt and kernel32 from the -B directory.  The script marks
  # tcc.exe executable, since tinycc-native.prev, compiled with _WIN32,
  # marks nothing it writes.
  buildTinyccNative =
    { pname, prev }:
    let
      compiler = kaem.runCommand "${pname}-${version}" prev.env ''
        catm config.h
        mkdir -p ''${out}/bin
        ${prev.compiler}/bin/tcc.exe \
          -B ${prev.libs}/lib \
          -v \
          -o ''${out}/bin/tcc.exe \
          -I . \
          -I ${src} \
          -D TCC_TARGET_I386=1 \
          -D TCC_TARGET_PE=1 \
          -D CONFIG_TCCDIR=\"\" \
          -D CONFIG_TCC_LIBPATHS=\"{B}\" \
          -D CONFIG_TCC_SYSINCLUDEPATHS=\"\" \
          -D CONFIG_TCC_SWITCHES=\"-D__MINGW32__=1\" \
          -D TCC_VERSION=\"0.9.28-${version}\" \
          -D ONE_SOURCE=1 \
          ${src}/tcc.c
        chmod 555 ''${out}/bin/tcc.exe
      '';
    in
    {
      inherit
        prev
        compiler
        env
        ;
      libs = buildRuntime pname { inherit compiler env; };
    };

  # tinycc-mes compiling against the Windows headers.  Its predefined
  # macros, compiled into it, end with an include of Mes's config.h, so
  # Mes's include directory comes last; its typedefs match tcc's own.
  tinycc-mes-win32 = rec {
    inherit (tinycc-mes) compiler;
    env = {
      C_INCLUDE_PATH = "${includes};${mes.srcPrefix}/include";
    };
    libs = buildRuntime "tinycc-mes-win32" { inherit compiler env; };
  };

  tinycc-native-boot = buildTinyccNative {
    pname = "tinycc-native-boot";
    prev = tinycc-mes-win32;
  };
in
buildTinyccNative {
  pname = "tinycc-native";
  prev = tinycc-native-boot;
}
