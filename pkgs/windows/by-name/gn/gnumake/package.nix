# GNU make, as nixpkgs' minimal-bootstrap/gnumake builds it with tinycc
# (nixpkgs 60cbef83e025a167470f66fae093dc9f261cb8fc), for Windows: the
# sources and flags of make's own build_w32.bat in its tcc mode, compiled by
# tinycc-native in one kaem script, with no configure and no shell.
#
# build_w32.bat compiles against TinyCC's own headers, with -D_MSC_VER for
# the compiler make's Windows config (src/config.h.W32) then assumes.  That
# config reads __MINGW32__ as MinGW's headers being present, which provide
# sys/param.h, strings.h and unistd.h where TinyCC's do not, so make
# compiles with tinycc-native's predefined __MINGW32__ undefined.
#
# nixpkgs' second patch takes /usr/gnu/include, /usr/local/include and
# /usr/include out of the directories make searches for an included
# makefile, and /lib and /usr/lib out of those it searches for -l<name>.
# On Windows those resolve against the current drive's root, which no
# derivation names, so each is replaced here by an empty comment (kaem drops
# an argument that is empty or a single space).  Its first patch
# concerns the Unix shell and its third Mes's C library; neither applies.
#
# The link takes user32 for wsprintfA and an import library for advapi32,
# whose InitializeSecurityDescriptor make calls before every child process
# and which TinyCC ships no .def for.  tcc writes make.def beside make.exe,
# the import library for the gmk_* functions make exports to the objects
# its load directive loads.
{
  fetchurl,
  stage0-pe32,
  tinycc-native,
}:
let
  inherit (stage0-pe32) kaem;

  pname = "gnumake";
  version = "4.4.1";

  tarball = fetchurl {
    url = "mirror://gnu/make/make-${version}.tar.gz";
    hash = "sha256-3Rb7HWe/q3mnL16DkHNcSePo5wtJRaFasfgd23hlj7M=";
  };

  # TinyCC's import library format, with the one name make takes from
  # advapi32.dll.
  advapi32 = builtins.toFile "advapi32.def" ''
    LIBRARY advapi32.dll

    EXPORTS
    InitializeSecurityDescriptor
  '';

  # build_w32.bat's sources for tcc, in its order.  src/misc.c and
  # src/w32/subproc/misc.c share a name, so each object is named for its
  # whole path.
  sources = [
    "src/ar.c"
    "src/arscan.c"
    "src/commands.c"
    "src/default.c"
    "src/dir.c"
    "src/expand.c"
    "src/file.c"
    "src/function.c"
    "src/getopt.c"
    "src/getopt1.c"
    "src/guile.c"
    "src/hash.c"
    "src/implicit.c"
    "src/job.c"
    "src/load.c"
    "src/loadapi.c"
    "src/main.c"
    "src/misc.c"
    "src/output.c"
    "src/read.c"
    "src/remake.c"
    "src/remote-stub.c"
    "src/rule.c"
    "src/shuffle.c"
    "src/signame.c"
    "src/strcache.c"
    "src/variable.c"
    "src/version.c"
    "src/vpath.c"
    "src/w32/pathstuff.c"
    "src/w32/w32os.c"
    "src/w32/compat/posixfcn.c"
    "src/w32/subproc/misc.c"
    "src/w32/subproc/sub_proc.c"
    "src/w32/subproc/w32err.c"
    "lib/fnmatch.c"
    "lib/glob.c"
    "lib/getloadavg.c"
    "src/w32/compat/dirent.c"
  ];
  objectOf = source: builtins.replaceStrings [ "/" ".c" ] [ "-" ".o" ] source;

  cc = "${tinycc-native.compiler}/bin/tcc.exe";

  # build_w32.bat's tcc flags, less -mthreads, which TinyCC ignores, with
  # its configuration headers placed beside the sources it builds.
  cflags = builtins.concatStringsSep " " [
    "-Wall"
    "-std=c11"
    "-O2"
    "-I src"
    "-I lib"
    "-I src/w32/include"
    "-D_cdecl="
    "-D_MSC_VER"
    "-U__MINGW32__"
    "-DHAVE_CONFIG_H"
  ];
in
kaem.runCommand "${pname}-${version}" tinycc-native.env ''
  ungz --file ${tarball} --output make.tar
  untar --file make.tar
  cd make-${version}

  cp src/config.h.W32 src/config.h
  cp lib/glob.in.h lib/glob.h
  cp lib/fnmatch.in.h lib/fnmatch.h

  replace --file src/read.c --output src/read.c --match-on "\"/usr/gnu/include\"," --replace-with "/**/"
  replace --file src/read.c --output src/read.c --match-on "\"/usr/local/include\"," --replace-with "/**/"
  replace --file src/read.c --output src/read.c --match-on "\"/usr/include\"," --replace-with "/**/"
  replace --file src/remake.c --output src/remake.c --match-on "\"/lib\"," --replace-with "/**/"
  replace --file src/remake.c --output src/remake.c --match-on "\"/usr/lib\"," --replace-with "/**/"

  ${builtins.concatStringsSep "\n" (
    map (source: "${cc} ${cflags} -c -o ${objectOf source} ${source}") sources
  )}

  mkdir -p ''${out}/bin
  ${cc} -B ${tinycc-native.libs}/lib -o ''${out}/bin/make.exe ${
    builtins.concatStringsSep " " (map objectOf sources)
  } -luser32 ${advapi32}
  chmod 555 ''${out}/bin/make.exe
''
