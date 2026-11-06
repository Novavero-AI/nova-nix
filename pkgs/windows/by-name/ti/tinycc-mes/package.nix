# TinyCC, as nixpkgs' minimal-bootstrap/tinycc/mes.nix builds it (nixpkgs
# 60cbef83e025a167470f66fae093dc9f261cb8fc), on the Windows Mes rung:
# tinycc-mes-boot, TinyCC's own source compiled by tinycc-bootstrappable,
# the last of janneke's rounds, and tinycc-mes, the same source compiled by
# tinycc-mes-boot.  Each is a round of tinycc's common.nix, with the changes
# for PE that tinycc's package.nix describes, and every derivation is a
# kaem.runCommand script, i686-windows as the rounds before it are.
#
# tcc's predefined macros are compiled into it from tccdefs_.h, which
# conftest.c, compiled with C2STR and run, writes from include/tccdefs.h
# as C strings; nixpkgs appends an include of Mes's config.h to them.
#
# nixpkgs makes three edits to the source, and two of them are made here.
# The third sets static_link in every TCCState, so that an ELF link writes
# a static executable, with no program interpreter.  A PE tcc reads the
# flag only where it resolves -l<name>, which it then makes skip <name>.def
# and <name>.dll, so tinycc-native's -lkernel32 would miss kernel32.def; a
# PE tcc clears it itself before it adds its default libraries.
#
# Two edits are made here besides.  tcc.h keys PATHSEP, which splits
# C_INCLUDE_PATH, -I and -L, on _WIN32, the host's macro, where janneke's
# keys it on TCC_TARGET_PE, and every round compiles tcc.c with _WIN32
# undefined (see common.nix), so an unedited tcc would split a store path
# at its drive's colon.  And tinycc-native compiles this source with
# _WIN32, and such a tcc adds the Windows system directory to its library
# paths, so msvcrt, kernel32 and the other import libraries it links by
# default are read from the DLLs there, which no derivation names, when
# the directories a script names lack their .def files.  The call that
# adds it is removed, so such a link fails instead; a tcc compiled without
# _WIN32, as these rounds are, never makes it.
{
  fetchurl,
  stage0-pe32,
  mes,
  tinycc,
}:
let
  inherit (stage0-pe32) kaem;
  inherit (import ../tinycc/common.nix { inherit kaem mes; }) buildTinyccMes;

  version = "unstable-2025-12-03";
  rev = "cb41cbfe717e4c00d7bb70035cda5ee5f0ff9341";

  # nixpkgs fetches repo.or.cz's snapshot of this commit.  That server does
  # not send every client the same gzip stream of it (the x64 and ARM64
  # runners got two sha256s for the one URL within an hour), so the archive
  # is TinyCC's GitHub mirror's of the same commit, the same tree, which
  # hashes alike on every download.
  tarball = fetchurl {
    name = "tinycc-${rev}.tar.gz";
    url = "https://github.com/TinyCC/tinycc/archive/${rev}.tar.gz";
    hash = "sha256-c4H5RKqSVc1WDoGSxbAkEkbSyD7qVLjrMXECmS/h4rs=";
  };

  src =
    (kaem.runCommand "tinycc-${version}-source" { } ''
      ungz --file ${tarball} --output tinycc.tar
      mkdir -p ''${out}
      cd ''${out}
      untar --file ''${NIX_BUILD_TOP}/tinycc.tar

      # GitHub's archive starts with a pax global header, which untar writes
      # out as a file.
      rm pax_global_header

      cd tinycc-${rev}

      replace --file i386-asm.c --output i386-asm.c --match-on "switch(size)" --replace-with "if (reg >= 8) { cstr_printf(add_str, \"%%r%d%c\", reg, (size == 1) ? 'b' : ((size == 2) ? 'w' : ((size == 4) ? 'd' : ' '))); return; } switch(size)"

      # If performing ptr + (-1) for example, the offset should be ptrdiff_t and not size_t
      replace --file tccgen.c --output tccgen.c --match-on "vpush_type_size(pointed_type(&vtop[-1].type), &align);" --replace-with "vpush_type_size(pointed_type(&vtop[-1].type), &align); if (!(vtop[-1].type.t & VT_UNSIGNED)) gen_cast_s(VT_PTRDIFF_T);"

      # A PE tcc on Mes libc splits its paths on the Windows separator.
      replace --file tcc.h --output tcc.h --match-on "# define PATHSEP \":\"" --replace-with "# define PATHSEP \";\""

      # tinycc-native searches no library directory a script does not name.
      replace --file libtcc.c --output libtcc.c --match-on "tcc_add_systemdir(s);" --replace-with "/* tcc_add_systemdir(s); */"
    '')
    + "/tinycc-${rev}";

  # What a script that runs one of these tccs sets: see tinycc's package.nix.
  env = {
    C_INCLUDE_PATH = "${src}/include;${mes.srcPrefix}/include";
  };

  config_h = builtins.toFile "config_tccdefs.h" ''
    "#include <mes/config.h>\n"
  '';

  # c2str is built by tinycc-bootstrappable as a program of its round is
  # (see test/data/tinycc.nix), and runs from the build directory, which
  # kaem reaches by its path.
  tccdefs = kaem.runCommand "tccdefs-${version}" tinycc.bootstrappable.env ''
    ${tinycc.bootstrappable.compiler}/bin/tcc.exe \
      -B ${tinycc.bootstrappable.libs}/lib \
      -DC2STR \
      -nostdlib \
      -o ''${NIX_BUILD_TOP}/c2str.exe \
      ${tinycc.bootstrappable.libs}/lib/crt1.o \
      ${src}/conftest.c \
      -lc \
      -ltcc1
    chmod 555 ''${NIX_BUILD_TOP}/c2str.exe
    ''${NIX_BUILD_TOP}/c2str.exe ${src}/include/tccdefs.h tccdefs_.h
    mkdir -p ''${out}
    catm ''${out}/tccdefs_.h tccdefs_.h ${config_h}
  '';

  libtccSources = [
    "${src}/lib/libtcc1.c"
    "${src}/lib/alloca.S"
  ];
  libtccObjects = [
    "libtcc1.o"
    "alloca.o"
  ];

  tinycc-mes-boot = buildTinyccMes {
    pname = "tinycc-mes-boot";
    inherit
      src
      version
      env
      libtccSources
      libtccObjects
      ;
    prev = tinycc.bootstrappable;
    buildOptions = [
      "-D HAVE_BITFIELD=1"
      "-D HAVE_FLOAT=1"
      "-D HAVE_LONG_LONG=1"
      "-D HAVE_SETJMP=1"
      "-D CONFIG_TCC_PREDEFS=1"
      "-I ${tccdefs}"
      "-D CONFIG_TCC_SEMLOCK=0"
    ];
    libtccBuildOptions = [
      "-D HAVE_FLOAT=1"
      "-D HAVE_LONG_LONG=1"
      "-D CONFIG_TCC_PREDEFS=1"
      "-I ${tccdefs}"
      "-D CONFIG_TCC_SEMLOCK=0"
    ];
  };
in
buildTinyccMes {
  pname = "tinycc-mes";
  inherit
    src
    version
    env
    libtccSources
    libtccObjects
    ;
  prev = tinycc-mes-boot;
  buildOptions = [
    "-std=c99"
    "-D HAVE_BITFIELD=1"
    "-D HAVE_FLOAT=1"
    "-D HAVE_LONG_LONG=1"
    "-D HAVE_SETJMP=1"
    "-D CONFIG_TCC_PREDEFS=1"
    "-I ${tccdefs}"
    "-D CONFIG_TCC_SEMLOCK=0"
  ];
  libtccBuildOptions = [
    "-D HAVE_FLOAT=1"
    "-D HAVE_LONG_LONG=1"
    "-D CONFIG_TCC_PREDEFS=1"
    "-I ${tccdefs}"
    "-D CONFIG_TCC_SEMLOCK=0"
  ];
}
// {
  # The source tinycc-native compiles.
  inherit src version;
}
