# The Mes C library as sources for TinyCC, as nixpkgs'
# minimal-bootstrap/mes/libc.nix bundles them: libc.c, libtcc1.c and
# libgetopt.c from the lists configure-lib.sh makes for gcc, whose files
# TinyCC compiles too, with the startup, crt1.c.
#
# Three things of nixpkgs' are not here.  lib/windows has crt1.c alone, and
# Linux's crti.c and crtn.c are empty.  The fork's list carries ldexpl.c,
# which nixpkgs vendors.  And nixpkgs links include to the tree's own with
# its ln-boot, which needs a symlink the Windows library does not have; the
# headers stay in mes.srcPrefix's include.
{ kaem, mes }:
let
  sourcesJson = builtins.fromJSON (builtins.readFile ./sources.json);

  pname = "mes-libc";
  inherit (mes.compiler) version;

  arch = "x86";

  sources = sourcesJson."${arch}.windows.gcc";
  inherit (sources) libtcc1_SOURCES libc_gnu1_SOURCES libc_gnu2_SOURCES;

  # In two calls to catm, as nixpkgs splits them: a stage0-pe32 program's
  # argv holds 256 entries (its x86/libc-core.M1), and the two lists are 269
  # files.
  firstLibc = libc_gnu1_SOURCES;
  lastLibc = libc_gnu2_SOURCES;
in
kaem.runCommand "${pname}-${version}" { inherit pname version; } ''
  cd ${mes.srcPrefix}

  # mescc compiled libc.a
  mkdir -p ''${out}/lib/${arch}-mes

  # libc.c
  catm ''${TMPDIR}/first.c ${firstLibc}
  catm ''${out}/lib/libc.c ''${TMPDIR}/first.c ${lastLibc}

  # crt1.c
  cp lib/windows/${arch}-mes-gcc/crt1.c ''${out}/lib/crt1.c

  # libtcc1.c
  catm ''${out}/lib/libtcc1.c ${libtcc1_SOURCES}

  # getopt.c
  cp lib/posix/getopt.c ''${out}/lib/libgetopt.c
''
