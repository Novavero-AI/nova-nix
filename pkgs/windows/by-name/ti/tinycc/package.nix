# TinyCC, as nixpkgs' minimal-bootstrap/tinycc/bootstrappable.nix builds it
# (nixpkgs 60cbef83e025a167470f66fae093dc9f261cb8fc), on the Windows Mes
# rung: janneke's bootstrappable fork compiled by MesCC on mes.exe, as
# tinycc-boot-mes, then by each round's tcc into the next, as boot0 to
# boot3 and tinycc-bootstrappable, each round compiling the Mes C library
# the next links with.  Every derivation is a kaem.runCommand script,
# i686-windows as Mes's are.
#
# tcc targets PE32 from the first round on.  TCC_TARGET_PE compiles tccpe.c
# into tcc, which needs the MesCC fixes the Mes fork carries.  The first
# round's tcc is the only one in the chain linked by MesCC rather than
# written by a tcc, so a tcc that wrote ELF there would leave every round
# after it with nothing Windows runs.
#
# nixpkgs writes tcc's include directories into it as
# CONFIG_TCC_SYSINCLUDEPATHS, a C string.  The builder spells a store path
# C:\nix\store/..., whose backslash starts an escape in that string, so tcc
# is built with none, and every derivation that runs it sets C_INCLUDE_PATH
# from env below, which tcc.c's set_environment reads and splits on ';', a
# PE tcc's PATHSEP.
{
  fetchurl,
  stage0-pe32,
  mes,
}:
let
  inherit (stage0-pe32) kaem;
  inherit (import ./common.nix { inherit kaem mes; }) recompileLibc buildTinyccMes;

  version = "unstable-2024-07-07";
  rev = "ea3900f6d5e71776c5cfabcabee317652e3a19ee";

  tarball = fetchurl {
    url = "https://gitlab.com/janneke/tinycc/-/archive/${rev}/tinycc-${rev}.tar.gz";
    hash = "sha256-16JBGJATAWP+lPylOi3+lojpdv0SR5pqyxOV2PiVx0A=";
  };

  # nixpkgs makes four edits besides these, none of which reaches an i386
  # tcc: three to x86_64-gen.c, and one to tccelf.c that nixpkgs notes is a
  # no-op outside x86_64, the only target with a PLT.
  src =
    (kaem.runCommand "tinycc-bootstrappable-${version}-source" { } ''
      ungz --file ${tarball} --output tinycc.tar
      mkdir -p ''${out}
      cd ''${out}
      untar --file ''${NIX_BUILD_TOP}/tinycc.tar

      # GitLab's archive starts with a pax global header, which untar writes
      # out as a file.
      rm pax_global_header

      cd tinycc-${rev}

      cp ${mes.libc}/lib/libtcc1.c lib/libtcc1.c

      # Static link by default
      replace --file libtcc.c --output libtcc.c --match-on "s->ms_extensions = 1;" --replace-with "s->ms_extensions = 1; s->static_link = 1;"

      # mes-libc depends on max_align_t in stddef.h, which is not provided by tinycc-boot
      replace --file include/stddef.h --output include/stddef.h --match-on "void *alloca" --replace-with "
        typedef union { long double ld; long long ll; } max_align_t;
        void *alloca
      "
    '')
    + "/tinycc-${rev}";

  # The Mes C library has no include link (see mes/libc.nix), so its headers
  # are named in Mes's tree.
  env = {
    C_INCLUDE_PATH = "${src}/include;${mes.srcPrefix}/include";
  };

  # Mes's arena while MesCC compiles and links tcc, in cells.  tcc.c is the
  # largest unit Mes compiles, and Mes keeps more cells across a collection
  # compiling it than mes.env's arena has room to copy into its tenth past
  # the end: 2,957,730, as the TinyCC CI job measures it, against
  # 2,000,000.  This arena's tenth holds 4,000,000, and with Mes's stack it
  # comes to 572 MiB of the 1,024 MiB the Mes port's brk reserves.
  arena = "40000000";
  mesEnv = mes.env // {
    MES_ARENA = arena;
    MES_MAX_ARENA = arena;
  };

  pname = "tinycc-boot-mes";

  # nixpkgs' defines, with TCC_TARGET_PE added and no include directory built
  # in.  Those for linking ELF (CRTPREFIX, ELFINTERP, LIBGCC) go unread by a
  # PE tcc, which links through tccpe.c.  MesCC links at the ImageBase of the
  # Mes port's PE header.
  tinycc-boot-mes = rec {
    compiler = kaem.runCommand "${pname}-${version}" mesEnv ''
      catm config.h
      ${mes.compiler}/bin/mes.exe --no-auto-compile -e main ${mes.srcPost.bin}/bin/mescc.scm -- \
        -S \
        -o tcc.s \
        -I . \
        -D BOOTSTRAP=1 \
        -D HAVE_LONG_LONG=0 \
        -I ${src} \
        -D TCC_TARGET_I386=1 \
        -D TCC_TARGET_PE=1 \
        -D inline= \
        -D CONFIG_TCCDIR=\"\" \
        -D CONFIG_SYSROOT=\"\" \
        -D CONFIG_TCC_CRTPREFIX=\"{B}\" \
        -D CONFIG_TCC_ELFINTERP=\"/mes/loader\" \
        -D CONFIG_TCC_LIBPATHS=\"{B}\" \
        -D CONFIG_TCC_SYSINCLUDEPATHS=\"\" \
        -D TCC_LIBGCC=\"libc.a\" \
        -D TCC_LIBTCC1=\"libtcc1.a\" \
        -D CONFIG_TCC_LIBTCC1_MES=0 \
        -D CONFIG_TCCBOOT=1 \
        -D CONFIG_TCC_STATIC=1 \
        -D CONFIG_USE_LIBGCC=1 \
        -D TCC_MES_LIBC=1 \
        -D TCC_VERSION=\"0.9.28-${version}\" \
        -D ONE_SOURCE=1 \
        ${src}/tcc.c
      mkdir -p ''${out}/bin
      ${mes.compiler}/bin/mes.exe --no-auto-compile -e main ${mes.srcPost.bin}/bin/mescc.scm -- \
        -L ${mes.libs}/lib \
        -l c+tcc \
        --base-address ${mes.baseAddress} \
        -o ''${out}/bin/tcc.exe \
        tcc.s
    '';

    libs = recompileLibc {
      inherit
        pname
        version
        src
        env
        ;
      tcc = compiler;
      libtccOptions = "${mes.libc.CFLAGS} -DTCC_TARGET_I386=1";
    };
  };

  # Each round's options, from janneke's boot.sh as nixpkgs takes them: each
  # hands tcc more of C than the round before.  tinycc-bootstrappable repeats
  # boot3's, so that its floating point is compiled by a tcc that has it.
  tinycc-boot0 = buildTinyccMes {
    pname = "tinycc-boot0";
    inherit src version env;
    prev = tinycc-boot-mes;
    buildOptions = [
      "-D HAVE_LONG_LONG=1"
      "-D HAVE_SETJMP=1"
    ];
    libtccBuildOptions = [
      "-D HAVE_LONG_LONG=1"
    ];
  };

  tinycc-boot1 = buildTinyccMes {
    pname = "tinycc-boot1";
    inherit src version env;
    prev = tinycc-boot0;
    buildOptions = [
      "-D HAVE_BITFIELD=1"
      "-D HAVE_LONG_LONG=1"
      "-D HAVE_SETJMP=1"
    ];
    libtccBuildOptions = [
      "-D HAVE_LONG_LONG=1"
    ];
  };

  tinycc-boot2 = buildTinyccMes {
    pname = "tinycc-boot2";
    inherit src version env;
    prev = tinycc-boot1;
    buildOptions = [
      "-D HAVE_BITFIELD=1"
      "-D HAVE_FLOAT_STUB=1"
      "-D HAVE_LONG_LONG=1"
      "-D HAVE_SETJMP=1"
    ];
    libtccBuildOptions = [
      "-D HAVE_FLOAT_STUB=1"
      "-D HAVE_LONG_LONG=1"
    ];
  };

  tinycc-boot3 = buildTinyccMes {
    pname = "tinycc-boot3";
    inherit src version env;
    prev = tinycc-boot2;
    buildOptions = [
      "-D HAVE_BITFIELD=1"
      "-D HAVE_FLOAT=1"
      "-D HAVE_LONG_LONG=1"
      "-D HAVE_SETJMP=1"
    ];
    libtccBuildOptions = [
      "-D HAVE_FLOAT=1"
      "-D HAVE_LONG_LONG=1"
    ];
  };

  tinycc-bootstrappable = buildTinyccMes {
    pname = "tinycc-bootstrappable";
    inherit src version env;
    prev = tinycc-boot3;
    buildOptions = [
      "-D HAVE_BITFIELD=1"
      "-D HAVE_FLOAT=1"
      "-D HAVE_LONG_LONG=1"
      "-D HAVE_SETJMP=1"
    ];
    libtccBuildOptions = [
      "-D HAVE_FLOAT=1"
      "-D HAVE_LONG_LONG=1"
    ];
  };
in
{
  inherit env;
  boot-mes = tinycc-boot-mes;
  boot0 = tinycc-boot0;
  boot1 = tinycc-boot1;
  boot2 = tinycc-boot2;
  boot3 = tinycc-boot3;
  bootstrappable = tinycc-bootstrappable;
}
