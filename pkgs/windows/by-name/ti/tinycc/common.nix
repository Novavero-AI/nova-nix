# What every TinyCC round shares, as nixpkgs' minimal-bootstrap/tinycc/
# common.nix gives it: recompileLibc, the Mes C library and TinyCC's runtime
# compiled by a round's tcc for the round after it to link with, and
# buildTinyccMes, a round's tcc compiled by the round before it.
#
# nixpkgs builds each tcc's include directories into it.  Here a tcc reads
# them from C_INCLUDE_PATH (see package.nix), so each round carries env,
# what a script that runs its tcc sets, and a script sets the env of the tcc
# it runs: the round before's for the compiler, the round's own for its
# library.  The two differ when a round is built from another source than
# the round before it, as tinycc-mes-boot is.
#
# lib/windows has crt1.c alone of the startup files (Linux's crti.c and
# crtn.c are empty), so there is no crti.o or crtn.o.  mescc-tools-extra's
# cp writes the file it is given and takes no directory on Windows, so each
# copy names its file.
{ kaem, mes }:
let
  inherit (mes) libc;
in
rec {
  recompileLibc =
    {
      tcc,
      pname,
      version,
      src,
      env,
      libtccOptions,
      libtccSources ? [
        "${src}/lib/libtcc1.c"
        "${src}/lib/va_list.c"
      ],
      libtccObjects ? [
        "libtcc1.o"
        "va_list.o"
      ],
    }:
    let
      cc = "${tcc}/bin/tcc.exe";

      crt = kaem.runCommand "crt" env ''
        mkdir -p ''${out}/lib
        ${cc} ${libc.CFLAGS} -c -o ''${out}/lib/crt1.o ${libc}/lib/crt1.c
      '';

      # tcc names each object for its source, in the build directory.
      library =
        libName: options: sources: objs:
        kaem.runCommand "${libName}.a" env ''
          ${builtins.concatStringsSep "\n" (map (source: "${cc} ${options} -c ${source}") sources)}
          ${cc} -ar cr ''${out} ${builtins.concatStringsSep " " objs}
        '';

      libtcc1 = library "libtcc1" libtccOptions libtccSources libtccObjects;
      libcArchive = library "libc" libc.CFLAGS [ "${libc}/lib/libc.c" ] [ "libc.o" ];
      libgetopt = library "libgetopt" libc.CFLAGS [ "${libc}/lib/libgetopt.c" ] [ "libgetopt.o" ];
    in
    kaem.runCommand "${pname}-libs-${version}" { } ''
      mkdir -p ''${out}/lib
      cp ${crt}/lib/crt1.o ''${out}/lib/crt1.o
      cp ${libtcc1} ''${out}/lib/libtcc1.a
      cp ${libcArchive} ''${out}/lib/libc.a
      cp ${libgetopt} ''${out}/lib/libgetopt.a
    '';

  # nixpkgs' round, with tinycc-boot-mes's changes for PE and four of its
  # own.
  #
  # A PE tcc predefines _WIN32, which tcc.c reads as a host with the Win32
  # API and answers by including windows.h.  tcc runs on Mes libc here, as
  # tinycc-boot-mes, which MesCC compiled without _WIN32, does, so _WIN32 is
  # undefined, and __windows__ tells Mes's fcntl.h the system in its place.
  #
  # A PE tcc adds no startup file or C library when it links, and links
  # msvcrt and kernel32 unless told -nostdlib (tccpe.c's pe_add_runtime), so
  # the round names crt1.o, libc and libtcc1 from the round before it.  Its
  # PE writer marks nothing executable, so the script does.
  #
  # The round compiles without nixpkgs' -g.  tcc's debugging information
  # names the directory tcc ran in and each file by the path it opened, a
  # source or header from the store by the store's physical path, so a
  # tcc.exe that carried it would differ with the store's drive and with
  # the user whose temporary directory the build ran in.  nixpkgs' builds see one store path and,
  # sandboxed, one build directory.
  buildTinyccMes =
    {
      pname,
      version,
      src,
      env,
      prev,
      buildOptions,
      libtccSources ? [
        "${src}/lib/libtcc1.c"
        "${src}/lib/va_list.c"
      ],
      libtccObjects ? [
        "libtcc1.o"
        "va_list.o"
      ],
      libtccBuildOptions,
    }:
    let
      options = builtins.concatStringsSep " " buildOptions;
      libtccOptions = builtins.concatStringsSep " " (
        [
          "-c"
          "-D"
          "TCC_TARGET_I386=1"
        ]
        ++ libtccBuildOptions
      );
      compiler = kaem.runCommand "${pname}-${version}" prev.env ''
        catm config.h
        mkdir -p ''${out}/bin
        ${prev.compiler}/bin/tcc.exe \
          -B ${prev.libs}/lib \
          -v \
          -o ''${out}/bin/tcc.exe \
          -U _WIN32 \
          -D __windows__=1 \
          -D BOOTSTRAP=1 \
          ${options} \
          -I . \
          -I ${src} \
          -D TCC_TARGET_I386=1 \
          -D TCC_TARGET_PE=1 \
          -D CONFIG_TCCDIR=\"\" \
          -D CONFIG_SYSROOT=\"\" \
          -D CONFIG_TCC_CRTPREFIX=\"{B}\" \
          -D CONFIG_TCC_ELFINTERP=\"\" \
          -D CONFIG_TCC_LIBPATHS=\"{B}\" \
          -D CONFIG_TCC_SYSINCLUDEPATHS=\"\" \
          -D TCC_LIBGCC=\"libc.a\" \
          -D TCC_LIBTCC1=\"libtcc1.a\" \
          -D CONFIG_TCCBOOT=1 \
          -D CONFIG_TCC_STATIC=1 \
          -D CONFIG_USE_LIBGCC=1 \
          -D TCC_MES_LIBC=1 \
          -D TCC_VERSION=\"0.9.28-${version}\" \
          -D ONE_SOURCE=1 \
          -nostdlib \
          ${prev.libs}/lib/crt1.o \
          ${src}/tcc.c \
          -lc \
          -ltcc1
        chmod 555 ''${out}/bin/tcc.exe
      '';
      libs = recompileLibc {
        inherit
          pname
          version
          src
          env
          libtccOptions
          libtccSources
          libtccObjects
          ;
        tcc = compiler;
      };
    in
    {
      inherit
        prev
        compiler
        libs
        env
        ;
    };
}
