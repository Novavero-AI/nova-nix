# What every TinyCC round shares, as nixpkgs' minimal-bootstrap/tinycc/
# common.nix gives it: recompileLibc, the Mes C library and TinyCC's runtime
# compiled by a round's tcc for the round after it to link with.
#
# lib/windows has crt1.c alone of the startup files (Linux's crti.c and
# crtn.c are empty), so there is no crti.o or crtn.o.  mescc-tools-extra's
# cp writes the file it is given and takes no directory on Windows, so each
# copy names its file.
{ kaem, mes }:
let
  inherit (mes) libc;
in
{
  recompileLibc =
    {
      tcc,
      pname,
      version,
      src,
      env,
      libtccOptions,
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

      libtcc1 =
        library "libtcc1" libtccOptions
          [
            "${src}/lib/libtcc1.c"
            "${src}/lib/va_list.c"
          ]
          [
            "libtcc1.o"
            "va_list.o"
          ];
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
}
