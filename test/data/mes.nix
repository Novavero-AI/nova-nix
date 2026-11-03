# pkgs/windows' Mes, as the Mes (Windows) CI jobs build it: mes-m2 and mes,
# each a script that runs it with --version, as nixpkgs' get-version test
# does; hello, which MesCC compiles and links with mes, and which prints its
# first argument, or Mes, and exits with its argument count; and arena,
# MesCC compiling libc+tcc, the largest of Mes's libraries, as the libraries
# are built, with MES_DEBUG=3 printing the cells Mes keeps across each
# collection.
let
  inherit (import ../../pkgs/windows) mes stage0-pe32;
  inherit (stage0-pe32) kaem writeText;

  getVersion =
    name: program:
    kaem.runCommand "${name}-get-version" mes.env ''
      ${program} --version
      mkdir ''${out}
    '';

  helloC = writeText "hello.c" ''
    #include <stdio.h>

    int
    main (int argc, char **argv)
    {
      printf ("Hello, %s!\n", argc > 1 ? argv[1] : "Mes");
      return argc - 1;
    }
  '';

  libcTcc = (builtins.fromJSON (builtins.readFile ../../pkgs/windows/by-name/me/mes/sources.json))."x86.windows.mescc".libc_tcc_SOURCES;
in
{
  mes-m2 = getVersion "mes-m2" "${mes.srcPost.bin}/bin/mes-m2.exe";
  mes = getVersion "mes" "${mes.compiler}/bin/mes.exe";

  # MesCC writes its intermediate files beside its input, so it compiles a
  # copy in the build directory rather than the file in the store.  It links
  # libc and libmescc unless told otherwise, and printf is in libc+tcc, which
  # nixpkgs' tinycc-bootstrappable links tcc with in the same way.
  hello = kaem.runCommand "mes-hello" mes.env ''
    cp ${helloC} hello.c
    mkdir -p ''${out}/bin
    ${mes.compiler}/bin/mes.exe -e main ${mes.srcPost.bin}/bin/mescc.scm -- \
      -D HAVE_CONFIG_H=1 \
      -I ${mes.srcPrefix}/include \
      -I ${mes.srcPrefix}/include/windows/x86 \
      -L ${mes.libs}/lib \
      -l c+tcc \
      --base-address 0x400000 \
      -o ''${out}/bin/hello.exe \
      hello.c
  '';

  arena = kaem.runCommand "mes-arena" (mes.env // { MES_DEBUG = "3"; }) ''
    cd ${mes.srcPrefix}
    catm ''${NIX_BUILD_TOP}/libc+tcc.c ${libcTcc}
    ${mes.srcPost.bin}/bin/mes-m2.exe -e main ${mes.srcPost.bin}/bin/mescc.scm \
      -D HAVE_CONFIG_H=1 \
      -I ${mes.srcPrefix}/include \
      -I ${mes.srcPrefix}/include/windows/x86 \
      -S -o ''${out} ''${NIX_BUILD_TOP}/libc+tcc.c
  '';
}
