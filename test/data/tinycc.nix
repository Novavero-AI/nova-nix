# pkgs/windows' TinyCC, as the TinyCC (Windows) CI jobs build it: boot-mes,
# a script that runs tinycc-boot-mes with -version, as nixpkgs' get-version
# test does; hello, which that tcc compiles and links against the library it
# compiled, and which prints its first argument, or TinyCC, and exits with
# its argument count; and arena, tinycc-boot-mes's own derivation with
# MES_DEBUG=3, which prints the cells Mes keeps across each collection while
# MesCC compiles and links tcc.
let
  inherit (import ../../pkgs/windows) stage0-pe32 tinycc;
  inherit (stage0-pe32) kaem writeText;
  inherit (tinycc.boot-mes) compiler libs;

  helloC = writeText "hello.c" ''
    #include <stdio.h>

    int
    main (int argc, char **argv)
    {
      printf ("Hello, %s!\n", argc > 1 ? argv[1] : "TinyCC");
      return argc - 1;
    }
  '';
in
{
  boot-mes = kaem.runCommand "tinycc-boot-mes-get-version" tinycc.env ''
    ${compiler}/bin/tcc.exe -version
    mkdir ''${out}
  '';

  # A PE tcc adds no startup file and no C library, and links its runtime,
  # msvcrt and kernel32 unless told -nostdlib (tccpe.c's pe_add_runtime), so
  # hello names crt1.o and the libraries itself.
  hello = kaem.runCommand "tinycc-boot-mes-hello" tinycc.env ''
    mkdir -p ''${out}/bin
    ${compiler}/bin/tcc.exe -nostdlib -o ''${out}/bin/hello.exe ${libs}/lib/crt1.o ${helloC} -L ${libs}/lib -lc -ltcc1
  '';

  arena = derivation (
    compiler.drvAttrs
    // {
      name = "tinycc-boot-mes-arena";
      MES_DEBUG = "3";
    }
  );
}
