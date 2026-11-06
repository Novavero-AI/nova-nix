# pkgs/windows' TinyCC, as the TinyCC (Windows) CI jobs build it: boot-mes
# and bootstrappable, scripts that run tinycc-boot-mes and
# tinycc-bootstrappable with -version, as nixpkgs' get-version test does;
# hello and bootstrappable-hello, which each of those tccs compiles and
# links against the library it compiled, and which print their first
# argument, or TinyCC, and exit with their argument count;
# bootstrappable-float, which tinycc-bootstrappable builds the same way, and
# which prints the two words of its argument count times 1.25 as a double;
# and arena, tinycc-boot-mes's
# own derivation with MES_DEBUG=3, which prints the cells Mes keeps across
# each collection while MesCC compiles and links tcc.
let
  inherit (import ../../pkgs/windows) stage0-pe32 tinycc;
  inherit (stage0-pe32) kaem writeText;

  helloC = writeText "hello.c" ''
    #include <stdio.h>

    int
    main (int argc, char **argv)
    {
      printf ("Hello, %s!\n", argc > 1 ? argv[1] : "TinyCC");
      return argc - 1;
    }
  '';

  # A double computed at run time, printed as the two words of its IEEE 754
  # encoding: 3ff40000 0 for 1.25, and 40040000 0 for 2.5 with one
  # argument, so it fails if the tcc reads the literal, converts the count
  # or multiplies wrongly.  printf's %f is no check at these rounds: Mes's
  # dtoab casts the double to long, which this tcc compiles as a call to
  # __fixdfdi, and the libtcc1.c the rounds link, Mes's own as nixpkgs
  # copies it in, stubs __fixdfdi to 0.  TinyCC's own libtcc1.c has the
  # conversion.
  floatC = writeText "float.c" ''
    #include <stdio.h>

    int
    main (int argc, char **argv)
    {
      union
      {
        double d;
        unsigned w[2];
      } u;
      u.d = argc * 1.25;
      printf ("%x %x\n", u.w[1], u.w[0]);
      return 0;
    }
  '';

  getVersion =
    name: round:
    kaem.runCommand "${name}-get-version" tinycc.env ''
      ${round.compiler}/bin/tcc.exe -version
      mkdir ''${out}
    '';

  # A PE tcc adds no startup file and no C library, and links its runtime,
  # msvcrt and kernel32 unless told -nostdlib (tccpe.c's pe_add_runtime), so
  # each program names crt1.o and the libraries itself.
  program =
    name: round: exe: source:
    kaem.runCommand name tinycc.env ''
      mkdir -p ''${out}/bin
      ${round.compiler}/bin/tcc.exe -nostdlib -o ''${out}/bin/${exe}.exe ${round.libs}/lib/crt1.o ${source} -L ${round.libs}/lib -lc -ltcc1
    '';
in
{
  boot-mes = getVersion "tinycc-boot-mes" tinycc.boot-mes;
  hello = program "tinycc-boot-mes-hello" tinycc.boot-mes "hello" helloC;

  bootstrappable = getVersion "tinycc-bootstrappable" tinycc.bootstrappable;
  bootstrappable-hello = program "tinycc-bootstrappable-hello" tinycc.bootstrappable "hello" helloC;
  bootstrappable-float = program "tinycc-bootstrappable-float" tinycc.bootstrappable "float" floatC;

  arena = derivation (
    tinycc.boot-mes.compiler.drvAttrs
    // {
      name = "tinycc-boot-mes-arena";
      MES_DEBUG = "3";
    }
  );
}
