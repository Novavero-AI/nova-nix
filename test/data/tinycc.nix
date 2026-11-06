# pkgs/windows' TinyCC, as the TinyCC (Windows) CI jobs build it: boot-mes,
# bootstrappable and mes, scripts that run tinycc-boot-mes,
# tinycc-bootstrappable and tinycc-mes with -version, as nixpkgs'
# get-version test does; hello, bootstrappable-hello and mes-hello, which
# each of those tccs compiles and links against the library it compiled,
# and which print their first argument, or TinyCC, and exit with their
# argument count; bootstrappable-float and mes-float, built the same way,
# which print the two words of their argument count times 1.25 as a double;
# mes-printf-float, which prints that double through printf's %f; and
# arena, tinycc-boot-mes's
# own derivation with MES_DEBUG=3, which prints the cells Mes keeps across
# each collection while MesCC compiles and links tcc.
let
  inherit (import ../../pkgs/windows) stage0-pe32 tinycc tinycc-mes;
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

  # The double printed through printf's %f: Mes's dtoab prints the integer
  # part, then the fraction times 1e8 as an integer with its trailing zeros
  # dropped, 1.25, and 2.5 with one argument.  Its cast of the double to
  # long is a call to __fixdfdi, which TinyCC's own libtcc1.c, which
  # tinycc-mes links, has.
  printfFloatC = writeText "printf-float.c" ''
    #include <stdio.h>

    int
    main (int argc, char **argv)
    {
      printf ("%f\n", argc * 1.25);
      return 0;
    }
  '';

  getVersion =
    name: round:
    kaem.runCommand "${name}-get-version" round.env ''
      ${round.compiler}/bin/tcc.exe -version
      mkdir ''${out}
    '';

  # A PE tcc adds no startup file and no C library, and links its runtime,
  # msvcrt and kernel32 unless told -nostdlib (tccpe.c's pe_add_runtime), so
  # each program names crt1.o and the libraries itself.
  program =
    name: round: exe: source:
    kaem.runCommand name round.env ''
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

  mes = getVersion "tinycc-mes" tinycc-mes;
  mes-hello = program "tinycc-mes-hello" tinycc-mes "hello" helloC;
  mes-float = program "tinycc-mes-float" tinycc-mes "float" floatC;
  mes-printf-float = program "tinycc-mes-printf-float" tinycc-mes "printf-float" printfFloatC;

  arena = derivation (
    tinycc.boot-mes.compiler.drvAttrs
    // {
      name = "tinycc-boot-mes-arena";
      MES_DEBUG = "3";
    }
  );
}
