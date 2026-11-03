# stage0-pe32's chain, from a 1,623-byte PE32 seed to hex0, hex1, hex2, catm,
# M0, cc_x86, M2 (M2-Planet), M1, hex2-new and kaem, each link built by the
# links below it.
#
# Each step, from hex0 down, is one command of stage0-pe32's
# x86/mescc-tools-mini.cmd, in the same order, and its derivation is named for
# the file that command writes, so the two read side by side.  The builder is
# the program the command runs and the args are its arguments: a file it
# reads from artifact\ is the output of the step that wrote it, and the file
# it writes is $out, the step's one output.  A link's attribute is its
# program's name, and an intermediate file's is its name with the dot
# spelled as an underscore.
#
# The links are 32-bit PE, which x64 Windows runs through WoW64, so every step
# is an i686-windows derivation.  Nothing else runs: no cmd.exe, no shell, and
# nothing from the MSYS2 seed.  The set is the seed and the ten links.
{ fetchurl }:
let
  sources = import ./sources.nix { inherit fetchurl; };
  inherit (sources)
    seed
    M2-Planet
    M2libc
    mescc-tools
    ;
  x86 = "${sources.stage0-pe32}/x86";

  out = builtins.placeholder "out";

  stepWith =
    attrs: name: builder: args:
    derivation (
      {
        inherit name args;
        builder = "${builder}";
        system = "i686-windows";
      }
      // attrs
    );
  step = stepWith { };

  # catm writes its first argument: the rest, concatenated in order.
  join = name: parts: step name catm ([ out ] ++ parts);

  # M2-Planet compiling the files in order, which is the order their
  # declarations have to come in.
  compile =
    name: flags: files:
    step name M2 (
      [
        "--architecture"
        "x86"
      ]
      ++ flags
      ++ builtins.concatMap (file: [
        "-f"
        file
      ]) files
      ++ [
        "-o"
        out
      ]
    );

  # Joined ahead of each program from M0 up before hex2 links it: the PE
  # header, and the Windows plumbing that finds ntdll's exports and opens
  # files.
  peHeader = "${x86}/PE32-i386.hex2";
  ntdll = "${x86}/ntdll-i386.hex2";

  # Joined ahead of each compiled program's M1 before M0 assembles it: the
  # instruction definitions, the startup and system calls, and the C library
  # for bootstrap mode or for M2libc's full POSIX layer.
  x86Defs = "${M2libc}/x86/x86_defs.M1";
  libcCore = "${x86}/libc-core.M1";
  libcBootstrap = "${x86}/libc-bootstrap.M1";
  libcFull = "${x86}/libc-full.M1";

  # Defines :PE_end where M2-Planet's output ends with :ELF_end.
  peEndShim = "${x86}/pe-end-shim.M1";

  # Phase-0: hex0 from the seed.  This must reproduce the seed byte for byte,
  # so hex0 is fixed-output to the seed's own hash, and a seed that does not
  # assemble itself fails here rather than under every link above it.
  hex0 =
    stepWith
      {
        outputHashMode = "flat";
        inherit (seed) outputHashAlgo outputHash;
      }
      "hex0.exe"
      seed
      [
        "${x86}/hex0_x86.hex0"
        out
      ];

  # Phase-1: hex1 adds single character labels.
  hex1 = step "hex1.exe" hex0 [
    "${x86}/hex1_x86.hex0"
    out
  ];

  # Phase-2: hex2 adds long labels and sized pointers, and so is a linker.
  hex2 = step "hex2.exe" hex1 [
    "${x86}/hex2_x86.hex1"
    out
  ];

  # Phase-2b: catm joins files, which nothing above could.
  catm = step "catm.exe" hex2 [
    "${x86}/catm_x86.hex2"
    out
  ];

  # Phase-3: M0, the macro assembler.
  M0_hex2 = join "M0.hex2" [
    peHeader
    ntdll
    "${x86}/M0_x86.hex2"
  ];
  M0 = step "M0.exe" hex2 [
    M0_hex2
    out
  ];

  # Phase-4: cc_x86, a compiler for the subset of C that M2-Planet is written
  # in, and the last link written by hand.
  cc_x86-0_hex2 = step "cc_x86-0.hex2" M0 [
    "${x86}/cc_x86.M1"
    out
  ];
  cc_x86_hex2 = join "cc_x86.hex2" [
    peHeader
    ntdll
    cc_x86-0_hex2
  ];
  cc_x86 = step "cc_x86.exe" hex2 [
    cc_x86_hex2
    out
  ];

  # Phase-5: M2-Planet, compiled by cc_x86 from one joined file.
  M2-0_c = join "M2-0.c" [
    "${M2libc}/x86/windows/bootstrap.c"
    "${M2-Planet}/cc.h"
    "${M2libc}/bootstrappable.c"
    "${M2-Planet}/cc_globals.c"
    "${M2-Planet}/cc_reader.c"
    "${M2-Planet}/cc_strings.c"
    "${M2-Planet}/cc_types.c"
    "${M2-Planet}/cc_emit.c"
    "${M2-Planet}/cc_core.c"
    "${M2-Planet}/cc_macro.c"
    "${M2-Planet}/cc.c"
  ];
  M2-0_M1 = step "M2-0.M1" cc_x86 [
    M2-0_c
    out
  ];
  M2-0-0_M1 = join "M2-0-0.M1" [
    x86Defs
    libcCore
    libcBootstrap
    M2-0_M1
  ];
  M2-0_hex2 = step "M2-0.hex2" M0 [
    M2-0-0_M1
    out
  ];
  M2-0-0_hex2 = join "M2-0-0.hex2" [
    peHeader
    ntdll
    M2-0_hex2
  ];
  M2 = step "M2.exe" hex2 [
    M2-0-0_hex2
    out
  ];

  # Phase-6: M1, the fuller assembler, compiled by M2 in bootstrap mode.
  M1-macro_M1 = compile "M1-macro.M1" [ "--bootstrap-mode" ] [
    "${M2libc}/x86/windows/bootstrap.c"
    "${M2libc}/bootstrappable.c"
    "${mescc-tools}/stringify.c"
    "${mescc-tools}/M1-macro.c"
  ];
  M1-macro-0_M1 = join "M1-macro-0.M1" [
    x86Defs
    libcCore
    libcBootstrap
    M1-macro_M1
    peEndShim
  ];
  M1-macro_hex2 = step "M1-macro.hex2" M0 [
    M1-macro-0_M1
    out
  ];
  M1-macro-0_hex2 = join "M1-macro-0.hex2" [
    peHeader
    ntdll
    M1-macro_hex2
  ];
  M1 = step "M1.exe" hex2 [
    M1-macro-0_hex2
    out
  ];

  # Phase-7: hex2 from C, on M2libc's full POSIX layer.
  hex2_linker_M1 =
    compile "hex2_linker.M1"
      [
        "-D"
        "__windows__=1"
      ]
      [
        "${M2libc}/sys/types.h"
        "${M2libc}/stddef.h"
        "${M2libc}/sys/utsname.h"
        "${M2libc}/x86/windows/ntdll.c"
        "${M2libc}/x86/windows/unistd.c"
        "${M2libc}/x86/windows/process.c"
        "${M2libc}/x86/windows/fcntl.c"
        "${M2libc}/fcntl.c"
        "${M2libc}/x86/windows/sys/stat.c"
        "${M2libc}/ctype.c"
        "${M2libc}/stdlib.c"
        "${M2libc}/stdarg.h"
        "${M2libc}/stdio.h"
        "${M2libc}/stdio.c"
        "${M2libc}/bootstrappable.c"
        "${mescc-tools}/hex2.h"
        "${mescc-tools}/hex2_linker.c"
        "${mescc-tools}/hex2_word.c"
        "${mescc-tools}/hex2.c"
      ];
  hex2_linker-0_M1 = join "hex2_linker-0.M1" [
    x86Defs
    libcCore
    libcFull
    hex2_linker_M1
    peEndShim
  ];
  hex2_linker_hex2 = step "hex2_linker.hex2" M0 [
    hex2_linker-0_M1
    out
  ];
  hex2_linker-0_hex2 = join "hex2_linker-0.hex2" [
    peHeader
    ntdll
    hex2_linker_hex2
  ];
  hex2-new = step "hex2-new.exe" hex2 [
    hex2_linker-0_hex2
    out
  ];

  # Phase-8: kaem, the shell the stages after this one are written for.  It
  # compiles like hex2-new, plus string.c.
  kaem_M1 =
    compile "kaem.M1"
      [
        "-D"
        "__windows__=1"
      ]
      [
        "${M2libc}/sys/types.h"
        "${M2libc}/stddef.h"
        "${M2libc}/sys/utsname.h"
        "${M2libc}/x86/windows/ntdll.c"
        "${M2libc}/x86/windows/unistd.c"
        "${M2libc}/x86/windows/process.c"
        "${M2libc}/x86/windows/fcntl.c"
        "${M2libc}/fcntl.c"
        "${M2libc}/x86/windows/sys/stat.c"
        "${M2libc}/ctype.c"
        "${M2libc}/stdlib.c"
        "${M2libc}/string.c"
        "${M2libc}/stdarg.h"
        "${M2libc}/stdio.h"
        "${M2libc}/stdio.c"
        "${M2libc}/bootstrappable.c"
        "${mescc-tools}/Kaem/kaem.h"
        "${mescc-tools}/Kaem/variable.c"
        "${mescc-tools}/Kaem/kaem_globals.c"
        "${mescc-tools}/Kaem/kaem.c"
      ];
  kaem-0_M1 = join "kaem-0.M1" [
    x86Defs
    libcCore
    libcFull
    kaem_M1
    peEndShim
  ];
  kaem_hex2 = step "kaem.hex2" M0 [
    kaem-0_M1
    out
  ];
  kaem-0_hex2 = join "kaem-0.hex2" [
    peHeader
    ntdll
    kaem_hex2
  ];
  kaem = step "kaem.exe" hex2 [
    kaem-0_hex2
    out
  ];
in
{
  inherit
    seed
    hex0
    hex1
    hex2
    catm
    M0
    cc_x86
    M2
    M1
    hex2-new
    kaem
    ;
}
