# stage0-pe32's chain, from a 1,623-byte PE32 seed to hex0, hex1, hex2, catm,
# M0, cc_x86, M2 (M2-Planet), M1, hex2-new and kaem, each link built by the
# links below it, mescc-tools-extra's programs built by those links, and the
# bin directories and kaem.runCommand that the stages after it run on, as
# nixpkgs' stage0-posix gives them to its own.
#
# Each step of the chain, from hex0 down, is one command of stage0-pe32's
# x86/mescc-tools-mini.cmd, in the same order, and its derivation is named for
# the file that command writes, so the two read side by side.  The builder is
# the program the command runs and the args are its arguments: a file it
# reads from artifact\ is the output of the step that wrote it, and the file
# it writes is $out, the step's one output.  A link's attribute is its
# program's name, except kaem's, which is kaem-unwrapped as nixpkgs names it,
# and up to M1 an intermediate file's is its name with the dot spelled as an
# underscore.  mescc-tools-extra's programs take the steps the .cmd gives
# kaem.
#
# The links are 32-bit PE, which x64 Windows runs through WoW64, so every
# derivation that runs one is i686-windows.  Nothing else runs: no cmd.exe,
# no shell but kaem, and nothing from the MSYS2 seed.  The set is the seed,
# the ten links, the bin directories kaem, mescc-tools and mescc-tools-extra,
# each with the programs it holds as attributes, and writeText.
{ fetchurl }:
let
  sources = import ./sources.nix { inherit fetchurl; };
  inherit (sources)
    seed
    M2-Planet
    M2libc
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
    "${sources.mescc-tools}/stringify.c"
    "${sources.mescc-tools}/M1-macro.c"
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

  # From Phase-7 on, a program is C on M2libc's full POSIX layer, and becomes
  # an executable in the same five steps: M2 compiles the C library and the
  # program's sources in one pass, catm puts the instruction definitions, the
  # startup, the full library's init and pe-end-shim.M1 around that, M0
  # assembles it, catm puts the PE header and the ntdll plumbing in front, and
  # hex2 links it.  The files between are named for stem, as the .cmd names
  # them, and the executable for name.
  program =
    {
      name,
      stem ? name,
      libc,
      sources,
    }:
    let
      compiled =
        compile "${stem}.M1"
          [
            "-D"
            "__windows__=1"
          ]
          (libc ++ sources);
      withLibc = join "${stem}-0.M1" [
        x86Defs
        libcCore
        libcFull
        compiled
        peEndShim
      ];
      assembled = step "${stem}.hex2" M0 [
        withLibc
        out
      ];
      withHeader = join "${stem}-0.hex2" [
        peHeader
        ntdll
        assembled
      ];
    in
    step "${name}.exe" hex2 [
      withHeader
      out
    ];

  m2libc = map (file: "${M2libc}/${file}");

  # M2libc on its x86/windows target, in the order M2-Planet needs the
  # declarations.  Without --expand-includes M2-Planet skips every #include
  # line, so every file a program's #include lines reach is named here
  # instead.  Under __M2__, <stdio.h> reaches
  # stdio.c, and through it sys/types.h, stddef.h, stdarg.h, fcntl.h,
  # unistd.h, sys/stat.h, stdlib.h and ctype.h; where those headers name
  # x86/linux/ for __i386__, the Windows target's files stand in, its unistd.c
  # split into ntdll.c, unistd.c and process.c.  That unistd.c calls
  # bootstrappable.c's int2str, so bootstrappable.c is in every list.
  libcBase = m2libc [
    "sys/types.h"
    "stddef.h"
    "sys/utsname.h"
    "x86/windows/ntdll.c"
    "x86/windows/unistd.c"
    "x86/windows/process.c"
    "x86/windows/fcntl.c"
    "fcntl.c"
    "x86/windows/sys/stat.c"
    "ctype.c"
    "stdlib.c"
  ];
  libcStdio = m2libc [
    "stdarg.h"
    "stdio.h"
    "stdio.c"
    "bootstrappable.c"
  ];

  # The list for a program whose sources include the M2libc headers in
  # includes.  Every program here includes <stdio.h>, and of the others they
  # include only <string.h> reaches a file <stdio.h> does not, string.c,
  # which comes after the stdlib.c it calls; <stdint.h> and <sys/wait.h> are
  # empty under __M2__.
  libcFor =
    includes:
    libcBase
    ++ (if builtins.elem "string.h" includes then m2libc [ "string.c" ] else [ ])
    ++ libcStdio;

  # Phase-7: hex2 from C.  The .cmd leaves out the string.c that hex2.h's
  # <string.h> would bring in, and hex2 calls nothing in it.
  hex2-new = program {
    name = "hex2-new";
    stem = "hex2_linker";
    libc = libcBase ++ libcStdio;
    sources = [
      "${sources.mescc-tools}/hex2.h"
      "${sources.mescc-tools}/hex2_linker.c"
      "${sources.mescc-tools}/hex2_word.c"
      "${sources.mescc-tools}/hex2.c"
    ];
  };

  # Phase-8: kaem, the shell the stages after this one are written for.
  kaem-unwrapped = program {
    name = "kaem";
    libc = libcFor [
      "stdio.h"
      "stdlib.h"
      "string.h"
      "unistd.h"
      "sys/wait.h"
      "bootstrappable.h"
    ];
    sources = [
      "${sources.mescc-tools}/Kaem/kaem.h"
      "${sources.mescc-tools}/Kaem/variable.c"
      "${sources.mescc-tools}/Kaem/kaem_globals.c"
      "${sources.mescc-tools}/Kaem/kaem.c"
    ];
  };

  # The programs upstream's mescc-tools-extra.kaem builds, each from its one C
  # file, with the M2libc headers its #include lines name.  Upstream compiles
  # them with M2-Mesoplanet, which has no Windows target.
  extraPrograms =
    builtins.mapAttrs
      (
        name: includes:
        program {
          inherit name;
          libc = libcFor includes;
          sources = [ "${sources.mescc-tools-extra}/${name}.c" ];
        }
      )
      {
        sha256sum = [
          "string.h"
          "stdio.h"
          "stdlib.h"
          "bootstrappable.h"
        ];
        match = [
          "stdio.h"
          "string.h"
          "bootstrappable.h"
        ];
        mkdir = [
          "stdio.h"
          "string.h"
          "sys/stat.h"
          "stdlib.h"
          "bootstrappable.h"
        ];
        untar = [
          "stdio.h"
          "stdlib.h"
          "string.h"
          "sys/stat.h"
          "bootstrappable.h"
        ];
        ungz = [
          "stdio.h"
          "stdlib.h"
          "string.h"
          "bootstrappable.h"
        ];
        unbz2 = [
          "stdio.h"
          "string.h"
          "stdlib.h"
          "unistd.h"
          "fcntl.h"
          "bootstrappable.h"
        ];
        unxz = [
          "stdio.h"
          "string.h"
          "unistd.h"
          "stdint.h"
          "stdlib.h"
          "bootstrappable.h"
        ];
        catm = [
          "stdio.h"
          "stdlib.h"
          "unistd.h"
          "fcntl.h"
        ];
        cp = [
          "stdio.h"
          "stdlib.h"
          "string.h"
          "unistd.h"
          "fcntl.h"
          "bootstrappable.h"
        ];
        chmod = [
          "stdio.h"
          "stdlib.h"
          "string.h"
          "unistd.h"
          "fcntl.h"
          "sys/stat.h"
          "bootstrappable.h"
        ];
        rm = [
          "stdio.h"
          "string.h"
          "unistd.h"
          "stdlib.h"
          "bootstrappable.h"
        ];
        replace = [
          "stdio.h"
          "stdlib.h"
          "string.h"
          "unistd.h"
          "bootstrappable.h"
        ];
        wrap = [
          "fcntl.h"
          "stdio.h"
          "stdlib.h"
          "string.h"
          "sys/stat.h"
          "unistd.h"
          "bootstrappable.h"
        ];
      };

  # kaem's ${var}, which kaem substitutes when the script runs.
  variable = var: "\${${var}}";

  # programs installed as bin/<name>.exe, each under its attribute's name: the
  # layout nixpkgs' stage0-posix gives kaem, mescc-tools and
  # mescc-tools-extra.  kaem lays the directory out with mescc-tools-extra's
  # mkdir, cp and chmod, following each cp with chmod 555 as nixpkgs'
  # kaem/default.nix lays out its own, and names them by store path, since
  # these directories are where PATH would find them.  A script from
  # builtins.toFile may not name a derivation, so each program reaches it as
  # the variable of its name.  cp writes a file that is not executable, and
  # M2libc's Windows chmod sets the stream nova-nix reads the executable bit
  # from, so the NAR records each program as executable, as nixpkgs' does.
  binDirectory =
    name: programs:
    let
      bin = "${variable "out"}/bin";
    in
    derivation (
      programs
      // {
        inherit name;
        inherit (extraPrograms) mkdir cp chmod;
        system = "i686-windows";
        builder = "${kaem-unwrapped}";
        args = [
          "--verbose"
          "--strict"
          "--file"
          (builtins.toFile "${name}-bin.kaem" (
            builtins.concatStringsSep "" (
              [
                ''
                  ${variable "mkdir"} -p ${bin}
                ''
              ]
              ++ map (program: ''
                ${variable "cp"} ${variable program} ${bin}/${program}.exe
                ${variable "chmod"} 555 ${bin}/${program}.exe
              '') (builtins.attrNames programs)
            )
          ))
        ];
      }
    );

  # kaem-unwrapped installed, as nixpkgs' kaem is, with runCommand.
  kaem = binDirectory "kaem" { kaem = kaem-unwrapped; } // {
    inherit runCommand;
  };

  # The link M2 as M2-Planet, the name Mes's kaem scripts call it by, and
  # hex2-new, the linker from C, as hex2.  nixpkgs' mescc-tools has M2 under
  # its own name and installs as M2-Planet the one M2 rebuilds, a step
  # stage0-pe32's .cmd does not take.
  mescc-tools = binDirectory "mescc-tools" {
    M2-Planet = M2;
    inherit M1;
    hex2 = hex2-new;
  };

  mescc-tools-extra = binDirectory "mescc-tools-extra" extraPrograms;

  # PATH for the bin directories of dirs, joined with ';', which kaem built
  # for Windows splits PATH on, since a Windows path holds a colon of its own.
  binPath = dirs: builtins.concatStringsSep ";" (map (dir: "${dir}/bin") dirs);

  # nixpkgs' writeText (minimal-bootstrap/utils.nix): a file holding text.
  # The text reaches kaem through passAsFile, as nixpkgs passes it, and not
  # through the environment, where kaem refuses an entry of 4,096 bytes or
  # more.
  writeText =
    name: text:
    derivation {
      inherit name text;
      system = "i686-windows";
      passAsFile = [ "text" ];
      builder = "${kaem}/bin/kaem.exe";
      args = [
        "--verbose"
        "--strict"
        "--file"
        (builtins.toFile "write-text.kaem" ''
          cp ''${textPath} ''${out}
        '')
      ];
      PATH = binPath [ mescc-tools-extra ];
    };

  # nixpkgs' kaem.runCommand (minimal-bootstrap/stage0-posix/kaem/default.nix):
  # a derivation called name, with env's attributes, that kaem builds by
  # running buildCommand strict, so the first command that fails fails the
  # build.  PATH holds kaem's, mescc-tools' and mescc-tools-extra's bin
  # directories, in each of which kaem tries a bare name as it is and then
  # with .exe.  env reaches the derivation as it stands: nixpkgs' handling of
  # nativeBuildInputs, meta and passthru is not here, since nothing passes
  # them.
  #
  # The builder spells the store C:\nix\store, and kaem reads a backslash in
  # a script as an escape, so a store path written into buildCommand would
  # reach its command as C:, a newline and ixstore.  Each one is spelled from
  # ${NIX_STORE} instead.  kaem substitutes a variable as it stands, so a path
  # in one of env's attributes, or an output named as ${out}, as nixpkgs'
  # scripts name it, needs nothing.
  #
  # mescc-tools-extra takes Windows paths its own way.  cp is given the file
  # it writes, never a directory: it takes a destination that does not begin
  # with '/' as relative to the working directory, so it never finds a C:
  # directory there.  mkdir -p makes the parents a path names before a '/'
  # only, and exits 0 even when it fails, as rm does.
  runCommand =
    name: env: buildCommand:
    derivation (
      {
        inherit name;
        system = "i686-windows";
        builder = "${kaem}/bin/kaem.exe";
        args = [
          "--verbose"
          "--strict"
          "--file"
          (writeText "${name}-builder" (
            builtins.replaceStrings [ "${builtins.storeDir}/" ] [ "${variable "NIX_STORE"}/" ] buildCommand
          ))
        ];
        PATH = binPath [
          kaem
          mescc-tools
          mescc-tools-extra
        ];
      }
      // env
    );
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
    kaem-unwrapped
    kaem
    mescc-tools
    mescc-tools-extra
    writeText
    ;
  inherit (sources) srcs;
}
