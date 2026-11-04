# GNU Mes 0.27.1, as nixpkgs' minimal-bootstrap/mes/default.nix builds it
# (nixpkgs 1d060086d60cb76a40cf37109f979d88a5bd6396), on the Windows stage0
# tools: mes-m2.exe built by kaem.windows.run, the Mes C library compiled by
# MesCC running on mes-m2, and mes.exe compiled by MesCC.  libc is the
# library's sources for TinyCC, which nixpkgs builds from libc.nix as
# mes-libc.  Every derivation is a kaem.runCommand script, i686-windows as
# stage0-pe32's are.
#
# The source is Novavero-AI/mes, GNU Mes 0.27.1 with a Windows port.  It
# carries nixpkgs' i686 C library fixes (strpbrk, ntoab, ioctl3, vfprintf and
# vsnprintf) as commits, so none of nixpkgs' replace calls on the library is
# made here; its others edit lib/linux and x86_64 files the Windows lists do
# not name.
#
# nixpkgs writes each path Mes reads from the environment into Mes's C and
# Scheme source.  The builder spells a store path C:\nix\store/..., whose
# backslash starts an escape in a string of either language, so here every
# derivation that runs Mes sets the variables instead, from env below.
{ fetchurl, stage0-pe32 }:
let
  sourcesJson = builtins.fromJSON (builtins.readFile ./sources.json);
  inherit (stage0-pe32) kaem mescc-tools;

  pname = "mes";
  version = "0.27.1";
  rev = "d9ab4ea10a921d265794580086c15cfe70da19db";

  src = fetchurl {
    name = "mes-${builtins.substring 0 7 rev}.tar.gz";
    url = "https://github.com/Novavero-AI/mes/archive/${rev}.tar.gz";
    sha256 = "392336c0f679c7a54f6da79a2703fedc451362f8f1a2a1880017399760fe00ee";
  };

  nyacc = import ./nyacc.nix { inherit fetchurl kaem; };

  intptr = "int";
  uintptr = "unsigned ${intptr}";

  config_h = builtins.toFile "config.h" ''
    #ifndef _MES_CONFIG_H
    #undef SYSTEM_LIBC
    #define MES_VERSION "${version}"
    #ifndef __M2__
    typedef ${uintptr} uintptr_t;
    typedef ${uintptr} size_t;
    typedef ${intptr} ssize_t;
    typedef ${intptr} intptr_t;
    typedef ${intptr} ptrdiff_t;
    #define __MES_SIZE_T
    #define __MES_SSIZE_T
    #define __MES_INTPTR_T
    #define __MES_UINTPTR_T
    #define __MES_PTRDIFF_T
    #endif
    #endif
  '';

  arch = "x86";

  # The lists build-aux/configure-lib.sh makes for this tree with
  # mes_kernel=windows, as nixpkgs' mes/gen-sources.sh writes them with
  # ARCHS="x86" and KERNELS="windows".
  sources = sourcesJson."${arch}.windows.mescc";

  inherit (sources)
    libc_mini_SOURCES
    libmescc_SOURCES
    libc_SOURCES
    libc_tcc_SOURCES
    mes_SOURCES
    ;

  # Mes's arena and its maximum, in cells, and its stack, which nixpkgs
  # writes into src/gc.c as 100000000 and 6000000.  The arena is allocated
  # with a tenth more, which the collector copies the cells it keeps into,
  # and then the stack, at 12 bytes a cell on i686: nixpkgs' sizes come to
  # 1,328 MiB, past the 1,024 MiB lib/windows/brk.c reserves for every
  # allocation, and these to 320 MiB.  The cells kept across a collection
  # have to fit in that tenth, which Mes does not check: compiling libc+tcc,
  # the largest library, Mes keeps about 1,000,000, as the Mes CI job
  # measures it, against the 2,000,000 there is room for.  The sizes reach
  # Mes through env, as the paths do, so a build can be given others without
  # rebuilding Mes.
  arena = "20000000";
  stack = "6000000";

  # The ImageBase lib/m2/x86/PE32-i386.hex2 declares.  MesCC links at
  # 0x1000000 unless told otherwise, Linux's address.
  baseAddress = "0x400000";

  srcPost =
    kaem.runCommand "${pname}-src-${version}"
      {
        outputs = [
          "out"
          "bin"
        ];
      }
      ''
        # Unpack source
        ungz --file ${src} --output mes.tar
        mkdir ''${out}
        cd ''${out}
        untar --non-strict --file ''${NIX_BUILD_TOP}/mes.tar # ignore symlinks

        # GitHub's archive starts with a pax global header and names one
        # test's directory in the ustar prefix field.  untar reads neither: it
        # writes the header out as a file, and that test beside it, both here
        # rather than in the tree.
        rm pax_global_header 44-switch-body-fallthrough-not-default.c

        MES_PREFIX=''${out}/mes-${rev}

        cd ''${MES_PREFIX}

        cp ${config_h} include/mes/config.h

        mkdir include/arch
        cp include/windows/${arch}/kernel-stat.h include/arch/kernel-stat.h
        cp include/windows/${arch}/signal.h include/arch/signal.h
        cp include/windows/${arch}/syscall.h include/arch/syscall.h

        # Remove pregenerated files
        rm mes/module/mes/psyntax.pp mes/module/mes/psyntax.pp.header

        # These files are symlinked in the repo
        cp mes/module/srfi/srfi-9-struct.mes mes/module/srfi/srfi-9.mes
        cp mes/module/srfi/srfi-9/gnu-struct.mes mes/module/srfi/srfi-9/gnu.mes

        # Create mescc.scm
        mescc_in=scripts/mescc.scm.in
        replace --file ''${mescc_in} --output ''${mescc_in} --match-on @VERSION@ --replace-with ${version}
        replace --file ''${mescc_in} --output ''${mescc_in} --match-on @mes_cpu@ --replace-with ${arch}
        replace --file ''${mescc_in} --output ''${mescc_in} --match-on @mes_kernel@ --replace-with windows
        mkdir -p ''${bin}/bin
        cp ''${mescc_in} ''${bin}/bin/mescc.scm

        # The header and footer MesCC links a program for kernel windows
        # between, from the three files kaem.windows.run links mes-m2 with:
        # the PE header and the code that finds ntdll's exports, which MesCC
        # takes as one file, and pe-end.M1, whose :PE_end the header measures
        # the image to.
        catm lib/windows/${arch}-mes/pe32-header.hex2 lib/m2/${arch}/PE32-i386.hex2 lib/m2/${arch}/ntdll-i386.hex2
        cp lib/windows/${arch}-mes/pe-end.M1 lib/windows/${arch}-mes/pe32-footer-single-main.hex2

        # Build mes-m2
        kaem --verbose --strict --file kaem.windows.run
        cp bin/mes-m2.exe ''${bin}/bin/mes-m2.exe
        chmod 555 ''${bin}/bin/mes-m2.exe
      '';

  srcPrefix = "${srcPost.out}/mes-${rev}";

  # What a derivation that runs Mes sets, for the getenv calls nixpkgs
  # replaces with paths and sizes.  GUILE_LOAD_PATH is split on ':' except
  # between double quotes (search-path-split, mes/module/mes/guile.mes), so
  # each entry is quoted for its drive's colon.  system* looks a bare name up
  # on a PATH split on ':' and without adding .exe, so MesCC is given M1 and
  # hex2 as paths.
  env = {
    MES_ARENA = arena;
    MES_MAX_ARENA = arena;
    MES_STACK = stack;
    MES_PREFIX = srcPrefix;
    GUILE_LOAD_PATH = builtins.concatStringsSep ":" (
      map (dir: "\"${dir}\"") [
        "${srcPrefix}/mes/module"
        "${srcPrefix}/module"
        nyacc.guilePath
      ]
    );
    M1 = "${mescc-tools}/bin/M1.exe";
    HEX2 = "${mescc-tools}/bin/hex2.exe";
  };

  cc = "${srcPost.bin}/bin/mes-m2.exe";
  ccArgs = [
    "-e"
    "main"
    "${srcPost.bin}/bin/mescc.scm"
    "-D"
    "HAVE_CONFIG_H=1"
    "-I"
    "${srcPrefix}/include"
    "-I"
    "${srcPrefix}/include/windows/${arch}"
  ];

  CC = toString ([ cc ] ++ ccArgs);

  # nixpkgs compiles lib/linux/x86-mes-mescc/crt1.c.  Windows hands a program
  # one UTF-16 command line rather than argc, argv and an environment on the
  # stack, and the startup that splits it is M1, which kaem.windows.run
  # assembles into mes-m2 too: argv.M1 and crt1.M1, with the instructions
  # they name from x86_defs.M1 and defs.M1.
  crt1 = kaem.runCommand "${pname}-crt1-${version}" env ''
    mkdir ''${out}
    cd ''${out}
    ${CC} -c -o crt1.o ${srcPrefix}/lib/m2/${arch}/${arch}_defs.M1 ${srcPrefix}/lib/windows/${arch}-mes/defs.M1 ${srcPrefix}/lib/windows/${arch}-mes/argv.M1 ${srcPrefix}/lib/windows/${arch}-mes-mescc/crt1.M1
  '';

  mkLib =
    libname: sources:
    kaem.runCommand "${pname}-${libname}-${version}" env ''
      cd ${srcPrefix}
      LIBDIR=''${out}/lib
      mkdir -p ''${LIBDIR}
      catm ''${LIBDIR}/${libname}.c ${sources}

      ${CC} -S -o ''${LIBDIR}/${libname}.s ''${out}/lib/${libname}.c
      ${CC} -c -o ''${LIBDIR}/${libname}.a ''${LIBDIR}/${libname}.s
    '';

  libc-mini = mkLib "libc-mini" libc_mini_SOURCES;
  libmescc = mkLib "libmescc" libmescc_SOURCES;
  libc = mkLib "libc" libc_SOURCES;
  # nixpkgs adds lib/linux/symlink.c for its ln-boot, which Windows does not
  # build: the Windows library has no symlink.
  libc_tcc = mkLib "libc+tcc" libc_tcc_SOURCES;

  # Recompile Mes and Mes C library using mes-m2 bootstrapped Mes
  #
  # mescc-tools-extra's cp writes the file it is given and takes no
  # directory on Windows, so each copy names its file.  crt1 has no .s, being
  # assembled from M1 rather than compiled.
  libs = kaem.runCommand "${pname}-m2-libs-${version}" { inherit pname version; } ''
    LIBDIR=''${out}/lib
    mkdir -p ''${out} ''${LIBDIR}

    mkdir -p ''${LIBDIR}/${arch}-mes

    # crt1.o
    cp ${crt1}/crt1.o ''${LIBDIR}/${arch}-mes/crt1.o

    # libc-mini.a
    cp ${libc-mini}/lib/libc-mini.a ''${LIBDIR}/${arch}-mes/libc-mini.a
    cp ${libc-mini}/lib/libc-mini.s ''${LIBDIR}/${arch}-mes/libc-mini.s

    # libmescc.a
    cp ${libmescc}/lib/libmescc.a ''${LIBDIR}/${arch}-mes/libmescc.a
    cp ${libmescc}/lib/libmescc.s ''${LIBDIR}/${arch}-mes/libmescc.s

    # libc.a
    cp ${libc}/lib/libc.a ''${LIBDIR}/${arch}-mes/libc.a
    cp ${libc}/lib/libc.s ''${LIBDIR}/${arch}-mes/libc.s

    # libc+tcc.a
    cp ${libc_tcc}/lib/libc+tcc.a ''${LIBDIR}/${arch}-mes/libc+tcc.a
    cp ${libc_tcc}/lib/libc+tcc.s ''${LIBDIR}/${arch}-mes/libc+tcc.s
  '';

  # Build mes itself
  compiler =
    let
      objs = builtins.filter builtins.isString (builtins.split " " mes_SOURCES);
    in
    kaem.runCommand "${pname}-${version}" (env // { inherit pname version; }) ''
      mkdir -p ''${out}/bin
      ${builtins.concatStringsSep "\n" (
        map (obj: ''
          ${srcPost.bin}/bin/mes-m2.exe -e main ${srcPost.bin}/bin/mescc.scm -- \
            -D HAVE_CONFIG_H=1 \
            -I ${srcPrefix}/include \
            -I ${srcPrefix}/include/windows/${arch} \
            -I ${srcPrefix}/src \
            -c \
            -o ${baseNameOf obj}.o \
            ${srcPrefix}/${obj}
        '') objs
      )}

      ${srcPost.bin}/bin/mes-m2.exe -e main ${srcPost.bin}/bin/mescc.scm -- \
        -I ${srcPrefix}/src \
        -L ${srcPrefix}/lib \
        -L ${libs}/lib \
        -lc \
        -lmescc \
        -nostdlib \
        --base-address ${baseAddress} \
        -o ''${out}/bin/mes.exe \
        ${libs}/lib/${arch}-mes/crt1.o \
        ${builtins.concatStringsSep " " (map (obj: "${baseNameOf obj}.o") objs)}
    '';

  self = {
    inherit
      src
      srcPost
      srcPrefix
      nyacc
      env
      ;
    inherit compiler libs;
    libc = import ./libc.nix {
      inherit kaem;
      mes = self;
    };
  };
in
self
