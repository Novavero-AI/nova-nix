# pkgs/windows' Mes, as the Mes (Windows) CI jobs build it: mes-m2 and mes,
# each a script that runs it with --version, as nixpkgs' get-version test
# does; hello, which MesCC compiles and links with mes, and which prints its
# first argument, or Mes, and exits with its argument count; exec-bit, in
# which a program MesCC compiles and links makes files with open and chmod;
# and arena, MesCC compiling libc+tcc, the largest of Mes's libraries, as
# the libraries are built, with MES_DEBUG=3 printing the cells Mes keeps
# across each collection.
let
  inherit (import ../../pkgs/windows) mes stage0-pe32;
  inherit (stage0-pe32) kaem writeText;

  getVersion =
    name: program:
    kaem.runCommand "${name}-get-version" mes.env ''
      ${program} --version
      mkdir ''${out}
    '';

  # The script lines in which MesCC, running on mes, compiles the C file
  # source and links it as output.  MesCC writes its intermediate files
  # beside its input, so it compiles a copy in the build directory rather
  # than the file in the store.  It links libc and libmescc unless told
  # otherwise, and printf and strcat are in libc+tcc, which nixpkgs'
  # tinycc-bootstrappable links tcc with in the same way.
  compile = source: output: ''
    cp ${source} ${source.name}
    ${mes.compiler}/bin/mes.exe -e main ${mes.srcPost.bin}/bin/mescc.scm -- \
      -D HAVE_CONFIG_H=1 \
      -I ${mes.srcPrefix}/include \
      -I ${mes.srcPrefix}/include/windows/x86 \
      -L ${mes.libs}/lib \
      -l c+tcc \
      --base-address ${mes.baseAddress} \
      -o ${output} \
      ${source.name}
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

  # Makes three files in the directory its argument names, each holding
  # CONTENTS: open-0755, created with that mode; open-0644, created with
  # that mode; and chmod-0755, created with mode 0644 and then chmodded to
  # 0755.  If open, write, close or chmod fails, it names the call and the
  # file and exits 1.
  execBitC = writeText "exec-bit.c" ''
    #include <fcntl.h>
    #include <stdio.h>
    #include <stdlib.h>
    #include <string.h>
    #include <sys/stat.h>
    #include <unistd.h>

    #define CONTENTS "written by Mes libc\n"

    static int
    failed (char const *call, char const *path)
    {
      fputs (call, stderr);
      fputs (" failed: ", stderr);
      fputs (path, stderr);
      fputs ("\n", stderr);
      return 1;
    }

    static char *
    in_dir (char const *dir, char const *name)
    {
      char *path;

      path = malloc (strlen (dir) + strlen (name) + 2);
      strcpy (path, dir);
      strcat (path, "/");
      strcat (path, name);
      return path;
    }

    static int
    create (char const *path, int mode)
    {
      int fd;
      int size;
      int written;

      fd = open (path, O_WRONLY | O_CREAT | O_TRUNC, mode);
      if (fd < 0)
        return failed ("open", path);
      size = strlen (CONTENTS);
      written = write (fd, CONTENTS, size);
      if (close (fd) != 0)
        return failed ("close", path);
      if (written != size)
        return failed ("write", path);
      return 0;
    }

    int
    main (int argc, char **argv)
    {
      char *chmodded;

      chmodded = in_dir (argv[1], "chmod-0755");
      if (create (in_dir (argv[1], "open-0755"), 0755) != 0
          || create (in_dir (argv[1], "open-0644"), 0644) != 0
          || create (chmodded, 0644) != 0)
        return 1;
      if (chmod (chmodded, 0755) != 0)
        return failed ("chmod", chmodded);
      return 0;
    }
  '';

  libcTcc = (builtins.fromJSON (builtins.readFile ../../pkgs/windows/by-name/me/mes/sources.json))."x86.windows.mescc".libc_tcc_SOURCES;
in
{
  mes-m2 = getVersion "mes-m2" "${mes.srcPost.bin}/bin/mes-m2.exe";
  mes = getVersion "mes" "${mes.compiler}/bin/mes.exe";

  hello = kaem.runCommand "mes-hello" mes.env ''
    mkdir -p ''${out}/bin
    ${compile helloC "\${out}/bin/hello.exe"}
  '';

  # Fixed-output, so the build registers its output only if the output's NAR
  # hash is this one: open-0755 and chmod-0755 executable and open-0644 not.
  # It is the hash of the same tree on POSIX, where the bit is the mode, as
  # `nix-hash --type sha256 exec-bit` prints it after
  #   mkdir exec-bit
  #   printf 'written by Mes libc\n' > exec-bit/open-0755
  #   cp exec-bit/open-0755 exec-bit/open-0644
  #   cp exec-bit/open-0755 exec-bit/chmod-0755
  #   chmod 755 exec-bit/open-0755 exec-bit/chmod-0755
  #   chmod 644 exec-bit/open-0644
  # With no file executable it would be
  # a2fa27cc356717d6e26e0353b45ecc9e247f30e16030731afd169e84f7ecc5af.
  exec-bit =
    kaem.runCommand "mes-exec-bit"
      (
        mes.env
        // {
          outputHashMode = "recursive";
          outputHashAlgo = "sha256";
          outputHash = "fb6574a2340a74fd8557859e349750994d8e04f3cb0c0f3c4f042ce350790815";
        }
      )
      ''
        ${compile execBitC "exec-bit.exe"}
        mkdir ''${out}
        ./exec-bit.exe ''${out}
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
