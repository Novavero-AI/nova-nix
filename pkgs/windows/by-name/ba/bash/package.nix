# GNU Bash 5.3 with GNU's official patches, for x86_64 Windows, built by
# the package set's stdenv and running on msvcrt alone, with no Cygwin or
# MSYS2 runtime.
#
# The port is the C files and replacement headers under win32/, which give
# bash the POSIX interfaces MinGW-w64 lacks over the Windows API, and
# win32.patch, which has the shell start a program as a new process where
# it would fork and exec one.  Windows has no fork, and this bash makes
# no copy of itself yet: a program, with its pipes and redirections, a #!
# script and a script with no #! line run, and a subshell, a command
# substitution, a pipeline holding a builtin or function, a coprocess and
# a background job other than a program fail with "fork: Function not
# implemented".  win32/proc.c says how a program is started and waited
# for, win32/io.c how descriptors and files behave, and win32/signals.c
# which signals arrive.
#
# A script's PATH keeps the value the script gives it.  The command search
# and a program's environment split it at `;' and at a `:' that does not
# end a drive letter, so C:\a\bin:C:\b\bin names two directories, and a
# program finds them joined with `;'.  In each directory the search takes
# NAME.exe where NAME is no program, as busybox-w32's does.  /dev/null and /dev/tty open as NUL
# and CON, and /dev/fd/N as a duplicate of descriptor N.
#
# The configuration is a builder's: no job control, which needs process
# groups, no line editing or history, which an interactive shell alone
# uses, no translations and no network redirections.  bash's own malloc
# takes its memory from a region win32/proc.c reserves.  The cache answers
# are what the port provides and configure cannot test here: /dev/fd and
# /dev/stdin as w32_open maps them, getcwd allocating as w32_getcwd does,
# and getenv left to msvcrt, whose declaration differs from the one bash
# would redefine.  strdup and wcsdup are bash's own: msvcrt's allocate from
# msvcrt's heap, and bash frees what they return with its own malloc's
# free.  A #! line is bash's to read: Windows starts no #! script, and
# configure's check that the system does passes only because busybox-w32's
# sh, which runs configure, reads the line itself.
#
# GNU publishes the patches as context diffs, which busybox's patch does
# not read, so a derivation writes each as the unified diff it is
# (context-to-unified.awk says how), and they apply in order before
# win32.patch.
{
  stdenv,
  fetchurl,
  busybox-w32,
}:
let
  version = "5.3";
  patchLevel = 20;

  src = fetchurl {
    url = "mirror://gnu/bash/bash-${version}.tar.gz";
    sha256 = "0d5cd86965f869a26cf64f4b71be7b96f90a3ba8b3d74e27e8e9d9d5550f31ba";
  };

  # GNU's official patches, bash53-001 to bash53-020, each signed by bash's
  # maintainer, as the tarball is.
  officialPatches =
    map
      (
        { n, sha256 }:
        fetchurl {
          url = "mirror://gnu/bash/bash-${version}-patches/bash53-${n}";
          inherit sha256;
        }
      )
      [
        { n = "001"; sha256 = "1f608434364af86b9b45c8b0ea3fb3b165fb830d27697e6cdfc7ac17dee3287f"; }
        { n = "002"; sha256 = "e385548a00130765ec7938a56fbdca52447ab41fabc95a25f19ade527e282001"; }
        { n = "003"; sha256 = "f245d9c7dc3f5a20d84b53d249334747940936f09dc97e1dcb89fc3ab37d60ed"; }
        { n = "004"; sha256 = "9591d245045529f32f0812f94180b9d9ce9023f5a765c039b852e5dfc99747d0"; }
        { n = "005"; sha256 = "cca1ef52dbbf433bc98e33269b64b2c814028efe2538be1e2c9a377da90bc99d"; }
        { n = "006"; sha256 = "29119addefed8eff91ae37fd51822c31780ee30d4a28376e96002706c995ff10"; }
        { n = "007"; sha256 = "c0976bbfffa1453c7cfdd62058f206a318568ff2d690f5d4fa048793fa3eb299"; }
        { n = "008"; sha256 = "097cd723cbfb8907674ac32214063a3fd85282657ec5b4e544d2c0f719653fb4"; }
        { n = "009"; sha256 = "eee30fe78a4b0cb2fe20e010e00308899cfc613e0774ebb3c8557a1552f24f8c"; }
        { n = "010"; sha256 = "cf76f1cce2ea300c18bff9f002d21f280cc931acd17c28518110b93fe6e72569"; }
        { n = "011"; sha256 = "0298df8f5ea2a31d3be43ed7d269c5b3c7c342dd5b570bea7f64d66dcbbe7531"; }
        { n = "012"; sha256 = "d71379b39bebaedaf123414414e77fb458a0a43b9ad3116594c6df7ca6754573"; }
        { n = "013"; sha256 = "042f9cda967e24bf4211944697441e93d06ff42b4b998629a98a1b249279f200"; }
        { n = "014"; sha256 = "bd4360b401d38507e358783dcad8536a99c6789f0d3a5bd0cfb8c4a34144696c"; }
        { n = "015"; sha256 = "55b79ceee2fc27f6767eed697e939a7eb2fe2a28c01556bd75f18d581014f46e"; }
        { n = "016"; sha256 = "9ea29b266b7d24cb34d0ff3f1c4631e4d527bfe2d1ef15d17cdb924bf31ef767"; }
        { n = "017"; sha256 = "443b927b45c1558ca72052410f8b8f6e5152b617ed707061a2781d4375b0d1c3"; }
        { n = "018"; sha256 = "ae715d76c50341d7d7095e9a8d2eeed1ca9546152c2ac7289206f90cf30ac697"; }
        { n = "019"; sha256 = "a25c581e4d0057dea3833918438a930e2e86ee4c6dc17fe15267b7f04cbc4e3d"; }
        { n = "020"; sha256 = "df217ed3a9122aa2286d9b67bbe348661b6a9db262b580c29150dae55d532896"; }
      ];

  # Each official patch as a unified diff, $out/<patch>.patch.
  unifiedPatches = derivation {
    name = "bash-${version}-patches";
    system = "x86_64-windows";
    builder = "${busybox-w32}/bin/busybox.exe";
    args = [
      "sh"
      "-e"
      (builtins.toFile "bash-patches.sh" ''
        mkdir "$out"
        for patch in $patches; do
          name=''${patch##*[/\\]}
          awk -f "$converter" "$patch" > "$out/''${name#*-}.patch"
        done
      '')
    ];
    patches = officialPatches;
    converter = ./context-to-unified.awk;
  };
in
stdenv.mkDerivation {
  name = "bash-${version}p${toString patchLevel}";
  inherit src;

  patches = map (patch: "${unifiedPatches}/${patch.name}.patch") officialPatches ++ [ ./win32.patch ];

  port = ./win32;

  # The port's library, built before configure so that configure's link
  # tests find what it provides.  configure writes _FILE_OFFSET_BITS 64 into
  # config.h, and the library is compiled with it, so the two agree on what
  # struct stat is; the build phase stops if config.h says otherwise.
  preConfigure = ''
    cp -r "$port" win32
    chmod -R u+w win32
    for file in io proc signals misc; do
      $CC -O2 -D_FILE_OFFSET_BITS=64 -Iwin32/include -c win32/$file.c -o win32/$file.o
    done
    $AR rcs win32/libw32.a win32/io.o win32/proc.o win32/signals.o win32/misc.o
    export CPPFLAGS="$CPPFLAGS -I$PWD/win32/include" LIBS="$PWD/win32/libw32.a"
  '';

  configureFlags = [
    "--disable-job-control"
    "--disable-readline"
    "--disable-history"
    "--disable-nls"
    "--disable-net-redirections"
    "--without-curses"
    "--with-bash-malloc"
    "bash_cv_dev_fd=standard"
    "bash_cv_dev_stdin=present"
    "bash_cv_getcwd_malloc=yes"
    "bash_cv_getenv_redef=no"
    "ac_cv_func_strdup=no"
    "ac_cv_func_wcsdup=no"
    "ac_cv_sys_interpreter=no"
  ];

  enableParallelBuilding = true;
  buildPhase = ''
    grep -q '^#define _FILE_OFFSET_BITS 64$' config.h || {
      echo "bash: config.h does not define _FILE_OFFSET_BITS 64, which win32/libw32.a is built with" >&2
      exit 1
    }
    make ''${enableParallelBuilding:+-j$NIX_BUILD_CORES} $makeFlags
  '';
}
// {
  srcs = [ src ] ++ officialPatches;
  inherit version patchLevel;
}
