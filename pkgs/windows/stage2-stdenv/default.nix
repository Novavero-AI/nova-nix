# nova-nix stage-2 stdenv: mkDerivation for x86_64 Windows on the seed
# ladder's native x86_64 toolchain, the package set's stdenv.
#
# The seed stdenv's interface, so a package builds with either:
#
#   { stdenv }:
#   stdenv.mkDerivation { name = "foo"; src = ...; }
#
# with the seed stdenv's builder and tools (../seed-stdenv): busybox-w32's
# sh runs that stdenv's setup.sh, through this one's, which adds the fixup
# phase it has not (./setup.sh says how), with busybox-w32's applets as the
# coreutils, sed, grep and awk and gnumake as make.  The compilers and the
# linker are gcc-native-x86_64's and binutils-native-x86_64's, which run on
# x86_64 Windows and write x86_64-w64-mingw32, so this stdenv's system is
# x86_64-windows, and everything it builds is x86_64.  setup.sh's header
# lists what it reads, the seed stdenv's tools among them; here CC, CXX,
# LD, AR, CONFIG_SITE and buildTriple name this stdenv's, and nothing sets
# C_INCLUDE_PATH, which TinyCC alone needs.
#
# A build reaches the compilers and the linker through cc, nixpkgs'
# stdenv.cc: gcc, cc, g++, c++, ld and windres, scripts that give every
# build C at -std=gnu17, links with no time stamp and no include or library
# directory from the environment (cc-wrapper.sh, link.specs and
# ld-wrapper.sh say how).  They have no extension: busybox takes a file
# that begins with #! for a program, and its sh and GNU make each run one
# through the sh that line names (has_exec_format and do_lstat in
# win32/mingw.c, tryexec in shell/ash.c and mingw_spawn_interpreter in
# win32/process.c in busybox-w32 5f09fb446, process_begin in
# src/w32/subproc/sub_proc.c in make 4.4.1).  GNU make looks for a command
# with .exe in every directory of PATH before it looks for it with no
# extension in any (find_file in sub_proc.c), so it runs a wrapper only
# while PATH holds no program of the same name: neither toolchain's bin
# directory is on it.  CC, CXX and LD name the wrappers, as nixpkgs' cc and
# bintools wrappers' setup hooks name theirs (the CC and CXX exports in
# pkgs/build-support/cc-wrapper/setup-hook.sh and the loop over ar, as, ld
# and the rest in bintools-wrapper/setup-hook.sh in nixpkgs
# 5eb598da22ce76bc18ec85ecc1de66ca838b3514), and AR binutils' archiver by
# its path: busybox's sh runs its own ar applet for a command named ar,
# sparing make alone (setup.sh's BB_OVERRIDE_APPLETS), and the applets'
# ar.exe comes first on PATH.  GNU ar writes an archive's symbol index
# unless told not to (write_archive in binutils/ar.c in binutils 2.46), so
# it does not matter that configure finds no ranlib.  OBJDUMP and STRIP name
# binutils' programs of those names by their paths, as nixpkgs' bintools
# wrapper names them for a build, WINDRES names cc's windres, a wrapper of
# binutils' that preprocesses with gcc-native-x86_64's gcc
# (windres-wrapper.sh says why), and a build runs binutils' others by
# their paths.
{
  busybox-w32,
  gnumake,
  seedStdenv,
  binutils-native-x86_64,
  gcc-native-x86_64,
}:
let
  system = "x86_64-windows";
  inherit (seedStdenv) applets;
  inherit (builtins.parseDrvName gcc-native-x86_64.name) version;

  # wrap.sh writes the wrappers from their templates, with these paths.
  cc = derivation {
    name = "gcc-wrapper-${version}";
    inherit system;
    builder = "${busybox-w32}/bin/busybox.exe";
    args = [
      "sh"
      ./wrap.sh
    ];
    compilers = "${gcc-native-x86_64}/bin";
    linker = "${binutils-native-x86_64}/bin/ld.exe";
    windres = "${binutils-native-x86_64}/bin/windres.exe";
    specs = ./link.specs;
    ccWrapper = ./cc-wrapper.sh;
    ldWrapper = ./ld-wrapper.sh;
    windresWrapper = ./windres-wrapper.sh;
  };
in
{
  inherit cc;

  mkDerivation =
    attrs:
    derivation (
      attrs
      // {
        inherit system;
        builder = "${busybox-w32}/bin/busybox.exe";
        args = [
          "sh"
          ./setup.sh
        ];
        seedSetup = ../seed-stdenv/setup.sh;
        initialPath = [
          applets
          gnumake
          cc
        ];
        CC = "gcc";
        CXX = "g++";
        LD = "ld";
        AR = "${binutils-native-x86_64}/bin/ar.exe";
        OBJDUMP = "${binutils-native-x86_64}/bin/objdump.exe";
        STRIP = "${binutils-native-x86_64}/bin/strip.exe";
        WINDRES = "windres";
        dllSearchPath = "${gcc-native-x86_64}/bin";
        CONFIG_SHELL = "${applets}/bin/sh.exe";
        CONFIG_SITE = ./config.site;
        # The target gcc-native-x86_64 is configured for, which
        # gcc -dumpmachine prints, since config.guess cannot work out the
        # platform from busybox's uname, whose system name is Windows_NT.
        buildTriple = "x86_64-w64-mingw32";
      }
      # Parallel installing follows parallel building unless the package
      # says otherwise, as nixpkgs' make-derivation.nix sets it.
      // (
        if attrs.enableParallelBuilding or false then
          { enableParallelInstalling = attrs.enableParallelInstalling or true; }
        else
          { }
      )
    )
    // {
      # What the package fetches, which ../sources.nix collects: its src,
      # unless that is a path in the tree.
      srcs = if (attrs.src or null) ? outputHash then [ attrs.src ] else [ ];
    };
}
