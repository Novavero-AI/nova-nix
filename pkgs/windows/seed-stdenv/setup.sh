# nova-nix seed stdenv: the Windows genericBuild, in POSIX sh.
#
# Run by busybox-w32's sh as a derivation's builder.  mkDerivation
# (seed-stdenv/default.nix) passes the package through the environment as
# the stage-1 stdenv's does:
#   $src           source tarball or directory
#   $out           install prefix (the output store path)
#   $buildInputs   dependency store paths
# plus the same optional knobs:
#   $configureFlags / $makeFlags  extra flags for the default phases
#   $dontConfigure                skip ./configure
#   $buildPhase / $installPhase   replace the default build / install commands
# three more, which no package of that stdenv's needs:
#   $patches                      patch files applied to the unpacked source
#   $preConfigure                 commands run in the source tree before
#                                 configure
#   $configureScript              the configure script, ./configure unless
#                                 set
# and the tools, all built from the seed:
#   $initialPath     the store paths whose bin directories the build runs
#   $CC, $LD, $AR    tinycc-native, which finds its runtime with -B
#   $C_INCLUDE_PATH  the headers tinycc-native reads, ';'-separated
#   $CONFIG_SHELL    busybox's sh, which configure writes into the Makefiles
#   $CONFIG_SITE     what configure cannot detect here (see config.site)
#   $buildTriple     the platform configure is told it builds on
#
# It is POSIX sh, since busybox's ash is the shell the seed provides, and
# its phases are the stage-1 stdenv's with a patch phase after unpack and
# without the last.  That stdenv's fixup phase copies each DLL an output
# imports from a build input beside the binary, reading the imports with
# objdump; none of this stdenv's tools reads a PE import table, so it has
# no fixup phase, and an output that links a DLL from a build input does
# not carry it.
set -e

# The builder spells the store C:\nix\store, so every store path in the
# environment holds backslashes, as does the build directory nova-nix
# names.  A backslash starts an escape in a sed script, a C string and a
# word sh reads twice, which configure and make both do with these paths,
# and busybox and every Windows API take '/' as well, so the paths this
# script hands on, the package's and the tools', are respelled with
# forward slashes before anything reads them.  configureFlags is among
# them: configure writes a path it is given into Makefiles and C strings,
# as GCC's does the --with-as and --with-ld it is built with.  A store
# path a package writes into makeFlags or a phase keeps the builder's
# spelling, as it does in the stage-1 stdenv.
# busybox's sh respells imported variables itself only when its
# environment spells PATH, COMSPEC or PROGRAMDATA in mixed case, as Windows
# does (win32/mingw.c's windows_env), and a builder's has PATH and COMSPEC
# in upper case and no PROGRAMDATA.
toSlashes() {
  eval "rest=\${$1}"
  slashed=
  while :; do
    case $rest in
      *\\*)
        slashed=$slashed${rest%%\\*}/
        rest=${rest#*\\}
        ;;
      *) break ;;
    esac
  done
  eval "$1=\$slashed\$rest"
}

# The loop variable is not "name", which is the derivation's own and
# reaches every program the build runs.
for pathVariable in out src patches buildInputs configureFlags initialPath CC LD AR C_INCLUDE_PATH CONFIG_SHELL CONFIG_SITE NIX_BUILD_TOP NIX_STORE; do
  toSlashes "$pathVariable"
done

# Only the seed's tools: busybox's applets, GNU make and tinycc-native.
# busybox-w32 splits PATH on ';', as Windows does.
PATH=
for dir in $initialPath; do
  PATH=${PATH:+$PATH;}$dir/bin
done

# What configure cannot detect about this PATH.  configure picks PATH's
# separator by running sh under a PATH split each way (its "The user is
# always right" block); busybox's sh runs its own sh applet whatever PATH
# holds, so both runs succeed and ':' wins, which splits every C: directory
# in two.  And configure takes a program it searches PATH for only if
# test -x passes, trying each name as it is and then with each suffix in
# ac_executable_extensions; the commands on PATH are .exe files, and
# busybox's test, unlike Cygwin's, does not add the suffix itself.  Both
# are in the environment, which every configure reads, and not in
# config.site: GCC's top level runs the configure of each directory it
# builds for the build machine, and of each target library, with
# CONFIG_SITE=no-such-file (its Makefile.in), and in a native build the
# build machine is this one.
export PATH_SEPARATOR=';' ac_executable_extensions=.exe

# busybox's sh runs its own applet for a command that names one before it
# searches PATH, and busybox has a make; the build's make is GNU make.
export BB_OVERRIDE_APPLETS=make

builddir="$PWD"
toSlashes builddir
mkdir -p "$builddir/tmp"
export TMPDIR="$builddir/tmp" TEMPDIR="$builddir/tmp" TMP="$builddir/tmp" TEMP="$builddir/tmp"

# --- dependency flags ---
# For each buildInput, make its headers and libraries visible to the
# compiler and its programs to the build.  Start from EMPTY flag sets, as
# the stage-1 stdenv does, so nothing but the declared buildInputs reaches
# a configure or make line.
CPPFLAGS=""
LDFLAGS=""
for dep in $buildInputs; do
  CPPFLAGS="$CPPFLAGS -I$dep/include"
  LDFLAGS="$LDFLAGS -L$dep/lib"
  PATH="$PATH;$dep/bin"
done
export CPPFLAGS LDFLAGS PATH

prefix="$out"

# --- unpack phase ---
mkdir -p "$builddir/src"
cd "$builddir/src"
if [ -d "$src" ]; then
  cp -r "$src"/. .
else
  # busybox's tar reads a compressed archive by its magic number.
  tar xf "$src"
  # cd into the single unpacked top-level directory.  Anything else is
  # ambiguous, so fail loudly instead of picking one.
  set -- */
  if [ "$#" -eq 1 ] && [ -d "$1" ]; then
    cd "$1"
  else
    echo "unpack: expected exactly one top-level directory in $src, got: $*" >&2
    exit 1
  fi
fi

# --- patch phase ---
# nixpkgs' patchPhase with its default flags: each patch applies from the
# top of the source tree, less the first component of the paths it names.
# busybox's patch exits non-zero on a hunk it cannot place.
for patchFile in $patches; do
  patch -p1 -i "$patchFile"
done

# --- configure phase (autotools packages only) ---
# $preConfigure first, as nixpkgs' configurePhase runs it: it may move to
# the directory whose configure is the package's, add to the tree what
# configure looks for there, or move to a build directory of its own and
# name the source tree's script in $configureScript, as nixpkgs' does.
# --build, since config.guess cannot name this platform (see default.nix).
# CONFIG_SHELL runs configure, which writes it into the Makefiles as SHELL;
# their default, /bin/sh, names a file on the current drive.
if [ -n "$preConfigure" ]; then
  eval "$preConfigure"
fi
configureScript=${configureScript:-./configure}
if [ -z "$dontConfigure" ] && [ -x "$configureScript" ]; then
  "$CONFIG_SHELL" "$configureScript" --prefix="$prefix" --build="$buildTriple" $configureFlags
fi

# --- build phase ---
if [ -n "$buildPhase" ]; then
  eval "$buildPhase"
else
  make $makeFlags
fi

# --- install phase ---
# $makeFlags again, as nixpkgs' installPhase passes them: a variable a
# package sets for make, as binutils sets MAKEINFO, holds for the rules
# install runs too.
if [ -n "$installPhase" ]; then
  eval "$installPhase"
else
  make $makeFlags install prefix="$prefix"
fi
