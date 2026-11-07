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
# and the tools, all built from the seed:
#   $initialPath     the store paths whose bin directories the build runs
#   $CC, $LD, $AR    tinycc-native, which finds its runtime with -B
#   $C_INCLUDE_PATH  the headers tinycc-native reads, ';'-separated
#   $CONFIG_SHELL    busybox's sh, which configure writes into the Makefiles
#   $CONFIG_SITE     what configure cannot detect here (see config.site)
#   $buildTriple     the platform configure is told it builds on
#
# It is POSIX sh, since busybox's ash is the shell the seed provides, and
# its phases are the stage-1 stdenv's but the last.  That stdenv's fixup
# phase copies each DLL an output imports from a build input beside the
# binary, reading the imports with objdump; nothing built from the seed
# reads a PE import table, so this stdenv has no fixup phase, and an
# output that links a DLL from a build input does not carry it.
set -e

# The builder spells the store C:\nix\store, so every store path in the
# environment holds backslashes, as does the build directory nova-nix
# names.  A backslash starts an escape in a sed script, a C string and a
# word sh reads twice, which configure and make both do with these paths,
# and busybox and every Windows API take '/' as well, so the paths this
# script hands on, the package's and the tools', are respelled with
# forward slashes before anything reads them.  A store path a package
# writes into its flags or phases keeps the builder's spelling, as it does
# in the stage-1 stdenv.  busybox's sh respells imported variables itself only when its
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
for pathVariable in out src buildInputs initialPath CC LD AR C_INCLUDE_PATH CONFIG_SHELL CONFIG_SITE NIX_BUILD_TOP NIX_STORE; do
  toSlashes "$pathVariable"
done

# Only the seed's tools: busybox's applets, GNU make and tinycc-native.
# busybox-w32 splits PATH on ';', as Windows does.
PATH=
for dir in $initialPath; do
  PATH=${PATH:+$PATH;}$dir/bin
done

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

# --- configure phase (autotools packages only) ---
# --build, since config.guess cannot name this platform (see default.nix).
# CONFIG_SHELL runs configure, which writes it into the Makefiles as SHELL;
# their default, /bin/sh, names a file on the current drive.
if [ -z "$dontConfigure" ] && [ -x ./configure ]; then
  "$CONFIG_SHELL" ./configure --prefix="$prefix" --build="$buildTriple" $configureFlags
fi

# --- build phase ---
if [ -n "$buildPhase" ]; then
  eval "$buildPhase"
else
  make $makeFlags
fi

# --- install phase ---
if [ -n "$installPhase" ]; then
  eval "$installPhase"
else
  make install prefix="$prefix"
fi
