# nova-nix stage-1 stdenv: the Windows genericBuild.
#
# Run by the seed's bash as a derivation's builder.  mkDerivation (stdenv.nix)
# passes the package via the environment: $src (source tarball or directory),
# $out (install prefix), $ccPath (the mingw toolchain bin, already
# drive-translated), $buildInputs (dependency store paths), and the optional
# $configureFlags / $makeFlags.
#
# Windows path notes (see the MSYS2 path model): the seed tools are at /usr/bin
# and /bin; an input store path is canonical /nix/store and must be mapped to
# /cygdrive/c for bash, or to a C:/ "mixed" path (via `cygpath -m`) for the
# native mingw compiler; the native $out is converted to a unix prefix with
# `cygpath -u`.
set -e

export PATH="/usr/bin:/bin:$ccPath"

builddir="$PWD"
mkdir -p "$builddir/tmp"
export TMPDIR="$builddir/tmp" TMP="$builddir/tmp" TEMP="$builddir/tmp"

# Map a canonical /nix/store path to the MSYS2 drive-mounted form (for bash).
toBash() {
  case "$1" in
    /nix/*) printf '/cygdrive/c%s' "$1" ;;
    *) printf '%s' "$1" ;;
  esac
}

# --- dependency flags ---
# For each buildInput, make its headers and libraries visible to the compiler.
# The mingw gcc is a native tool, so -I/-L take C:/ "mixed" paths, while its bin
# goes on PATH in the bash drive-mounted form.
for dep in $buildInputs; do
  depWin="$(cygpath -m "$(toBash "$dep")")"
  CPPFLAGS="$CPPFLAGS -I$depWin/include"
  LDFLAGS="$LDFLAGS -L$depWin/lib"
  PATH="$PATH:$(toBash "$dep")/bin"
done
export CPPFLAGS LDFLAGS PATH

prefix="$(cygpath -u "$out")"

# --- unpack phase ---
mkdir -p "$builddir/src"
cd "$builddir/src"
srcPath="$(toBash "$src")"
if [ -d "$srcPath" ]; then
  cp -r "$srcPath"/. .
else
  tar xf "$srcPath"
  cd "$(ls -d */ | head -1)"
fi

# --- configure phase (autotools packages only) ---
if [ -x ./configure ]; then
  ./configure --prefix="$prefix" $configureFlags
fi

# --- build phase ---
make $makeFlags

# --- install phase ---
make install prefix="$prefix"
