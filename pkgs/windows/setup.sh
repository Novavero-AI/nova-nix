# nova-nix stage-1 stdenv: the Windows genericBuild.
#
# Run by the seed's bash as a derivation's builder.  mkDerivation (stdenv.nix)
# passes the package via the environment: $src (source tarball), $out (install
# prefix), $ccPath (the mingw toolchain bin, already drive-translated), and the
# optional $configureFlags / $makeFlags.
#
# Windows path notes (see the MSYS2 path model): the seed tools are at /usr/bin
# and /bin; an input store path ($src) is canonical /nix/store and must be
# mapped to /cygdrive/c for bash; the native $out is used as-is and converted to
# a unix-form prefix with cygpath.
set -e

export PATH="/usr/bin:/bin:$ccPath"

builddir="$PWD"
mkdir -p "$builddir/tmp"
export TMPDIR="$builddir/tmp" TMP="$builddir/tmp" TEMP="$builddir/tmp"

# Map a canonical /nix/store path to the MSYS2 drive-mounted form.
toBash() {
  case "$1" in
    /nix/*) printf '/cygdrive/c%s' "$1" ;;
    *) printf '%s' "$1" ;;
  esac
}

# --- unpack phase ---
mkdir -p "$builddir/src"
cd "$builddir/src"
tar xf "$(toBash "$src")"
cd "$(ls -d */ | head -1)"

# --- configure phase ---
./configure --prefix="$(cygpath -u "$out")" $configureFlags

# --- build phase ---
make $makeFlags

# --- install phase ---
make install
