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

# --- cc-wrapper: route every compiler call through our flag-adding shim ---
# Install cc-wrapper.sh (a store path, passed as $ccWrapperSrc) under the tool
# names the build invokes, in a directory placed ahead of the toolchain on PATH,
# and hand it the real gcc by absolute path.  See cc-wrapper.sh for the why.
wrapperBin="$builddir/wrappers"
mkdir -p "$wrapperBin"
for tool in gcc cc; do
  cp "$(toBash "$ccWrapperSrc")" "$wrapperBin/$tool"
  chmod +x "$wrapperBin/$tool"
done
export NN_REAL_CC="$ccPath/gcc.exe"
export PATH="$wrapperBin:$PATH"

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

# --- fixup phase: bundle non-system DLLs so outputs are self-contained ---
# Windows has no RPATH: an executable finds its DLLs in its own directory first,
# then the system dirs, then PATH.  So a distributable output must carry every
# non-system DLL it needs next to the binary -- it cannot lean on whatever
# happens to be on the host's PATH.  We read each output binary's PE import
# table (objdump), and for every imported DLL that is not part of the guaranteed
# Windows ABI we find it among the declared inputs and copy it beside the
# binary.  We repeat to a fixpoint, because a bundled DLL has its own imports
# (e.g. libintl needs libiconv) -- this is the runtime-closure walk.  If a
# non-system DLL is not found in the inputs, we fail the build: that is an
# undeclared dependency, not something to ship silently broken.

# DLLs guaranteed present on every Windows machine (the ABI) -- never bundled.
# Matched case-insensitively; the api-ms-win-* / ext-ms-win-* API sets are
# virtual and always resolved by the OS, so they are treated as system too.
systemDlls=" kernel32.dll kernelbase.dll ntdll.dll msvcrt.dll user32.dll \
 gdi32.dll gdi32full.dll advapi32.dll sechost.dll rpcrt4.dll combase.dll \
 shell32.dll shlwapi.dll ole32.dll oleaut32.dll ws2_32.dll wsock32.dll \
 comctl32.dll comdlg32.dll winmm.dll version.dll crypt32.dll secur32.dll \
 bcrypt.dll ncrypt.dll userenv.dll iphlpapi.dll dnsapi.dll setupapi.dll \
 psapi.dll powrprof.dll dbghelp.dll imm32.dll netapi32.dll mpr.dll \
 uxtheme.dll dwmapi.dll ucrtbase.dll "

# Where to look for a non-system DLL: the toolchain bin, then each input's bin.
dllSearchDirs="$ccPath"
for dep in $buildInputs; do
  dllSearchDirs="$dllSearchDirs $(toBash "$dep")/bin"
done

isSystemDll() {
  dllLower="$(printf '%s' "$1" | tr '[:upper:]' '[:lower:]')"
  case "$dllLower" in
    api-ms-win-* | ext-ms-win-*) return 0 ;;
  esac
  case "$systemDlls" in
    *" $dllLower "*) return 0 ;;
  esac
  return 1
}

bundleDlls() {
  bindir="$1"
  [ -d "$bindir" ] || return 0
  changed=1
  while [ "$changed" = 1 ]; do
    changed=0
    for bin in "$bindir"/*.exe "$bindir"/*.dll; do
      [ -e "$bin" ] || continue
      for needed in $(objdump -p "$bin" 2>/dev/null | sed -n 's/^[[:space:]]*DLL Name: //p'); do
        isSystemDll "$needed" && continue
        [ -e "$bindir/$needed" ] && continue
        found=0
        for dir in $dllSearchDirs; do
          if [ -e "$dir/$needed" ]; then
            cp "$dir/$needed" "$bindir/"
            echo "fixup: bundled $needed (needed by $(basename "$bin"))"
            changed=1
            found=1
            break
          fi
        done
        if [ "$found" = 0 ]; then
          echo "fixup: ERROR: $(basename "$bin") needs $needed, which is not a" \
               "system DLL and was not found in the declared inputs" >&2
          echo "fixup: searched: $dllSearchDirs" >&2
          exit 1
        fi
      done
    done
  done
}

bundleDlls "$prefix/bin"
