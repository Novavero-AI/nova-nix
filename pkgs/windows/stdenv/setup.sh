# nova-nix stage-1 stdenv: the Windows genericBuild.
#
# Run by the seed's bash as a derivation's builder.  mkDerivation (stdenv/default.nix)
# passes the package via the environment:
#   $src           source tarball or directory
#   $out           install prefix (the output store path)
#   $ccPath        the mingw toolchain bin (canonical store path; mapped here)
#   $ccWrapperSrc  the cc-wrapper script (a store path), installed onto PATH
#   $buildInputs   dependency store paths
# plus these optional knobs a package may set:
#   $configureFlags / $makeFlags  extra flags for the default phases
#   $dontConfigure                skip ./configure
#   $buildPhase / $installPhase   replace the default build / install commands
#
# Windows path notes (see the MSYS2 path model): the seed tools are at /usr/bin
# and /bin; an input store path is canonical /nix/store and must be mapped to
# /cygdrive/c for bash, or to a C:/ "mixed" path (via `cygpath -m`) for the
# native mingw compiler; the native $out is converted to a unix prefix with
# `cygpath -u`.
set -e

# Seed tools first; the toolchain joins PATH below once its path is mapped.
export PATH="/usr/bin:/bin"

# Map a canonical /nix/store path to the MSYS2 drive-mounted form (for bash).
# The drive letter comes from $NIX_STORE - the physical store root the
# builder exports at spawn time (build-time only, never derivation text) -
# so a store on any drive works.  Identity stays canonical /nix/store;
# this mapping is the one host-specific step.  Pure parameter expansion:
# nothing here may depend on PATH lookups beyond the seed itself.
storeDrive="${NIX_STORE%%:*}"
if [ "${#storeDrive}" != 1 ]; then storeDrive=c; fi
storeDrive="${storeDrive,,}"
toBash() {
  case "$1" in
    /nix/*) printf '/cygdrive/%s%s' "$storeDrive" "$1" ;;
    *) printf '%s' "$1" ;;
  esac
}

# $ccPath arrives canonical (stdenv/default.nix passes the store path unmapped);
# every use below - including the PATH entry, which gcc's own spawned
# cc1/ld need to find the toolchain DLLs - wants the mapped form.
ccPath="$(toBash "$ccPath")"
export PATH="$PATH:$ccPath"

builddir="$PWD"
mkdir -p "$builddir/tmp"
export TMPDIR="$builddir/tmp" TMP="$builddir/tmp" TEMP="$builddir/tmp"

# --- dependency flags ---
# For each buildInput, make its headers and libraries visible to the compiler.
# The mingw gcc is a native tool, so -I/-L take C:/ "mixed" paths, while its bin
# goes on PATH in the bash drive-mounted form.
# Start from EMPTY flag sets: host-inherited CPPFLAGS/LDFLAGS would inject
# ambient -I/-L directories ahead of the declared buildInputs, silently
# leaking undeclared dependencies into every configure/make line.
CPPFLAGS=""
LDFLAGS=""
for dep in $buildInputs; do
  depWin="$(cygpath -m "$(toBash "$dep")")"
  CPPFLAGS="$CPPFLAGS -I$depWin/include"
  LDFLAGS="$LDFLAGS -L$depWin/lib"
  PATH="$PATH:$(toBash "$dep")/bin"
done
export CPPFLAGS LDFLAGS PATH

# --- cc-wrapper: route every toolchain call through our flag-adding shim ---
# Install cc-wrapper.sh (a store path, passed as $ccWrapperSrc) under every
# tool name the build may invoke, in a directory placed ahead of the toolchain
# on PATH, and hand it the toolchain dir by absolute path.  A tool reachable
# directly on PATH would bypass both the hermeticity unsets and the
# deterministic-link flag.  See cc-wrapper.sh for the per-tool treatment.
wrapperBin="$builddir/wrappers"
mkdir -p "$wrapperBin"
for tool in gcc cc g++ c++ ld windres; do
  cp "$(toBash "$ccWrapperSrc")" "$wrapperBin/$tool"
  chmod +x "$wrapperBin/$tool"
done
export NN_TOOLCHAIN="$ccPath"
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
  # cd into the single unpacked top-level directory.  Anything else is
  # ambiguous - silently picking one would build the alphabetically-first
  # entry of a multi-directory tarball - so fail loudly instead.
  set -- */
  if [ "$#" -eq 1 ] && [ -d "$1" ]; then
    cd "$1"
  else
    echo "unpack: expected exactly one top-level directory in $src, got: $*" >&2
    exit 1
  fi
fi

# --- configure phase (autotools packages only) ---
# $dontConfigure skips this -- for packages with no ./configure, or that build
# via a hand-written makefile.
if [ -z "$dontConfigure" ] && [ -x ./configure ]; then
  ./configure --prefix="$prefix" $configureFlags
fi

# --- build phase ---
# $buildPhase overrides the default for packages that build differently (e.g.
# `make -f win32/Makefile.gcc`).  It runs in the unpacked source dir, with
# $prefix and the toolchain already in scope.
if [ -n "$buildPhase" ]; then
  eval "$buildPhase"
else
  make $makeFlags
fi

# --- install phase ---
# $installPhase overrides the default the same way.
if [ -n "$installPhase" ]; then
  eval "$installPhase"
else
  make install prefix="$prefix"
fi

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
 uxtheme.dll dwmapi.dll ucrtbase.dll winhttp.dll wininet.dll normaliz.dll \
 shcore.dll msimg32.dll winspool.drv "

# Where to look for a non-system DLL: the toolchain bin, then each input's bin.
dllSearchDirs="$ccPath"
for dep in $buildInputs; do
  dllSearchDirs="$dllSearchDirs $(toBash "$dep")/bin"
done

isSystemDll() {
  local dllLower
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
  local bindir="$1" changed bin needed found dir imports
  [ -d "$bindir" ] || return 0
  # A missing or broken objdump must FAIL the fixup, not read as "this
  # binary has no imports" - that would silently register an output
  # missing its bundled DLLs.
  command -v objdump >/dev/null 2>&1 || {
    echo "fixup: objdump not found; cannot scan DLL imports" >&2
    exit 1
  }
  changed=1
  while [ "$changed" = 1 ]; do
    changed=0
    for bin in "$bindir"/*.exe "$bindir"/*.dll; do
      [ -e "$bin" ] || continue
      imports="$(objdump -p "$bin")" || {
        echo "fixup: objdump failed on $bin" >&2
        exit 1
      }
      for needed in $(printf '%s\n' "$imports" | sed -n 's/^[[:space:]]*DLL Name: //p'); do
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
