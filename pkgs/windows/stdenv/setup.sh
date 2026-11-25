# nova-nix stdenv: the seed stdenv's genericBuild, then the fixup
# phase it has not.
#
# Run by busybox-w32's sh as a derivation's builder.  $seedSetup is the
# seed stdenv's setup.sh, which this runs as it is, in this shell, for
# every phase from unpack to install; its header says what it reads.
# This reads three more, which the seed stdenv does not set:
#   $OBJDUMP, $STRIP  binutils' programs by their paths, as nixpkgs'
#                     bintools wrapper names them for a build; a
#                     package's phases may run the second
#   $dllSearchPath    the toolchain's bin directory, the first place the
#                     fixup looks for a DLL
#
# The fixup copies each DLL an output's programs and libraries import,
# and that Windows does not provide, beside them: Windows has no RPATH,
# and a program finds a DLL in its own directory before the system's and
# PATH's.  The imports are read from
# each PE file with objdump, and a bundled DLL's own imports in turn, to a
# fixpoint.  A DLL found in no input is an undeclared dependency, and
# fails the build.
set -e

# These paths reach the package's phases, which the seed stdenv's setup.sh
# runs, so they are respelled with forward slashes first, for the reason
# that script gives for its own.  busybox's sh runs its sed applet
# whatever PATH holds, so it is here before PATH is set.
for pathVariable in OBJDUMP STRIP dllSearchPath; do
  eval "spelled=\${$pathVariable}"
  slashed=$(printf '%s\n' "$spelled" | sed 's|\\|/|g')
  eval "$pathVariable=\$slashed"
done
export OBJDUMP STRIP

. "$seedSetup"

# --- fixup phase ---
# The DLLs every Windows machine provides, which are never bundled,
# matched without regard to case.  The api-ms-win-* and ext-ms-win-* API
# sets are virtual, and the system resolves them.
systemDlls=" kernel32.dll kernelbase.dll ntdll.dll msvcrt.dll user32.dll \
 gdi32.dll gdi32full.dll advapi32.dll sechost.dll rpcrt4.dll combase.dll \
 shell32.dll shlwapi.dll ole32.dll oleaut32.dll ws2_32.dll wsock32.dll \
 comctl32.dll comdlg32.dll winmm.dll version.dll crypt32.dll secur32.dll \
 bcrypt.dll ncrypt.dll userenv.dll iphlpapi.dll dnsapi.dll setupapi.dll \
 psapi.dll powrprof.dll dbghelp.dll imm32.dll netapi32.dll mpr.dll \
 uxtheme.dll dwmapi.dll ucrtbase.dll winhttp.dll wininet.dll normaliz.dll \
 shcore.dll msimg32.dll winspool.drv "

dllSearchDirs="$dllSearchPath"
for dep in $buildInputs; do
  dllSearchDirs="$dllSearchDirs $dep/bin"
done

isSystemDll() {
  dllLower=$(printf '%s' "$1" | tr 'A-Z' 'a-z')
  case $dllLower in
    api-ms-win-* | ext-ms-win-*) return 0 ;;
  esac
  case $systemDlls in
    *" $dllLower "*) return 0 ;;
  esac
  return 1
}

# objdump is a native program, so its text-mode output ends each line
# with a carriage return, which is removed before a name is read.
bundleDlls() {
  bindir=$1
  [ -d "$bindir" ] || return 0
  changed=1
  while [ "$changed" = 1 ]; do
    changed=0
    for binary in "$bindir"/*.exe "$bindir"/*.dll; do
      [ -e "$binary" ] || continue
      imports=$("$OBJDUMP" -p "$binary") || {
        echo "fixup: objdump failed on $binary" >&2
        exit 1
      }
      for needed in $(printf '%s\n' "$imports" | tr -d '\r' | sed -n 's/^[[:space:]]*DLL Name: //p'); do
        isSystemDll "$needed" && continue
        [ -e "$bindir/$needed" ] && continue
        found=0
        for dir in $dllSearchDirs; do
          if [ -e "$dir/$needed" ]; then
            cp "$dir/$needed" "$bindir/"
            echo "fixup: bundled $needed (needed by ${binary##*/})"
            changed=1
            found=1
            break
          fi
        done
        if [ "$found" = 0 ]; then
          echo "fixup: ${binary##*/} needs $needed, which is not a system DLL and is in no declared input" >&2
          echo "fixup: searched: $dllSearchDirs" >&2
          exit 1
        fi
      done
    done
  done
}

bundleDlls "$prefix/bin"
