# Stage 1, step 1: the MSYS2 userland seed - a POSIX shell and the build tools
# the stdenv's setup.sh runs through.  Companion to seed.nix (the compiler):
# seed.nix is the mingw64 toolchain, this is the msys runtime + shell.
#
# Baby step: start with just the bash closure (msys2-runtime + bash + its
# readline/ncurses/locale deps) to prove bash.exe runs from the store, then
# grow the package set to coreutils/make/sed/grep/gawk/tar/gzip/xz.
#
# These are the msys/x86_64 packages, which (unlike the mingw64 ones) all link
# the msys2-runtime DLL (msys-2.0.dll, the Cygwin-fork POSIX layer).  They
# unpack under usr/, so bash.exe and msys-2.0.dll land side by side in usr/bin
# and resolve each other via same-directory DLL lookup.
let
  fetchurl = import <nix/fetchurl.nix>;

  mirror = "https://repo.msys2.org/msys/x86_64";

  fetchMsys2 =
    { file, sha256 }:
    fetchurl {
      url = "${mirror}/${file}";
      # Store path names cannot carry '~' (parseStorePath enforces
      # upstream's name charset, and upstream's own checkName rejects it
      # the same way), but MSYS2 epoch-marked filenames contain one -
      # grep's "1~3.0".  Sanitize the name; the URL keeps the real
      # filename.
      name = builtins.replaceStrings [ "~" ] [ "-" ] file;
      inherit sha256;
    };

  packages = [
    {
      file = "msys2-runtime-3.6.9-2-x86_64.pkg.tar.zst";
      sha256 = "20f39ad6d0fd2aae93ca84c2e9efbe567d3d7f5d465dc0810b700d7cbac0c3a4";
    }
    {
      file = "bash-5.3.015-1-x86_64.pkg.tar.zst";
      sha256 = "ecd097c0b85938b3167abd7e839a6113b25c8b581b14954aad02e1f8ce556c8e";
    }
    {
      file = "libreadline-8.3.003-1-x86_64.pkg.tar.zst";
      sha256 = "f78e11a496010305fb80b6804518d5c1257794fc87bc8caaf8c4a65d7a983dec";
    }
    {
      file = "ncurses-6.6-2-x86_64.pkg.tar.zst";
      sha256 = "828c92f09b17f00d3b49362fcfa4affa58e5e50e238f290ea36eb7a690da3acb";
    }
    {
      file = "libiconv-1.19-1-x86_64.pkg.tar.zst";
      sha256 = "0fa55ea2a6ccf97cf8c58b24b2615815e15e16e6e4e888091c263c2c83c5313d";
    }
    {
      file = "gettext-0.22.5-1-x86_64.pkg.tar.zst";
      sha256 = "b31ab065c3538ecb1fb6ab48ab2b3049243d6d9e585331c882f38326eefc1831";
    }
    # libintl: the runtime i18n DLL (msys-intl-8.dll) every tool links for
    # translated messages.  MSYS2 splits this from the gettext *tools* package
    # above, so it must be pinned separately.
    {
      file = "libintl-0.22.5-1-x86_64.pkg.tar.zst";
      sha256 = "336d66b9d95cf9c1804958f8e260762a3e83bf158ed5981f783bc772a31073cf";
    }
    # coreutils (the GNU userland: mkdir, cp, install, ls, ...) + its gmp dep.
    {
      file = "coreutils-8.32-5-x86_64.pkg.tar.zst";
      sha256 = "62dfee1c39fd15f99c39802b35e82643bc14fffc16d6c76d4001caa385ec77e3";
    }
    {
      file = "gmp-6.3.0-2-x86_64.pkg.tar.zst";
      sha256 = "f84536138e618d47a1df2da83916cfebbd449f159ffad52f994a71060a41cf34";
    }
    # Build tools: GNU make (buildPhase) + GNU tar/gzip/xz (unpackPhase).
    # GNU tar shells out to gzip/xz for compressed tarballs, so no libarchive
    # cluster is needed.  liblzma is the runtime DLL the xz binary links.
    {
      file = "make-4.4.1-3-x86_64.pkg.tar.zst";
      sha256 = "af0bdba17f06fe037f0194069adaa31a8fe45f1a11381501896aea1fae37bd5d";
    }
    {
      file = "tar-1.35-3-x86_64.pkg.tar.zst";
      sha256 = "71983bed200c4026cdec157837d989728d3de65e8cf644e03876df41a0f1b7c4";
    }
    {
      file = "gzip-1.14-2-x86_64.pkg.tar.zst";
      sha256 = "d63e8d98ff030c88663cc64cf8ebd04203fba17365807477efa06f717d7daedc";
    }
    {
      file = "xz-5.8.3-1-x86_64.pkg.tar.zst";
      sha256 = "05a0cba55b1d302861e8152c7aa08fb35819af625d79c75bd8502b5e105cd657";
    }
    {
      file = "liblzma-5.8.3-1-x86_64.pkg.tar.zst";
      sha256 = "63e778c1724dfa165394ca0c021ee399cffb7be7a1c8ff43ec2a1341aaf5d8fa";
    }
    {
      file = "zlib-1.3.2-1-x86_64.pkg.tar.zst";
      sha256 = "a04aa79996c57f0db936be66cf94326d7e67e9cd8dbffe4cf6e97693d0a1d9ef";
    }
    # configure-script tools: sed, grep, gawk, find - what ./configure leans on.
    # grep links pcre *1* (msys-pcre-1.dll), not pcre2.  gawk needs mpfr, which
    # itself pulls libgcc_s from gcc-libs (msys-gcc_s-seh-1.dll) - a transitive
    # dep only the PE import table reveals.  (grep's 1~3.0 version has an epoch
    # marker in the filename - fine in a URL, invalid in a store path name;
    # fetchMsys2 sanitizes it.)
    {
      file = "sed-4.9-1-x86_64.pkg.tar.zst";
      sha256 = "3748af28f69e946ec5a42e6670c9bbf6da7352dc93baaa537f69e99c5483b9fc";
    }
    {
      file = "grep-1~3.0-7-x86_64.pkg.tar.zst";
      sha256 = "2909b3bbef8d33d66980339fb7bd2d3d9145bae0f04191e6671df39868aea427";
    }
    {
      file = "gawk-5.4.0-1-x86_64.pkg.tar.zst";
      sha256 = "618c817ac25bf81d7541d7284cff2ab6043fcf966a1ffa5638ac698a4d985076";
    }
    {
      file = "findutils-4.10.0-2-x86_64.pkg.tar.zst";
      sha256 = "ed3ce5a8684156dd90c98e50e34cb988867ad1462efc3c6f58170ca244d7c161";
    }
    {
      file = "libpcre-8.45-5-x86_64.pkg.tar.zst";
      sha256 = "b4dc2eb7795cbacaa683101d720fa67f536262497a1d89934a58f5dab8d17d17";
    }
    {
      file = "mpfr-4.2.2-1-x86_64.pkg.tar.zst";
      sha256 = "8f1b6c17c53e00494a488e03dfdf8fc742027ed03a5bd7b1b3a6a33f595a7498";
    }
    {
      file = "gcc-libs-15.2.0-1-x86_64.pkg.tar.zst";
      sha256 = "d77e7228e596283cb5fbdcba67bbf9c3abd4bc3e21fc225a9752c0c8cdc8b91d";
    }
  ];
in
derivation {
  name = "msys-seed";
  system = "builtin";
  builder = "builtin:unpack";
  srcs = map fetchMsys2 packages;
}
