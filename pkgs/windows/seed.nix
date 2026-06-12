# Stage 0 of the native Windows stdenv: the MinGW-w64 toolchain seed.
#
# The bootstrap trust anchor - pre-built MSYS2 packages, pinned by sha256,
# fetched with builtin:fetchurl and extracted with builtin:unpack into one
# merged toolchain root (MSYS2 packages share the mingw64/ prefix).  This is
# the Windows equivalent of nixpkgs' bootstrapTools tarball: everything past
# this point is built from source by tools that live in the store.
#
# The pin set is the runtime closure of mingw-w64-x86_64-gcc, resolved and
# hash-verified against repo.msys2.org on 2026-06-10.  MSYS2 prunes old
# package versions from its repo, so these URLs rot upstream; the durable
# source is a mirror of the tarballs in our own binary cache.
let
  fetchurl = import <nix/fetchurl.nix>;

  mirror = "https://repo.msys2.org/mingw/mingw64";

  fetchMsys2 =
    { file, sha256 }:
    fetchurl {
      url = "${mirror}/${file}";
      inherit sha256;
    };

  # gcc's full runtime closure: gcc -> binutils, crt, gcc-libs, gmp, headers,
  # isl, mpc, mpfr, windows-default-manifest, winpthreads, zlib, zstd;
  # binutils -> gettext-runtime, libwinpthread; gcc-libs -> tzdata;
  # gettext-runtime -> libiconv.  (cc-libs is virtual, provided by gcc-libs.)
  packages = [
    {
      file = "mingw-w64-x86_64-gcc-16.1.0-5-any.pkg.tar.zst";
      sha256 = "10f7c55275f7fbe7924209d61c368a1a6fcf775cffbdeac2eef1f5ccacdd35cd";
    }
    {
      file = "mingw-w64-x86_64-binutils-2.46-3-any.pkg.tar.zst";
      sha256 = "c2dc93c7a403574dce6c4b68584a12a95251e1088424bf3c43bc5130808fc626";
    }
    {
      file = "mingw-w64-x86_64-gcc-libs-16.1.0-5-any.pkg.tar.zst";
      sha256 = "aa560f5438c35b71c3e7b24fd5becbca028f70c5b4d1f1697a86ff80fec947da";
    }
    {
      file = "mingw-w64-x86_64-crt-14.0.0.r59.g93753750c-1-any.pkg.tar.zst";
      sha256 = "74eb6daf7909f4418e3580ba1761b2ecd54d24def12270fdf4a8f3ee5c5c3663";
    }
    {
      file = "mingw-w64-x86_64-headers-14.0.0.r59.g93753750c-1-any.pkg.tar.zst";
      sha256 = "da4d28cbf691f9a1a1735004c03f28b5b667212bb829a186ddbfbcb68e12e549";
    }
    {
      file = "mingw-w64-x86_64-gmp-6.3.0-2-any.pkg.tar.zst";
      sha256 = "8924433974c4add46cb46ea4f6ef283b5c5139d3f552375115b5580f855015cc";
    }
    {
      file = "mingw-w64-x86_64-isl-0.27-1-any.pkg.tar.zst";
      sha256 = "25a18fb1ed2f580a96cb5ea6b7baa34cc385154ee5ee711175337584cc763a97";
    }
    {
      file = "mingw-w64-x86_64-mpc-1.4.1-1-any.pkg.tar.zst";
      sha256 = "ce024a90d59c8a591d2c88ef94a386c6367750bf9edc6a944e80815ec5d93344";
    }
    {
      file = "mingw-w64-x86_64-mpfr-4.2.2-3-any.pkg.tar.zst";
      sha256 = "9ecbc05f1f855bc656a8f111d367f61fbd90dbbfcf469ba74d6d5dd1ec07a542";
    }
    {
      file = "mingw-w64-x86_64-windows-default-manifest-6.4-4-any.pkg.tar.zst";
      sha256 = "716b48ad9fe4a9c88c3a323279224943b79a04152bce66797debc83491bb91fa";
    }
    {
      file = "mingw-w64-x86_64-winpthreads-14.0.0.r59.g93753750c-1-any.pkg.tar.zst";
      sha256 = "c27ee207be6633c78f3fe83c095cfa534bea6da0b88fe8f7c79a799cfdadb06d";
    }
    {
      file = "mingw-w64-x86_64-libwinpthread-14.0.0.r59.g93753750c-1-any.pkg.tar.zst";
      sha256 = "425567f0c88e660a73c9d5bb89f79e53c22d58e6ee8249480771732fbec2c6b7";
    }
    {
      file = "mingw-w64-x86_64-zlib-1.3.2-2-any.pkg.tar.zst";
      sha256 = "9e75842a070ba648e986e12424e1c92c9d7d77200e85f6a34eeb600819f2e694";
    }
    {
      file = "mingw-w64-x86_64-zstd-1.5.7-2-any.pkg.tar.zst";
      sha256 = "1add6705b344664f6aca108c85f79ab5bdd9e1162662bb06a4cf40a34f6e0907";
    }
    {
      file = "mingw-w64-x86_64-gettext-runtime-1.0-1-any.pkg.tar.zst";
      sha256 = "be68d7f260633284b910c588c6d82ee304a81c8817a686d2cd9df83f872c27af";
    }
    {
      file = "mingw-w64-x86_64-tzdata-2026b-1-any.pkg.tar.zst";
      sha256 = "e197c0f945d0461f704b46463ec7f8455d376431e8b9b489233d721275033360";
    }
    {
      file = "mingw-w64-x86_64-libiconv-1.19-1-any.pkg.tar.zst";
      sha256 = "21e334d0911f25de75d3e18e0697648bcecfa9658256d600cad0827d719c2f35";
    }
  ];
in
derivation {
  name = "mingw-w64-seed";
  system = "builtin";
  builder = "builtin:unpack";
  srcs = map fetchMsys2 packages;
}
