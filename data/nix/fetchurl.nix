# nova-nix's implementation of the <nix/fetchurl.nix> interface.
#
# Nix exposes a built-in fetcher expression under the search-path entry
# <nix/fetchurl.nix>; nixpkgs' bootstrap imports it directly, so nova-nix
# must provide one with the exact same interface and derivation output.
# Every attribute below feeds the .drv ATerm, so the produced derivation
# (and therefore its store path) byte-matches upstream Nix — this is
# enforced by the parity CI job.  The argument names, default chains, and
# derivation attributes are all fixed by that contract; only fetch one
# thing at a time and let the fixed-output hash do the verifying.
{
  system ? "", # ignored; kept for interface compatibility
  url,
  hash ? "", # an SRI hash

  # Pre-SRI hash arguments, still accepted everywhere in nixpkgs.
  md5 ? "",
  sha1 ? "",
  sha256 ? "",
  sha512 ? "",
  outputHash ?
    if hash != "" then
      hash
    else if sha512 != "" then
      sha512
    else if sha1 != "" then
      sha1
    else if md5 != "" then
      md5
    else
      sha256,
  outputHashAlgo ?
    if hash != "" then
      ""
    else if sha512 != "" then
      "sha512"
    else if sha1 != "" then
      "sha1"
    else if md5 != "" then
      "md5"
    else
      "sha256",

  executable ? false,
  unpack ? false,
  name ? baseNameOf (toString url),
  impure ? false,
}:

derivation (
  {
    # Handled in-process by the Builder, not spawned as a subprocess
    # (see Nix.Builder.runBuiltinFetchurl).
    builder = "builtin:fetchurl";

    # An unpacked or executable output is a tree, so its fixed-output
    # hash must be taken over the NAR serialisation rather than the
    # flat file bytes.
    outputHashMode = if unpack || executable then "recursive" else "flat";

    inherit
      name
      url
      executable
      unpack
      ;

    system = "builtin";

    # The fetch is cheaper than copying the result between machines.
    preferLocalBuild = true;

    # Proxy configuration may legitimately vary between hosts without
    # changing what gets fetched, so it is allowed through the env.
    impureEnvVars = [
      "http_proxy"
      "https_proxy"
      "ftp_proxy"
      "all_proxy"
      "no_proxy"
    ];

    # Tooling that prefetches (nix-prefetch-url and friends) reads the
    # candidate URLs from this attribute.
    urls = [ url ];
  }
  // (if impure then { __impure = true; } else { inherit outputHashAlgo outputHash; })
)
