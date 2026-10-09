# Nova's package fetcher: the bundled <nix/fetchurl.nix> interface plus the
# two extensions nixpkgs' fetchurl adds to it, an ordered `urls` list and
# the mirror://<site>/<path> scheme.  A plain single-URL call still goes to
# the bundled fetcher untouched, so its drvPath keeps parity with upstream.
#
# nixpkgs expands mirror:// in its fetchurl builder (pkgs/build-support/
# fetchurl/builder.sh) at build time, from a mirror list the derivation
# carries as a file.  builtin:fetchurl runs no shell, so the expansion
# happens here at eval and the expanded list is what the derivation records.
# The output path is fixed by the hash either way.
args:
let
  mirrors = import ./mirrors.nix;

  mirrorScheme = "mirror://";
  isMirror = url: builtins.substring 0 (builtins.stringLength mirrorScheme) url == mirrorScheme;

  # The HTTP client behind builtin:fetchurl does not speak ftp, which
  # nixpkgs' lists carry for curl.
  fetchable = url: builtins.match "https?://.*" url != null;

  # mirror://gnu/hello/hello-2.12.3.tar.gz is every host in mirrors.gnu, each
  # followed by hello/hello-2.12.3.tar.gz: the site ends at the first slash,
  # as nixpkgs splits it.  nixpkgs can only warn about an unknown site from
  # its build script, and expands a URL with no path to the bare hosts; at
  # eval both are errors in the recipe.
  expand =
    url:
    let
      parts = builtins.match "mirror://([^/]+)/(.*)" url;
      site = builtins.elemAt parts 0;
      path = builtins.elemAt parts 1;
    in
    if parts == null then
      if isMirror url then throw "fetchurl: malformed mirror URL '${url}'" else [ url ]
    else if !(mirrors ? ${site}) then
      throw "fetchurl: unknown mirror site '${site}' in '${url}'"
    else
      builtins.filter fetchable (map (host: host + path) mirrors.${site});

  fetchUrls =
    {
      urls,
      sha256 ? "",
      hash ? "",
      name ? baseNameOf (builtins.head urls),
    }:
    if
      !(builtins.isList urls)
      || urls == [ ]
      || !(builtins.all (url: builtins.isString url && builtins.match "[^[:space:]]+" url != null) urls)
    then
      throw "fetchurl: urls must be a non-empty list of non-empty strings without ASCII whitespace"
    else if (sha256 == "") == (hash == "") then
      throw "fetchurl: provide exactly one of sha256 or hash"
    else
      derivation {
        inherit name;
        urls = builtins.concatMap expand urls;
        builder = "builtin:fetchurl";
        system = "builtin";
        outputHashMode = "flat";
        outputHashAlgo = if hash != "" then "" else "sha256";
        outputHash = if hash != "" then hash else sha256;
        preferLocalBuild = true;
        impureEnvVars = [
          "http_proxy"
          "https_proxy"
          "ftp_proxy"
          "all_proxy"
          "no_proxy"
        ];
      };
in
if args ? urls then
  fetchUrls args
else if args ? url && isMirror args.url then
  fetchUrls (builtins.removeAttrs args [ "url" ] // { urls = [ args.url ]; })
else
  (import <nix/fetchurl.nix>) args
