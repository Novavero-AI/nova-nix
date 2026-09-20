# Nova's package fetcher. Single-URL calls retain the bundled upstream
# interface; the mirror extension is a flat fixed-output fetch using either
# sha256 or an SRI hash. Keep <nix/fetchurl.nix> unchanged for drvPath parity.
args:
if !(args ? urls) then
  (import <nix/fetchurl.nix>) args
else
  (
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
        inherit name urls;
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
      }
  ) args
