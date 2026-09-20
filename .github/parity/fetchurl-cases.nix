# The mirror interface is a Nova extension; its evaluator and fixed-output
# identity still follow upstream Nix. Assertions must pass in both evaluators.
let
  fetchurl = import ./fetchurl.nix;
  upstream = import <nix/fetchurl.nix>;
  url = "https://example.org/source.tar.gz";
  urls = [ url "https://mirror.example.org/source.tar.gz" ];
  sha256 = "2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824";
  hash = "sha256-LPJNul+wow4m6DsqxbninhsWHlwfp0JecwQzYpOLmCQ=";
  original = upstream { inherit url sha256; };
  rejects = args: !(builtins.tryEval (fetchurl args).outPath).success;
in
{
  legacy = assert (fetchurl { inherit url sha256; }).drvPath == original.drvPath; true;
  sha256 = assert (fetchurl { inherit urls sha256; }).outPath == original.outPath; true;
  sri = assert (fetchurl { inherit urls hash; }).outPath == original.outPath; true;
  order = assert (fetchurl { inherit urls sha256; }).urls == urls; true;
  invalidUrls = assert builtins.all
    (invalid: rejects { inherit sha256; urls = invalid; })
    [ [ ] url [ "" ] [ url 1 ] [ "https://example.org/has space" ] [ "https://example.org/has\nnewline" ] ];
    true;
  invalidHashes = assert builtins.all rejects [
    { inherit urls; }
    { inherit urls sha256 hash; }
  ]; true;
  escapedUrl = assert (fetchurl {
    name = "escaped-url";
    urls = [ "https://example.org/has%20space" ];
    inherit sha256;
  }).outPath == (upstream {
    name = "escaped-url";
    url = "https://example.org/has%20space";
    inherit sha256;
  }).outPath; true;
}
