# Parity fixture: a derivation whose source is a pinned builtins.fetchGit
# checkout, coerced through the result's outPath, with every other attribute
# the fetch returns in its environment.  One drvPath then covers the fetched
# tree's store path and the rev, shortRev, revCount, lastModified,
# lastModifiedDate, narHash and submodules values upstream computes.
#
# The rev is the tip of GitHub's own example repository, committed in 2012;
# fetching it is the one network access evaluation makes in this job.
let
  src = builtins.fetchGit {
    url = "https://github.com/octocat/Hello-World";
    rev = "7fd1a60b01f91b314f59955a4e4d4e80d8edf11d";
  };
in
derivation {
  name = "parity-fetchgit";
  system = "x86_64-linux";
  builder = "/bin/sh";
  args = [ "-c" "cp -r $src $out" ];
  inherit src;
  inherit (src) rev shortRev revCount lastModified lastModifiedDate;
  inherit (src) narHash submodules;
}
