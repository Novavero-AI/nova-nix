# Hosts behind the mirror://<site>/<path> scheme, tried in this order.
#
# A verbatim copy of the sets this package set uses from nixpkgs'
# pkgs/build-support/fetchurl/mirrors.nix at commit
# 50ab793786d9de88ee30ec4e4c24fb4236fc2674, comments included.  Refresh by
# copying the set again from a newer nixpkgs and updating the commit here.
# builtin:fetchurl speaks HTTP only, so fetchurl.nix leaves an ftp:// host
# out of the expansion; the entry stays so the copy diffs cleanly.
{
  gnu = [
    # This one redirects to a (supposedly) nearby and (supposedly) up-to-date
    # mirror
    "https://ftpmirror.gnu.org/"

    "https://ftp.nluug.nl/pub/gnu/"
    "https://mirrors.kernel.org/gnu/"
    "https://mirror.ibcp.fr/pub/gnu/"
    "https://mirror.dogado.de/gnu/"
    "https://mirror.tochlab.net/pub/gnu/"

    # This one is the master repository, and thus it's always up-to-date
    "https://ftp.gnu.org/pub/gnu/"

    "ftp://ftp.funet.fi/pub/mirrors/ftp.gnu.org/gnu/"
  ];
}
