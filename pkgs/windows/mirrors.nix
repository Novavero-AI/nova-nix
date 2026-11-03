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

  # GNU Savannah
  savannah = [
    # Mirrors from https://download-mirror.savannah.gnu.org/releases/00_MIRRORS.html
    "https://mirror.easyname.at/nongnu/"
    "https://savannah.c3sl.ufpr.br/"
    "https://mirror.csclub.uwaterloo.ca/nongnu/"
    "https://mirror.cedia.org.ec/nongnu/"
    "https://ftp.igh.cnrs.fr/pub/nongnu/"
    "https://mirror6.layerjet.com/nongnu"
    "https://mirror.netcologne.de/savannah/"
    "https://ftp.cc.uoc.gr/mirrors/nongnu.org/"
    "https://nongnu.uib.no/"
    "https://ftp.acc.umu.se/mirror/gnu.org/savannah/"
    "http://mirror2.klaus-uwe.me/nongnu/"
    "http://mirrors.fe.up.pt/pub/nongnu/"
    "http://ftp.twaren.net/Unix/NonGNU/"
    "http://savannah-nongnu-org.ip-connect.vn.ua/"
    "http://www.mirrorservice.org/sites/download.savannah.gnu.org/releases/"
    "http://gnu.mirrors.pair.com/savannah/savannah/"
    "ftp://mirror.easyname.at/nongnu/"
    "ftp://mirror2.klaus-uwe.me/nongnu/"
    "ftp://mirror.csclub.uwaterloo.ca/nongnu/"
    "ftp://ftp.igh.cnrs.fr/pub/nongnu/"
    "ftp://mirror.netcologne.de/savannah/"
    "ftp://nongnu.uib.no/pub/nongnu/"
    "ftp://mirrors.fe.up.pt/pub/nongnu/"
    "ftp://ftp.twaren.net/Unix/NonGNU/"
    "ftp://savannah-nongnu-org.ip-connect.vn.ua/mirror/savannah.nongnu.org/"
    "ftp://ftp.mirrorservice.org/sites/download.savannah.gnu.org/releases/"
  ];
}
