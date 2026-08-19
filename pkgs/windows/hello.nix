# A buildable pointer at the by-name hello package, for `nova-nix build
# pkgs\windows\hello.nix`: build takes one derivation-valued expression, and
# by-name/he/hello/package.nix is a function (it takes { stdenv, fetchurl }),
# so something has to call it. pkgs/windows/default.nix already does, as
# `hello`.
(import ./default.nix).hello
