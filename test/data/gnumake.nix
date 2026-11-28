# pkgs/windows' GNU make, as the TinyCC (Windows) CI jobs build it:
# version, a script that runs make --version, as nixpkgs' get-version test
# does; and pattern-rule, in which make builds greeting.txt from
# greeting.in through a pattern rule whose recipe runs mescc-tools-extra's
# cp, by its path, on the rule's $< and $@.  With no sh.exe on PATH, make
# takes cmd.exe for its shell, and starts a recipe line itself, without
# the shell, when the line holds none of cmd.exe's operators and does not
# begin with one of its own commands; so the recipe is a program from the
# store, and no program from outside it runs.
let
  inherit (import ../../pkgs/windows) stage0-pe32 gnumake;
  inherit (stage0-pe32) writeText mescc-tools-extra;
  kaem = import ./check.nix stage0-pe32.kaem;

  # .RECIPEPREFIX keeps the recipe's tab out of this file.
  makefile = writeText "pattern-rule.mk" ''
    .RECIPEPREFIX := >
    %.txt: %.in
    > ${mescc-tools-extra}/bin/cp.exe $< $@
  '';

  greeting = writeText "greeting.in" ''
    Hello from make
  '';
in
{
  version = kaem.runCommand "gnumake-get-version" { } ''
    ${gnumake}/bin/make.exe --version
    mkdir ''${out}
  '';

  pattern-rule = kaem.runCommand "gnumake-pattern-rule" { } ''
    cp ${greeting} greeting.in
    ${gnumake}/bin/make.exe -f ${makefile} greeting.txt
    cp greeting.txt ''${out}
  '';
}
