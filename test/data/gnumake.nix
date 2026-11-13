# pkgs/windows' GNU make, as the Bootstrap (Windows) CI jobs build it:
# version, a script that runs make --version, as nixpkgs' get-version test
# does; and pattern-rule, in which make builds greeting.txt from
# greeting.in through a pattern rule whose recipe is cmd.exe's copy, which
# make writes to a batch file and runs, having no sh.exe on PATH.
let
  inherit (import ../../pkgs/windows) stage0-pe32 gnumake;
  inherit (stage0-pe32) writeText;
  kaem = import ./check.nix stage0-pe32.kaem;

  # .RECIPEPREFIX keeps the recipe's tab out of this file.
  makefile = writeText "pattern-rule.mk" ''
    .RECIPEPREFIX := >
    %.txt: %.in
    > copy $< $@
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
