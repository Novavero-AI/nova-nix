# pkgs/windows' bash, as the Bash (Windows) CI jobs check it.  checks is
# a build whose builder is bash itself, running bash-checks/checks.sh,
# which writes what it checked to $out for CI to compare with
# bash-checks-answer.txt; probe, which the stdenv builds, is the program
# it runs, and scripts holds the #! script it runs.  Each forbids
# substitutes, as the checks kaem builds do (see check.nix), so CI builds
# them on every run.
let
  pkgs = import ../../pkgs/windows;
  inherit (pkgs) bash busybox-w32;

  probe = pkgs.stdenv.mkDerivation {
    name = "bash-probe";
    src = ./bash-checks;
    dontConfigure = true;
    allowSubstitutes = false;
    buildPhase = "$CC -O2 -o probe.exe probe.c";
    installPhase = ''
      mkdir -p "$out/bin"
      cp probe.exe "$out/bin/probe.exe"
    '';
  };

  # greet: a script whose #! line names bash by its store path with no
  # .exe, as a script nixpkgs patches names its interpreter.
  scripts = derivation {
    name = "bash-scripts";
    system = "x86_64-windows";
    builder = "${busybox-w32}/bin/busybox.exe";
    args = [
      "sh"
      "-e"
      (builtins.toFile "bash-scripts.sh" ''
        mkdir -p "$out/bin"
        printf '#!%s\necho hello from a script\nexit 4\n' "$interpreter" > "$out/bin/greet"
      '')
    ];
    interpreter = "${bash}/bin/bash";
    allowSubstitutes = false;
  };
in
{
  inherit probe scripts;

  checks = derivation {
    name = "bash-checks";
    system = "x86_64-windows";
    builder = "${bash}/bin/bash.exe";
    args = [ ./bash-checks/checks.sh ];
    inherit probe scripts;
    allowSubstitutes = false;
  };
}
