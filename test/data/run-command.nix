# stage0-pe32's writeText and kaem.runCommand, as the Stage0 (Windows) CI jobs
# build them: hello, a file writeText writes; copies, a script that calls
# mkdir, cp and catm by name on hello's store path; and fails, a script whose
# first command fails and whose second would make the output.
let
  inherit (import ../../pkgs/windows) stage0-pe32;
  inherit (stage0-pe32) kaem writeText;

  hello = writeText "hello.txt" ''
    stage0-pe32 runs a kaem script
  '';
in
{
  inherit hello;

  copies = kaem.runCommand "run-command-copies" { } ''
    mkdir -p ''${out}/a/b
    cp ${hello} ''${out}/a/b/hello.txt
    catm ''${out}/twice.txt ${hello} ${hello}
  '';

  fails = kaem.runCommand "run-command-fails" { } ''
    cp missing.txt copied.txt
    mkdir ''${out}
  '';
}
