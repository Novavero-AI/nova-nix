# stage0-pe32's writeText and kaem.runCommand, as the Stage0 (Windows) CI jobs
# build them: hello, a file writeText writes; copies, a script that calls
# mkdir, cp and catm by name on hello's store path; fails, a script whose
# first command fails and whose second would make the output; and marks, a
# script whose chmod calls decide which of its files are executable.
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

  # Fixed-output, so the build registers its output only if the output's NAR
  # hash is this one: bin/hello executable and hello.txt not.  It is the hash
  # of the same tree on POSIX, where the bit is the mode, as
  # `nix-hash --type sha256 marks` prints it after
  #   mkdir -p marks/bin
  #   printf 'stage0-pe32 runs a kaem script\n' > marks/bin/hello
  #   cp marks/bin/hello marks/hello.txt
  #   chmod 555 marks/bin/hello
  #   chmod 644 marks/hello.txt
  # With neither file executable it would be
  # 0ba9a92fd86ccb894f2dd67415a1590d578d6b2c0bd5ca04ecb9120af2167af9.
  marks =
    kaem.runCommand "run-command-marks"
      {
        outputHashMode = "recursive";
        outputHashAlgo = "sha256";
        outputHash = "d7b2412a32f4ff4be9340e6bbd33e316c64fdf38623ba4e76ed8af570dfaee9a";
      }
      ''
        mkdir -p ''${out}/bin
        cp ${hello} ''${out}/bin/hello
        chmod 555 ''${out}/bin/hello
        cp ${hello} ''${out}/hello.txt
        chmod 755 ''${out}/hello.txt
        chmod 644 ''${out}/hello.txt
      '';
}
