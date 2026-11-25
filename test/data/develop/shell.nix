# A derivation develop captures without nixpkgs, for the CLI tests on Linux
# and macOS: its builder is the host's bash, and its stdenv is a directory
# whose setup puts its build input's bin on PATH, defines a phase, and
# appends a line to the file marker names each time a capture runs it.
# With structured, it has structured attributes; with arrayOutput, the
# setup makes out an array, which no output's variable may be.
{
  marker,
  builder ? "/bin/bash",
  structured ? false,
  arrayOutput ? false,
}:
let
  tool = derivation {
    name = "develop-tool";
    system = builtins.currentSystem;
    builder = "/bin/sh";
    args = [
      "-c"
      "mkdir -p $out/bin && printf '#!/bin/sh\\necho tool ran\\n' > $out/bin/develop-tool && chmod +x $out/bin/develop-tool"
    ];
    # A build's own PATH names no directory, so the host's tools are named.
    PATH = "/usr/bin:/bin";
  };
in
derivation (
  {
    name = "develop-fixture";
    system = builtins.currentSystem;
    inherit builder marker;
    # get-env.sh writes the environment to the outputs this names.
    outputs = [ "out" ];
    stdenv = ./stdenv;
    buildInputs = [ tool ];
    greeting = "it's \"quoted\"\nand on two lines";
  }
  // (if structured then { __structuredAttrs = true; } else { })
  // (if arrayOutput then { arrayOutput = "1"; } else { })
)
