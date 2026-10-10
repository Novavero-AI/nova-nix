# Parity fixture: a derivation whose builder script is a builtins.toFile
# output that references another toFile output and this directory copied to
# the store.  The script's store path hashes its contents and both
# references (sorted: one text path, one source path whose NAR mixes
# executable and plain files), and the script reaches the drvPath through
# inputSrcs.
let
  greeting = builtins.toFile "parity-greeting" "hello\n";
  script = builtins.toFile "parity-builder.sh" ''
    cat ${greeting} > $out
    ls ${./.} >> $out
  '';
in
derivation {
  name = "parity-tofile";
  system = "x86_64-linux";
  builder = "/bin/sh";
  args = [ script ];
}
