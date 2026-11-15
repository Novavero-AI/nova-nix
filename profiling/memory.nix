# The memory suite's walk: walk.nix over the units SCOREBOARD_BUDGET lets
# it begin, valued as a function whose closure holds the package set the
# walk visited.  Both evaluators print a function as <LAMBDA> without
# forcing anything in it, so the package set, with every attribute the
# walk forced in it, is still held when evaluation returns and nova-nix's
# census marks from the result, while the walk's own state per attribute
# is not.
let
  walk = import ./walk.nix;
in
builtins.seq walk.complete (_: walk.pkgs)
