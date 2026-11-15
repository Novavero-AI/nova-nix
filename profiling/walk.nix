# The package-set walk behind bench.sh's scoreboard (scoreboard.nix) and its
# memory suite (memory.nix).  nova-nix and upstream Nix both evaluate it
# unchanged, so the two visit the same attributes wherever they agree on
# the values that decide the walk.
#
# It visits what `nix-env -qa` visits (getDerivations in upstream's
# get-drvs.cc at 2.24.9): the top-level set of <nixpkgs> for x86_64-linux
# with no user config or overlays, as the parity job binds it; the
# attributes whose names match [A-Za-z_][A-Za-z0-9_+-]*, in sorted order;
# and, depth first, every set among them whose recurseForDerivations is
# true.  A value that throws or fails an assertion while it is forced,
# checked for being a derivation, or asked its name is not a package, as
# nix-env's ignoreAssertionFailures makes it, and a derivation whose name
# is not a string without context fails as it does in nix-env.  For each
# derivation the walk forces its drvPath under tryEval.  Unlike nix-env it
# does not drop a derivation already reached through another attribute:
# nix-env keys that on heap identity, which no expression can observe, and
# the scoreboard counts attributes.
#
# Each attribute visited is a unit, reported through builtins.trace, which
# both evaluators write to stderr as a line of its own while evaluation
# runs:
#
#   trace: nova-nix-scoreboard begin PATH
#   trace: nova-nix-scoreboard drv PATH DRVPATH
#   trace: nova-nix-scoreboard fail PATH       (drvPath throws or asserts)
#   trace: nova-nix-scoreboard skip PATH       (neither a package nor a set
#                                               to descend into)
#   trace: nova-nix-scoreboard recurse PATH    (its own units follow)
#
# A failure tryEval cannot catch ends the process after a begin line that
# nothing settles, and that line names the unit it ended.  bench.sh records
# the unit and resumes the walk after it in a new process.  PATH needs no
# quoting anywhere: its components match the pattern above and are joined
# with dots.
#
# Inputs, from the environment:
#
#   SCOREBOARD_SETTLED  a JSON object mapping the path of each unit an
#                       earlier process settled to its kind.  The walk
#                       passes over those units without forcing them,
#                       except that it still descends into a recursed set.
#   SCOREBOARD_BUDGET   the number of units this process may begin; unset
#                       or empty for no bound.
#
# The value is a set: `complete` is true when the walk is complete and false
# when the budget left units for another process, and `pkgs` is the package
# set it walks.
let
  marker = "nova-nix-scoreboard";

  settledFile = builtins.getEnv "SCOREBOARD_SETTLED";
  settled = if settledFile == "" then { } else builtins.fromJSON (builtins.readFile settledFile);

  budgetText = builtins.getEnv "SCOREBOARD_BUDGET";
  budget = if budgetText == "" then null else builtins.fromJSON budgetText;

  pkgs = import <nixpkgs> {
    system = "x86_64-linux";
    config = { };
    overlays = [ ];
  };

  isPathComponent = name: builtins.match "[A-Za-z_][A-Za-z0-9_+-]*" name != null;

  report = line: builtins.trace "${marker} ${line}" null;

  # EvalState::isDerivation: a set whose type is the string "derivation".
  isDerivation = value: builtins.isAttrs value && (value.type or null) == "derivation";

  # queryName's forceStringNoCtx: a name that is not a string, or carries
  # context, is an error tryEval cannot catch, as upstream's TypeError is.
  isPlainName =
    name:
    builtins.isString name && !builtins.hasContext name
    || abort "the name of a derivation must be a string without context";

  # getDerivation's catch: forcing the value, isDerivation and queryName,
  # and nothing after them, so recurseForDerivations is forced outside it
  # as upstream's forceBool is.
  probe = value: builtins.tryEval (if isDerivation value then isPlainName value.name else false);

  # A unit's kind and the rest of its report line.
  classify =
    value:
    let
      probed = probe value;
      drvPath = builtins.tryEval value.drvPath;
    in
    if !probed.success then
      {
        kind = "skip";
        detail = "";
      }
    else if probed.value then
      if drvPath.success then
        {
          kind = "drv";
          detail = " ${drvPath.value}";
        }
      else
        {
          kind = "fail";
          detail = "";
        }
    else if builtins.isAttrs value && (value.recurseForDerivations or false) then
      {
        kind = "recurse";
        detail = "";
      }
    else
      {
        kind = "skip";
        detail = "";
      };

  # The state is the number of units this process has begun.  Once the
  # budget turns a unit away the state is budget + 1 and stays there, so the
  # walk ends past its budget exactly when it left units unvisited.
  walkSet =
    begun: prefix: set:
    builtins.foldl' (visit prefix set) begun (builtins.filter isPathComponent (builtins.attrNames set));

  visit =
    prefix: set: begun: name:
    let
      path = if prefix == "" then name else "${prefix}.${name}";
      value = set.${name};
      earlier = settled.${path} or null;
    in
    if earlier == "recurse" then
      walkSet begun path value
    else if earlier != null then
      begun
    else if budget != null && begun >= budget then
      budget + 1
    else
      builtins.seq (report "begin ${path}") (
        let
          unit = classify value;
        in
        builtins.seq (report "${unit.kind} ${path}${unit.detail}") (
          if unit.kind == "recurse" then walkSet (begun + 1) path value else begun + 1
        )
      );

  begun = walkSet 0 "" pkgs;
in
{
  complete = builtins.seq begun (budget == null || begun <= budget);
  inherit pkgs;
}
