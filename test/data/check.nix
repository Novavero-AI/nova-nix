# kaem for the checks in this directory, the derivations CI builds to see a
# builder run: its runCommand makes each one forbid substitutes.  CI
# publishes what it builds on main, and a check whose output a cache held
# would be downloaded where it is meant to run, so its script would print
# and test nothing.
kaem:
kaem
// {
  runCommand = name: env: kaem.runCommand name (env // { allowSubstitutes = false; });
}
