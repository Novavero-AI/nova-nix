# The seed ladder's rungs: the attributes of ./default.nix that CI's Mes and
# Bootstrap legs build, each leg's in the order its steps build them.
#
# A push to main publishes what the Bootstrap leg built to the public cache,
# and a pull request's legs in substitute mode (Ladder (mode) in
# .github/workflows/ci.yml) take each rung from there:
# .github/cache/substitute-rungs.sh realises a leg's list with the cache as a
# substituter, and fails the leg when a rung the cache holds was built all
# the same.  The test suite checks each list against the builds its leg's
# steps name in the workflow.
let
  mes = [
    "stage0-pe32.kaem"
    "stage0-pe32.mescc-tools"
    "stage0-pe32.mescc-tools-extra"
    "mes.nyacc"
    "mes.srcPost"
    "mes.libs"
    "mes.compiler"
    "mes.libc"
  ];
in
{
  inherit mes;

  # The Bootstrap leg builds the Mes leg's rungs first, on a runner of its
  # own.
  bootstrap = mes ++ [
    "tinycc.boot-mes.compiler"
    "tinycc.boot-mes.libs"
    "tinycc.boot0.libs"
    "tinycc.boot1.libs"
    "tinycc.boot2.libs"
    "tinycc.boot3.libs"
    "tinycc.bootstrappable.libs"
    "tinycc-mes.prev.libs"
    "tinycc-mes.libs"
    "tinycc-native.prev.libs"
    "tinycc-native.libs"
    "tinycc-native.prev.compiler"
    "tinycc-native.compiler"
    "gnumake"
    "busybox-w32"
    "seedStdenv.applets"
    "binutils"
    "mingw-w64-headers"
    "mingw-w64-dirent"
    "gcc-core"
  ];
}
