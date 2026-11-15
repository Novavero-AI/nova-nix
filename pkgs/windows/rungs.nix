# The seed ladder's rungs: the attributes of ./default.nix that CI's ladder
# jobs build, one list for each stage, in the order its job's steps build
# them.
#
# CI builds the ladder on each Windows leg as a chain of jobs, one for each
# stage, each starting from the store paths the job before it built, which
# it takes from that job's handoff (.github/cache/handoff-serve.sh and
# .github/workflows/ladder.yml).  A push to main publishes what each stage's
# x64 job built to the public cache, and the jobs of a pull request or a
# later push, in substitute mode (Ladder (mode) in
# .github/workflows/ci.yml), take each rung from there.
# .github/cache/substitute-rungs.sh realises a stage's list with a cache as
# a substituter, and fails the job when a rung the cache holds was built
# all the same: each job's own list from the public cache in substitute
# mode, and the stage before's from its handoff.  The test suite checks
# each list against the builds its job's steps name in the workflow.
{
  # The stage0 tools and GNU Mes, the Mes stage.
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

  # TinyCC's rounds and the seed stdenv's tools they compile, the TinyCC
  # stage.
  tinycc = [
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
    "tinycc.boot0.compiler"
    "tinycc.boot1.compiler"
    "tinycc.boot2.compiler"
    "tinycc.boot3.compiler"
    "tinycc.bootstrappable.compiler"
    "tinycc-mes.prev.compiler"
    "tinycc-mes.compiler"
    "gnumake"
    "busybox-w32"
    "seedStdenv.applets"
  ];

  # binutils, MinGW-w64 and GCC 4.6.4's C compiler, the GCC 4.6.4 stage.
  gcc46 = [
    "binutils"
    "mingw-w64-headers"
    "mingw-w64-dirent"
    "gcc-core"
    "mingw-w64-crt"
    "gcc46"
  ];

  # GCC 4.6.4 with C++, the GCC 4.6.4 C++ stage.
  gcc46-cxx = [ "gcc46-cxx" ];

  # GCC 10.4.0 with C and C++, and the current MinGW-w64 it compiles, the
  # GCC 10 stage.
  gcc10 = [
    "gcc10"
    "mingw-w64-headers-latest"
    "mingw-w64-crt-latest"
  ];

  # GCC 16.2.0 with C and C++, the GCC 16 stage.
  gcc-latest = [ "gcc-latest" ];

  # binutils for x86_64-w64-mingw32, the x86_64 stage.
  x86_64 = [ "binutils-x86_64" ];
}
