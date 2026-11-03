# Everything stage0-pe32's chain reads: the seed, and four repositories at
# the commits stage0-pe32 pins.  Each repository value is the root of its
# tree in the store, so a step names a file as "${M2libc}/x86/x86_defs.M1",
# the path the .cmd reaches it by.
#
# A GitHub archive leaves submodules out, so stage0-pe32's tree has M2-Planet/,
# M2libc/ and mescc-tools/ as empty directories, and each is fetched at the
# commit stage0-pe32 records for it (`git submodule status`).  Moving the pin
# is the stage0-pe32 commit, those submodule commits, the sha256 of each
# archive and of the seed, the archive names the test suite's stage0-pe32
# group expects, and the link hashes the Stage0 (Windows) CI job compares
# against, which stage0-pe32's own CI prints for the new commit.
{ fetchurl }:
let
  stage0-pe32Rev = "4b5915ca4fd1c4483cbde8f3c256d67c9570b7b2";

  # GitHub's archive of one commit, fetched and unpacked.  The archive holds a
  # single directory, <repo>-<rev>, which is the value.  The sha256 is of the
  # archive's bytes, which GitHub generates from the commit.
  fromGitHub =
    {
      owner,
      repo,
      rev,
      sha256,
    }:
    let
      name = "${repo}-${builtins.substring 0 7 rev}";
      archive = fetchurl {
        # builtin:unpack chooses its decompressor by this suffix.
        name = "${name}.tar.gz";
        url = "https://github.com/${owner}/${repo}/archive/${rev}.tar.gz";
        inherit sha256;
      };
      tree = derivation {
        inherit name;
        system = "builtin";
        builder = "builtin:unpack";
        srcs = [ archive ];
      };
    in
    "${tree}/${repo}-${rev}";
in
{
  # The one binary the chain starts from, 1,623 bytes of PE32 i386.  It is
  # also in stage0-pe32's tree, and is fetched on its own so that the trust
  # anchor is pinned by its own hash, which is also the hash hex0 has to
  # reproduce.
  seed = fetchurl {
    name = "hex0-seed.exe";
    url = "https://raw.githubusercontent.com/Novavero-AI/stage0-pe32/${stage0-pe32Rev}/bootstrap-seeds/PE32/i386/hex0-seed.exe";
    sha256 = "b9db7be0fed7770be9fca46b7aa551b611fc60eaf1112a053531aae14aa46742";
  };

  stage0-pe32 = fromGitHub {
    owner = "Novavero-AI";
    repo = "stage0-pe32";
    rev = stage0-pe32Rev;
    sha256 = "d4df5a63ae7ec695f41e80a7cdf4138ed427cab58682f96d84ab8830b873a988";
  };

  M2-Planet = fromGitHub {
    owner = "Novavero-AI";
    repo = "M2-Planet";
    rev = "ee0bf91eb02390c2d40bd85379113e524f0c752b";
    sha256 = "1c68e8fe603e28fb40aa0c9accf3e4d690d1b9a35f154ae2d83f9278a060b1d3";
  };

  M2libc = fromGitHub {
    owner = "Novavero-AI";
    repo = "M2libc";
    rev = "f0b1726e25967098940327f4ddc47987c671d0c0";
    sha256 = "8bd0cc84b413a019806545d977fac94bc958a4f7ac4f0b9f4c2d058a80893ddc";
  };

  mescc-tools = fromGitHub {
    owner = "Novavero-AI";
    repo = "mescc-tools";
    rev = "578ac99623d291e2b9eacf03cacc390cfe512661";
    sha256 = "e7539f945f280b615e53183d538c25fdd99f4dc492a7e513ae3d141fe7626a1c";
  };
}
