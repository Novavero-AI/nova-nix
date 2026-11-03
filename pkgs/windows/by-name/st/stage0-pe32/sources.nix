# Everything stage0-pe32's chain and mescc-tools-extra read: the seed, and
# five repositories at the commits stage0-pe32 pins.  Each repository value is
# the root of its tree in the store, so a step names a file as
# "${M2libc}/x86/x86_defs.M1", the path the .cmd reaches it by.
#
# A GitHub archive leaves submodules out, so stage0-pe32's tree has M2-Planet/,
# M2libc/, mescc-tools/ and mescc-tools-extra/ as empty directories, and each
# is fetched at the commit stage0-pe32 records for it (`git submodule
# status`).  Moving the pin is the stage0-pe32 commit, those submodule commits,
# the sha256 of each archive and of the seed, the archive names the test
# suite's stage0-pe32 group expects, and the link hashes the Stage0 (Windows)
# CI job compares against, which stage0-pe32's own CI prints for the new
# commit.
{ fetchurl }:
let
  stage0-pe32Rev = "ff1c2ad86a91d3d7d086722ffed49dfe18dd50d0";

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
    sha256 = "7f6923711dab4644c3dfbc7eb49a94fd5b7738274cb03bd5b92245845652df80";
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
    rev = "13e57eb52bd30a6b0296f1cef1c54fc40065f07b";
    sha256 = "bde242167fe1ffc52a0322cac6e4f24a32a10403df34dad82f40adc0b478c6af";
  };

  mescc-tools = fromGitHub {
    owner = "Novavero-AI";
    repo = "mescc-tools";
    rev = "578ac99623d291e2b9eacf03cacc390cfe512661";
    sha256 = "e7539f945f280b615e53183d538c25fdd99f4dc492a7e513ae3d141fe7626a1c";
  };

  # Upstream's, unforked: stage0-pe32 vendors it with nothing on top.  Its own
  # M2libc submodule is not fetched; its programs compile against the fork
  # above, as mescc-tools' do.
  mescc-tools-extra = fromGitHub {
    owner = "oriansj";
    repo = "mescc-tools-extra";
    rev = "d1d814602d83226c5a5e597e13dc8013a005ce76";
    sha256 = "285b3c747361ce288f4b5750e1271f633d88e9a7aa722a06887d07c07f307598";
  };
}
