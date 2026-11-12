# busybox-w32, the shell and tools the native rungs run configure under,
# compiled by tinycc-native in one kaem script with no shell: upstream's
# mingw32_defconfig build, from Kbuild's sources for that configuration
# with Makefile.flags' flags.  The headers upstream's build generates are
# written as it writes them.  Its host programs in C, kconfig's conf,
# applet_tables and usage, are compiled by tinycc-native and run; its shell
# scripts are C here, gen_build_files.c and usage_compressed.c, which runs
# usage; and what scripts/embedded_scripts, scripts/generate_BUFSIZ.sh and
# the Makefile's BB_VER.h rule write for this configuration is text here.
#
# Upstream compiles against MinGW-w64's headers and links its runtime.
# This compiles against TinyCC's own headers, as gnumake does, with compat/
# adding the MinGW-w64 declarations busybox-w32 reads and TinyCC's headers
# lack, each file naming the lines it follows, and links five mingw-w64-crt
# sources, as they are, for functions busybox-w32 takes from that runtime:
# getopt.c is under the ISC and 2-clause BSD licences, strtok_r.c under
# the 3-clause BSD licence, and the others are in the public domain.  What
# else busybox-w32 takes from that runtime, edits to its own sources
# supply, so no file of nova-nix's is linked into the GPL-2.0-only
# busybox.exe.
#
# The configuration differs from mingw32_defconfig in three settings, for
# what this toolchain lacks: FEATURE_COMPRESS_USAGE packs the usage text
# with bzip2 at build time; FEATURE_RESOURCES's version information,
# manifest and icon take a resource compiler; and GLOBBING has MinGW-w64's
# startup code expand wildcards in the arguments, which TinyCC's crt1.c
# does not, so busybox --help says noglob.  time_t is 32 bits, as TinyCC's
# headers define it for i386, since busybox-w32 takes 64-bit time from
# MinGW-w64 10 on alone (include/mingw.h:5-14).  busybox.exe needs the
# msvcrt.dll of Windows Vista or later, for _chsize_s, _set_fmode,
# getenv_s and strnlen, which MinGW-w64's runtime supplies on older ones
# (mingw-w64-crt/lib-common/msvcrt.def.in:1285-1826).
{
  fetchurl,
  stage0-pe32,
  tinycc-native,
}:
let
  inherit (stage0-pe32) kaem;

  pname = "busybox-w32";
  version = "FRP-6354-g5f09fb446";

  # The version busybox reports: the Makefile's VERSION, PATCHLEVEL and
  # SUBLEVEL, then the release name with FRP read as git, as the Makefile
  # spells it for a source tree carrying .frp_describe, as the release
  # archive does.
  bbVersion = "1.39.0.git-6354-g5f09fb446";

  tarball = fetchurl {
    url = "https://frippery.org/files/busybox/busybox-w32-${version}.tgz";
    hash = "sha256-lQ0sc+l55Uxlx3I2aEBtHW50D8R09BUWqC3a6AX78ks=";
  };

  # mingw-w64-crt's sources at the v14.0.0 release's commit, from the
  # GitHub mirror of its repository: what busybox-w32 links from
  # MinGW-w64's runtime and msvcrt.dll lacks.  msvcr80plus_ftruncate64.c
  # is the ftruncate64 MinGW-w64 links for a runtime whose _chsize_s is
  # always there, as msvcrt.dll's is from Windows Vista on.
  mingwRev = "9b3dd0125792fe94d16cacdc596dbd42fca1b369";
  mingwCrt = [
    {
      path = "misc/getopt.c";
      hash = "sha256-En/HHRhV2EsKpHYTfxndf7UsxuZz2S3IZ9Auky6Hegs=";
    }
    {
      path = "misc/mempcpy.c";
      hash = "sha256-bQxrHu8VU30Ws5wpUgzZjrF0/z5rUlRtu8c0Rv7vki8=";
    }
    {
      path = "misc/mingw_usleep.c";
      hash = "sha256-E0HwtDP63gKLufmRaTh2Z4LnXlgIaBOmkk8efOGXp+o=";
    }
    {
      path = "stdio/strtok_r.c";
      hash = "sha256-DkiMY6yUIt6FeFk2s3MzIYvucB61SUYJSLRqGwsp9z4=";
    }
    {
      path = "stdio/msvcr80plus_ftruncate64.c";
      hash = "sha256-mzlytqBjGOuONuubMW2y8pSg6wbOehAeqPhgpSzD7/M=";
    }
  ];
  fetchMingwCrt =
    { path, hash }:
    fetchurl {
      url = "https://raw.githubusercontent.com/mingw-w64/mingw-w64/${mingwRev}/mingw-w64-crt/${path}";
      inherit hash;
    };
  objectOfCrt = { path, ... }: "mingw-w64-crt-" + objectOf path;

  # TinyCC's import library format, with the calls busybox-w32 makes into
  # advapi32.dll, for which TinyCC ships no .def, and into kernel32.dll
  # that its kernel32.def predates.
  advapi32 = builtins.toFile "advapi32.def" ''
    LIBRARY advapi32.dll

    EXPORTS
    AccessCheck
    CheckTokenMembership
    DuplicateToken
    EqualSid
    GetSecurityInfo
    GetTokenInformation
    GetUserNameA
    OpenProcessToken
    SaferComputeTokenFromLevel
    SaferCreateLevel
    SetTokenInformation
  '';
  kernel32 = builtins.toFile "kernel32.def" ''
    LIBRARY kernel32.dll

    EXPORTS
    FindFirstVolumeA
    FindNextVolumeA
    FindVolumeClose
    GetConsoleWindow
    GetProcessId
    GetVolumeNameForVolumeMountPointA
    GetVolumePathNamesForVolumeNameA
    IsWow64Process
  '';

  # What the Makefile's filechk_BB_VER.h, scripts/embedded_scripts and
  # scripts/generate_BUFSIZ.sh write for this configuration: no scripted
  # applet is configured and the tree has no embed/ directory, and
  # FEATURE_USE_BSS_TAIL is off.
  bbVerHeader = builtins.toFile "BB_VER.h" ''
    #define BB_VER "${bbVersion}"
  '';
  embeddedScriptsHeader = builtins.toFile "embedded_scripts.h" "\n#define NUM_SCRIPTS 0\n\n";
  commonBufsizHeader = builtins.toFile "common_bufsiz.h" ''
    enum { COMMON_BUFSIZE = 1024 };
    extern char bb_common_bufsiz1[];
    #define setup_common_bufsiz() ((void)0)
  '';

  # Each file the edits below change, as they leave it: replace writes a
  # file unchanged when its match is not there, so an edit that stops
  # matching fails the build here.
  editedFiles = builtins.toFile "busybox-w32-edits.sha256" ''
    99923f9b5136c08783c43ae779be646d16622a417d009b6ce53fd6b5548ceebc  configs/mingw32_defconfig
    0bed84f5ec5143d1d3e07f15bd3618482816dae910b56fccc9818408abbafebe  scripts/kconfig/lkc.h
    d098c32854cc9483e9fbecc5e89b5eb170e8a956bdea4b94abb8d57c5a4208ac  libbb/dump.c
    490e66386b77139b814feff086ae7a2fe3d0eb53ec3356828c5a64f45d0d0710  libbb/lineedit.c
    40f35b269f0eae19bea4b6e3a633fc14bb38be72375b8a8e4a55c086ebd8788e  win32/dirname.c
    f435aebacf417af560d38b1b562ba0347341cd0da137237626c2d954e58cecf6  libbb/appletlib.c
    43edcd5ea5d1afa3fed0d5e79db8d1d107539344c26e0e0432c539bbc89e1c3a  win32/mingw.c
  '';

  # The objects Kbuild builds for this configuration, from each directory's
  # Kbuild as scripts/gen_build_files.sh writes it from Kbuild.src, in
  # Kbuild's directory order.  The .S files assemble to nothing on i386
  # without GCC.  Objects are named for their sources' paths, which share
  # base names across directories.
  sources = [
    "applets/applets.c"
    "archival/ar.c"
    "archival/bbunzip.c"
    "archival/bzip2.c"
    "archival/chksum_and_xwrite_tar_header.c"
    "archival/cpio.c"
    "archival/dpkg.c"
    "archival/dpkg_deb.c"
    "archival/gzip.c"
    "archival/lzop.c"
    "archival/rpm.c"
    "archival/tar.c"
    "archival/unzip.c"
    "archival/libarchive/common.c"
    "archival/libarchive/unpack_ar_archive.c"
    "archival/libarchive/filter_accept_list_reassign.c"
    "archival/libarchive/get_header_ar.c"
    "archival/libarchive/get_header_tar.c"
    "archival/libarchive/get_header_tar_gz.c"
    "archival/libarchive/get_header_tar_bz2.c"
    "archival/libarchive/get_header_tar_lzma.c"
    "archival/libarchive/get_header_tar_xz.c"
    "archival/libarchive/get_header_cpio.c"
    "archival/libarchive/lzo1x_1.c"
    "archival/libarchive/lzo1x_1o.c"
    "archival/libarchive/lzo1x_d.c"
    "archival/libarchive/open_transformer.c"
    "archival/libarchive/decompress_bunzip2.c"
    "archival/libarchive/decompress_unlzma.c"
    "archival/libarchive/decompress_unxz.c"
    "archival/libarchive/decompress_gunzip.c"
    "archival/libarchive/decompress_uncompress.c"
    "archival/libarchive/data_skip.c"
    "archival/libarchive/data_extract_all.c"
    "archival/libarchive/data_extract_to_stdout.c"
    "archival/libarchive/unsafe_prefix.c"
    "archival/libarchive/unsafe_symlink_target.c"
    "archival/libarchive/filter_accept_all.c"
    "archival/libarchive/filter_accept_list.c"
    "archival/libarchive/filter_accept_reject_list.c"
    "archival/libarchive/header_skip.c"
    "archival/libarchive/header_list.c"
    "archival/libarchive/header_verbose_list.c"
    "archival/libarchive/seek_by_read.c"
    "archival/libarchive/seek_by_jump.c"
    "archival/libarchive/data_align.c"
    "archival/libarchive/find_list_entry.c"
    "archival/libarchive/init_handle.c"
    "console-tools/clear.c"
    "console-tools/reset.c"
    "coreutils/basename.c"
    "coreutils/cat.c"
    "coreutils/chmod.c"
    "coreutils/cksum.c"
    "coreutils/comm.c"
    "coreutils/cp.c"
    "coreutils/cut.c"
    "coreutils/date.c"
    "coreutils/dd.c"
    "coreutils/df.c"
    "coreutils/dirname.c"
    "coreutils/dos2unix.c"
    "coreutils/du.c"
    "coreutils/echo.c"
    "coreutils/env.c"
    "coreutils/expand.c"
    "coreutils/expr.c"
    "coreutils/factor.c"
    "coreutils/false.c"
    "coreutils/fold.c"
    "coreutils/head.c"
    "coreutils/id.c"
    "coreutils/install.c"
    "coreutils/join.c"
    "coreutils/link.c"
    "coreutils/ln.c"
    "coreutils/logname.c"
    "coreutils/ls.c"
    "coreutils/md5_sha1_sum.c"
    "coreutils/mkdir.c"
    "coreutils/mktemp.c"
    "coreutils/mv.c"
    "coreutils/nl.c"
    "coreutils/nproc.c"
    "coreutils/od.c"
    "coreutils/paste.c"
    "coreutils/printenv.c"
    "coreutils/printf.c"
    "coreutils/pwd.c"
    "coreutils/readlink.c"
    "coreutils/realpath.c"
    "coreutils/rm.c"
    "coreutils/rmdir.c"
    "coreutils/seq.c"
    "coreutils/shred.c"
    "coreutils/shuf.c"
    "coreutils/sleep.c"
    "coreutils/sort.c"
    "coreutils/split.c"
    "coreutils/stat.c"
    "coreutils/stty.c"
    "coreutils/sum.c"
    "coreutils/sync.c"
    "coreutils/tac.c"
    "coreutils/tail.c"
    "coreutils/tee.c"
    "coreutils/test.c"
    "coreutils/test_ptr_hack.c"
    "coreutils/timeout.c"
    "coreutils/touch.c"
    "coreutils/tr.c"
    "coreutils/true.c"
    "coreutils/truncate.c"
    "coreutils/tsort.c"
    "coreutils/uname.c"
    "coreutils/uniq.c"
    "coreutils/unlink.c"
    "coreutils/usleep.c"
    "coreutils/uudecode.c"
    "coreutils/uuencode.c"
    "coreutils/wc.c"
    "coreutils/whoami.c"
    "coreutils/yes.c"
    "coreutils/libcoreutils/cp_mv_stat.c"
    "debianutils/pipe_progress.c"
    "debianutils/which.c"
    "e2fsprogs/chattr.c"
    "e2fsprogs/e2fs_lib.c"
    "e2fsprogs/lsattr.c"
    "editors/awk.c"
    "editors/cmp.c"
    "editors/diff.c"
    "editors/ed.c"
    "editors/patch.c"
    "editors/sed.c"
    "editors/vi.c"
    "findutils/find.c"
    "findutils/grep.c"
    "findutils/xargs.c"
    "libbb/auto_string.c"
    "libbb/bb_cat.c"
    "libbb/bb_get_servport_by_name.c"
    "libbb/bb_getgroups.c"
    "libbb/bb_getsockname.c"
    "libbb/bitops.c"
    "libbb/c_escape.c"
    "libbb/common_bufsiz.c"
    "libbb/duration.c"
    "libbb/endofname.c"
    "libbb/get_shell_name.c"
    "libbb/getopt32.c"
    "libbb/getopt_allopts.c"
    "libbb/hash_hmac.c"
    "libbb/hash_sha256_block.c"
    "libbb/isqrt.c"
    "libbb/iterate_on_dir.c"
    "libbb/missing_syscalls.c"
    "libbb/nuke_str.c"
    "libbb/parse_config.c"
    "libbb/percent_decode.c"
    "libbb/print_numbered_lines.c"
    "libbb/replace.c"
    "libbb/ubi.c"
    "libbb/appletlib.c"
    "libbb/ask_confirmation.c"
    "libbb/bb_bswap_64.c"
    "libbb/bb_do_delay.c"
    "libbb/bb_pwd.c"
    "libbb/bb_qsort.c"
    "libbb/bb_strtonum.c"
    "libbb/chomp.c"
    "libbb/compare_string_array.c"
    "libbb/concat_path_file.c"
    "libbb/concat_subpath_file.c"
    "libbb/const_hack.c"
    "libbb/copy_file.c"
    "libbb/copyfd.c"
    "libbb/crc32.c"
    "libbb/default_error_retval.c"
    "libbb/dump.c"
    "libbb/executable.c"
    "libbb/fclose_nonstdin.c"
    "libbb/fflush_stdout_and_exit.c"
    "libbb/fgets_str.c"
    "libbb/find_pid_by_name.c"
    "libbb/full_write.c"
    "libbb/get_last_path_component.c"
    "libbb/get_line_from_file.c"
    "libbb/herror_msg.c"
    "libbb/human_readable.c"
    "libbb/isdirectory.c"
    "libbb/last_char_is.c"
    "libbb/lineedit.c"
    "libbb/lineedit_ptr_hack.c"
    "libbb/llist.c"
    "libbb/make_directory.c"
    "libbb/hash_md5_sha.c"
    "libbb/hash_sha1_x86-64.S"
    "libbb/hash_sha1_hwaccel_x86-64.S"
    "libbb/hash_sha1_hwaccel_x86-32.S"
    "libbb/hash_sha256_hwaccel_x86-64.S"
    "libbb/hash_sha256_hwaccel_x86-32.S"
    "libbb/messages.c"
    "libbb/mode_string.c"
    "libbb/parse_mode.c"
    "libbb/perror_msg.c"
    "libbb/perror_nomsg_and_die.c"
    "libbb/platform.c"
    "libbb/popcnt.c"
    "libbb/printable.c"
    "libbb/printable_string.c"
    "libbb/process_escape_sequence.c"
    "libbb/procps.c"
    "libbb/progress.c"
    "libbb/ptr_to_globals.c"
    "libbb/read.c"
    "libbb/read_key.c"
    "libbb/read_printf.c"
    "libbb/recursive_action.c"
    "libbb/remove_file.c"
    "libbb/run_shell.c"
    "libbb/safe_gethostname.c"
    "libbb/safe_poll.c"
    "libbb/safe_strncpy.c"
    "libbb/safe_write.c"
    "libbb/securetty.c"
    "libbb/simplify_path.c"
    "libbb/single_argv.c"
    "libbb/skip_whitespace.c"
    "libbb/str_tolower.c"
    "libbb/strrstr.c"
    "libbb/sysconf.c"
    "libbb/time.c"
    "libbb/trim.c"
    "libbb/u_signal_names.c"
    "libbb/uuencode.c"
    "libbb/verror_msg.c"
    "libbb/vfork_daemon_rexec.c"
    "libbb/warn_ignoring_args.c"
    "libbb/wfopen.c"
    "libbb/wfopen_input.c"
    "libbb/xatonum.c"
    "libbb/xconnect.c"
    "libbb/xfuncs.c"
    "libbb/xfuncs_printf.c"
    "libbb/xfunc_die.c"
    "libbb/xgetcwd.c"
    "libbb/xreadlink.c"
    "libbb/xrealloc_vector.c"
    "libbb/inode_hash.c"
    "libbb/find_mount_point.c"
    "libbb/inet_cksum.c"
    "libbb/xregcomp.c"
    "libpwdgrp/uidgid_get.c"
    "loginutils/suw32.c"
    "miscutils/ascii.c"
    "miscutils/bc.c"
    "miscutils/crond.c"
    "miscutils/crontab.c"
    "miscutils/drop.c"
    "miscutils/iconv.c"
    "miscutils/inotifyd.c"
    "miscutils/jn.c"
    "miscutils/less.c"
    "miscutils/make.c"
    "miscutils/man.c"
    "miscutils/strings.c"
    "miscutils/time.c"
    "miscutils/ts.c"
    "miscutils/ttysize.c"
    "networking/ftpgetput.c"
    "networking/httpd.c"
    "networking/ipcalc.c"
    "networking/nc.c"
    "networking/parse_pasv_epsv.c"
    "networking/ping.c"
    "networking/ssl_client.c"
    "networking/tls.c"
    "networking/tls_pstm.c"
    "networking/tls_pstm_montgomery_reduce.c"
    "networking/tls_pstm_mul_comba.c"
    "networking/tls_pstm_sqr_comba.c"
    "networking/tls_aes.c"
    "networking/tls_aesgcm.c"
    "networking/tls_rsa.c"
    "networking/tls_fe.c"
    "networking/tls_sp_c32.c"
    "networking/wget.c"
    "networking/whois.c"
    "procps/free.c"
    "procps/kill.c"
    "procps/pgrep.c"
    "procps/pidof.c"
    "procps/ps.c"
    "procps/uptime.c"
    "procps/watch.c"
    "shell/ash.c"
    "shell/ash_ptr_hack.c"
    "shell/shell_common.c"
    "shell/random.c"
    "shell/math.c"
    "util-linux/cal.c"
    "util-linux/flock.c"
    "util-linux/getopt.c"
    "util-linux/hexdump.c"
    "util-linux/hexdump_xxd.c"
    "util-linux/rev.c"
    "util-linux/uuidgen.c"
    "win32/actype.c"
    "win32/dirent.c"
    "win32/dirname.c"
    "win32/env.c"
    "win32/flock.c"
    "win32/fnmatch.c"
    "win32/fnmatch2.c"
    "win32/fsync.c"
    "win32/glob.c"
    "win32/inet_pton.c"
    "win32/ioctl.c"
    "win32/mingw.c"
    "win32/process.c"
    "win32/mntent.c"
    "win32/net.c"
    "win32/poll.c"
    "win32/popen.c"
    "win32/regex.c"
    "win32/select.c"
    "win32/statfs.c"
    "win32/strndup.c"
    "win32/strptime.c"
    "win32/strverscmp.c"
    "win32/system.c"
    "win32/termios.c"
    "win32/timegm.c"
    "win32/uname.c"
    "win32/winansi.c"
  ];
  objectOf = source: builtins.replaceStrings [ "/" ".c" ".S" ] [ "-" ".o" ".o" ] source;

  cc = "${tinycc-native.compiler}/bin/tcc.exe";
  libs = "${tinycc-native.libs}/lib";
  compat = ./compat;

  # The programs the build runs, compiled with the -Wall of the Makefile's
  # HOSTCFLAGS and linked from the -B directory.
  hostFlags = "-B ${libs} -Wall -I ${compat}";

  # Makefile.flags' CPPFLAGS and the CFLAGS TinyCC reads, with the
  # compatibility headers.  include/platform.h defines __attribute__ away
  # for a compiler that is not GCC 2.7 or later unless it is already a
  # macro; TinyCC honours the packed, aligned and noreturn attributes
  # busybox-w32 declares, so the macro keeps them.
  cflags = builtins.concatStringsSep " " [
    "-std=gnu99"
    "-Wall"
    "-funsigned-char"
    "-I include"
    "-I libbb"
    "-include include/autoconf.h"
    "-D_GNU_SOURCE"
    "-DNDEBUG"
    "-D_LARGEFILE_SOURCE"
    "-D_LARGEFILE64_SOURCE"
    "-D_FILE_OFFSET_BITS=64"
    "-D_TIME_BITS=64"
    "-DMINGW_VER=\\\"\\\""
    "-I win32"
    "-DHAVE_STRING_H=1"
    "-DHAVE_CONFIG_H=0"
    "-I ${compat}"
    "-D__attribute__=__attribute__"
  ];

  # The stack upstream's release reserves, GNU ld's default for a PE
  # executable; tcc's is half that.
  stackReserve = "2097152";
in
kaem.runCommand "${pname}-${version}"
  (
    tinycc-native.env
    // {
      # KCONFIG_NOTIMESTAMP keeps the build time, which busybox prints in
      # its banner, out of autoconf.h; KERNELVERSION is the version the
      # Makefile exports for conf to write there.
      KCONFIG_NOTIMESTAMP = "1";
      KERNELVERSION = bbVersion;
    }
  )
  ''
    ungz --file ${tarball} --output busybox.tar
    untar --file busybox.tar
    cd busybox-w32

    # The three settings this toolchain cannot honour.
    replace --file configs/mingw32_defconfig --output configs/mingw32_defconfig --match-on "CONFIG_FEATURE_COMPRESS_USAGE=y" --replace-with "# CONFIG_FEATURE_COMPRESS_USAGE is not set"
    replace --file configs/mingw32_defconfig --output configs/mingw32_defconfig --match-on "CONFIG_FEATURE_RESOURCES=y" --replace-with "# CONFIG_FEATURE_RESOURCES is not set"
    replace --file configs/mingw32_defconfig --output configs/mingw32_defconfig --match-on "CONFIG_GLOBBING=y" --replace-with "# CONFIG_GLOBBING is not set"

    # GCC takes a string literal in parentheses as an array's initializer,
    # as conf.c's N_ writes one; TinyCC does not.
    replace --file scripts/kconfig/lkc.h --output scripts/kconfig/lkc.h --match-on "#define N_(text) (text)" --replace-with "#define N_(text) text"

    # TinyCC's assembler knows bswapl as bswap.
    replace --file libbb/dump.c --output libbb/dump.c --match-on "bswapl %%eax" --replace-with "bswap %%eax"

    # A call in a block no configuration without UNICODE_SUPPORT reaches, to
    # a function such a configuration declares and leaves undefined: GCC
    # drops the block, and TinyCC compiles it, since it holds a loop.
    replace --file libbb/lineedit.c --output libbb/lineedit.c --match-on "adjust_width_and_validate_wc(&num, command_ps[cursor + n]);" --replace-with "IF_UNICODE_SUPPORT(adjust_width_and_validate_wc(&num, command_ps[cursor + n]));"

    # busybox-w32 carries a dirname from mingw-w64's runtime, which it
    # compiles in place of the runtime's own from MinGW-w64 12 on;
    # msvcrt.dll has none.
    replace --file win32/dirname.c --output win32/dirname.c --match-on "#if defined(__MINGW64_VERSION_MAJOR) && __MINGW64_VERSION_MAJOR > 11" --replace-with "#if 1"

    # MinGW-w64's startup code applies the binary _fmode win32/mingw.c
    # defines, and appletlib.c then makes stdin, stdout and stderr binary
    # under MinGW-w64 alone; TinyCC's crt1.c does neither.
    replace --file libbb/appletlib.c --output libbb/appletlib.c --match-on "#if defined(__MINGW64_VERSION_MAJOR)" --replace-with "#if 1\n_set_fmode(_O_BINARY);"

    # include/mingw.h declares ffs for a compiler other than GCC, whose
    # builtin it takes otherwise, and msvcrt.dll has none; win32/mingw.c
    # holds the C library functions busybox-w32 supplies itself.
    replace --file win32/mingw.c --output win32/mingw.c --match-on "smallint bb_got_signal;" --replace-with "#if !defined(__GNUC__)
    int ffs(int i)
    {
      unsigned int bits = i;
      int position = 1;

      if (bits == 0)
        return 0;
      while (!(bits & 1)) {
        bits >>= 1;
        position++;
      }
      return position;
    }
    #endif

    smallint bb_got_signal;"
    sha256sum --check ${editedFiles}

    cp scripts/kconfig/zconf.tab.c_shipped scripts/kconfig/zconf.tab.c
    cp scripts/kconfig/lex.zconf.c_shipped scripts/kconfig/lex.zconf.c
    cp scripts/kconfig/zconf.hash.c_shipped scripts/kconfig/zconf.hash.c

    ${cc} ${hostFlags} -o ''${NIX_BUILD_TOP}/gen_build_files.exe ${./gen_build_files.c}
    chmod 555 ''${NIX_BUILD_TOP}/gen_build_files.exe
    ''${NIX_BUILD_TOP}/gen_build_files.exe

    ${cc} ${hostFlags} -DKBUILD_NO_NLS -I scripts/kconfig -o ''${NIX_BUILD_TOP}/conf.exe scripts/kconfig/conf.c scripts/kconfig/zconf.tab.c
    chmod 555 ''${NIX_BUILD_TOP}/conf.exe
    ''${NIX_BUILD_TOP}/conf.exe -D configs/mingw32_defconfig Config.in

    cp ${bbVerHeader} include/BB_VER.h
    cp ${embeddedScriptsHeader} include/embedded_scripts.h
    cp ${commonBufsizHeader} include/common_bufsiz.h

    ${cc} ${hostFlags} -I include -o ''${NIX_BUILD_TOP}/applet_tables.exe applets/applet_tables.c
    chmod 555 ''${NIX_BUILD_TOP}/applet_tables.exe
    ''${NIX_BUILD_TOP}/applet_tables.exe include/applet_tables.h include/NUM_APPLETS.h

    ${cc} ${hostFlags} -I . -I include -o ''${NIX_BUILD_TOP}/usage_compressed.exe ${./usage_compressed.c}
    chmod 555 ''${NIX_BUILD_TOP}/usage_compressed.exe
    ''${NIX_BUILD_TOP}/usage_compressed.exe ''${NIX_BUILD_TOP}/usage.txt include/usage_compressed.h

    ${builtins.concatStringsSep "\n" (
      map (source: "${cc} ${cflags} -c -o ${objectOf source} ${source}") sources
    )}
    ${builtins.concatStringsSep "\n" (
      map (
        crt: "${cc} -Wall -I ${compat} -c -o ${objectOfCrt crt} ${fetchMingwCrt crt}"
      ) mingwCrt
    )}

    mkdir -p ''${out}/bin
    ${cc} -B ${libs} -Wl,--stack=${stackReserve} -o ''${out}/bin/busybox.exe ${
      builtins.concatStringsSep " " (map objectOf sources ++ map objectOfCrt mingwCrt)
    } -lws2_32 -luser32 ${advapi32} ${kernel32}
    chmod 555 ''${out}/bin/busybox.exe
  ''
// {
  srcs = [ tarball ] ++ map fetchMingwCrt mingwCrt;
}
