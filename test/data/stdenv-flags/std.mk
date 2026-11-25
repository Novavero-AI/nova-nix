# A recipe line with no shell syntax, which GNU make runs itself, finding
# the command on PATH (pkgs/windows/stdenv/default.nix says how).
std-make.exe: std.c
	gcc -o std-make.exe std.c
