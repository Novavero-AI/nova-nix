/*
 * busybox-w32's win32/mingw.c includes ntsecapi.h and userenv.h, which
 * TinyCC lacks, for calls it loads at run time and declares itself
 * (SystemFunction036, GetUserProfileDirectoryA); it reads nothing else
 * from either.
 */
